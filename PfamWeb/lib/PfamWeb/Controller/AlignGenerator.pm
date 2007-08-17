# AlignGenerator.pm
# rdf 20070815 WTSI
#
# $Id: AlignGenerator.pm,v 1.1 2007-08-17 09:04:38 rdf Exp $

=head1 NAME

PfamWeb::Controller::AlignGenerator - perform sequence searches

=cut

package PfamWeb::Controller::AlignGenerator;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: AlignGenerator.pm,v 1.1 2007-08-17 09:04:38 rdf Exp $

=cut

use strict;
use warnings;

use File::Temp qw( tempfile );
use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;
use Storable qw( thaw );

use Data::Dump qw( dump );

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 generatAlign : Path


=cut

sub generateAlign : Path {
  my($this, $c) = @_;
  
  if($c->req->param('subTree') and $c->req->param('seqAccs') and $c->res->param('acc') ) {
    #Detaint the pfamA accession and if we have one, get the data for the family  
    $c->log->debug( 'AlignGenerator::generateAlign: Checking accession' );    
    if( $c->req->param('acc') =~ m/^(PF\d{5})$/i ) {
      $c->stash->{pfam} = $c->model('PfamDB::Pfam')
                            ->find( { pfamA_acc => $1 } );
    }
    
    $c->log->debug( 'AlignGenerator::generateAlign: checking for sub-tree sequences' );    

    # detaint the list of sequence accessions (again... we've already done this
    # in SpeciesTree, but since the user can have put their sticky little hands
    # on them in between, we'll do it once more)
    my @seqAccs;
    
    # retrieve the list of accessions from the Request...
    my @taintedSeqAccs = $c->req->param('seqAccs'); 
           
    foreach ( @taintedSeqAccs ) {
      next unless m/^\s*([AOPQ]\d[A-Z0-9]{3}\d)\s*$/i;
      push @seqAccs, $1;
    }
    $c->log->debug( 'AlignGenerator::generateAlign found |' . scalar @seqAccs
                    . '| valid sequence accessions' );    

    # get each of the sequences in turn...
    my $fasta;
    foreach my $seqAcc ( @seqAccs ) {
     $c->log->debug("AlignGenerator::generateAlign: getting sequence for |$seqAcc|...");
      my @rows = $c->model('PfamDB::PfamA_ref_full_significant')
              ->search( { 'pfamseq.pfamseq_acc' => $seqAcc,
                          'in_full'             => 1 },
                      { join                  => [ qw( pfamseq ) ],
                        prefetch              => [ qw( pfamseq ) ] } );
      foreach my $r (@rows){
        $fasta .= ">".$r->pfamseq_acc."/".$r->seq_start."-".$r->seq_end."\n";
        $fasta .= substr($r->seq, $r->start - 1, $r->end - $r->start + 1); 
      }
    }
    $c->log->debug("AlignGenerator::generateAlign: Built fasta file |$fasta|");
    $c->stash->{fasta} = $fasta;
  }
  $c->forward('queueAlign');
}




sub queueAlign : Private {
  my($this, $c) = @_; 
 
  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();
  my ($opts, $estimatedTime);
  if($c->stash->{pfam}){
    $opts .= "-acc ".$c->stash->{pfam}->pfamA_acc.".".c->stash->{pfam}->version; 
  }
  # add this job to the tracking table
  my $jobHistory = $c->model('WebUser::JobHistory')
                     ->create( { options        => $opts,
                                 job_type       => 'align',
                                 estimated_time => $estimatedTime,
                                 job_id         => $jobId,
                                 opened         => \'NOW()',
                                 status         => 'PEND' } );

  my $jobStream = $c->model('WebUser::JobStream')
                    ->create( { id    => $jobHistory->id,
                                stdin => $c->stash->{fasta} || q() } );

  # check the submission time with a separate query
  my $historyRow = $c->model( 'WebUser::JobHistory' )
                     ->find( { id => $jobHistory->id } );


  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side
  my $jobStatus = {
                    checkURI      => $c->uri_for( '/aligngenerator/checkStatus' )->as_string,
                    doneURI       => $c->uri_for( '/aligngenerator/results' )->as_string,    
                    estimatedTime => $estimatedTime,
                    interval      => $this->{pollingInterval},
                    jobId         => $jobId,
                    name          => 'Alignment',
                    jobClass      => 'Alignment',
                    opened        => $historyRow->opened,
                  };
  
} 

sub results : Local {
  my( $this, $c ) = @_;

  # extract the list of IDs from the URI
  my @jobIds = $c->req->param( 'jobId' );
  foreach ( @jobIds ) {

    # detaint the job ID
    ( my $jobId ) = $_ =~ m/^([A-F0-9\-]+)$/;
    $c->log->debug( "Search::Sequence::results: looking up details for job ID: |$jobId|" );

    next unless defined $jobId;

    # job ID *looks* valid; try looking for that job
    my $job = $c->model( 'WebUser::JobHistory' )
                ->find( { job_id => $jobId },
                        { join     => [ qw( job_stream ) ],
                          prefetch => [ qw( job_stream ) ] } );

    # bail unless it exists
    next unless defined $job;

    # retrieve the results of the job and stash them
    $c->stash->{results}->{$jobId}->{rawData} = $job->stdout;
    $c->stash->{results}->{$jobId}->{method}  = $job->job_type;
    $c->stash->{results}->{$jobId}->{options} = $job->options;
    $c->{stash}->{seq} = $job->stdin;
  }

  # do something interesting with the results
  $c->forward( 'handleResults' );
  $c->forward( 'generateGraphic' );
  $c->stash->{template} = 'pages/search/sequence/results.tt';
}
1;

