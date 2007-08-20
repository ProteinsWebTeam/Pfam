
# Builder.pm
# rdf 20070815 WTSI
#
# $Id: Builder.pm,v 1.1 2007-08-20 08:57:10 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Alignment::Builder - generate sequence alignments

=cut

package PfamWeb::Controller::Family::Alignment::Builder;

=head1 DESCRIPTION

This controller is responsible for building sequence alignments based on a list
of sequence entry accessions.

$Id: Builder.pm,v 1.1 2007-08-20 08:57:10 jt6 Exp $

=cut

use strict;
use warnings;

use URI::Escape;
use JSON;
use Data::UUID;
use Storable qw( thaw );

use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Family::Alignment';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 build : Path

Builds a sequence alignment from the specified sequences.

=cut

sub build : Path {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Family::Alignment::Builder::build: checking for sequences' );

  return unless( $c->req->param('seqAccs') );

  # retrieve the sequences
  $c->forward( 'getSequences' );
  
  # make sure we got something...
  unless( length $c->stash->{fasta} ) {
    # TODO how (and where) the hell do we flag an error ?
    $c->log->debug( 'Family::Alignment::Builder::build: failed to get a FASTA sequence' );
    return;
  }

  # submit the job to actually build the alignment
  my $submissionStatus = $c->forward( 'queueAlignment' );

  # and see if we managed it...
  if( $submissionStatus < 0 ) {
    $c->log->debug( 'Family::Alignment::Builder::build: problem with submission; returning error page' ); 
    $c->stash->{template} = 'components/tools/seqViewAlignmentPolling.tt';
  } else {
    $c->log->debug( 'Family::Alignment::Builder::build: alignment job submitted; polling' ); 
    $c->stash->{template} = 'components/tools/seqViewAlignmentPolling.tt';
  }
}

#-------------------------------------------------------------------------------

=head2 results : Local

Retrieves the sequence alignment that we generated.

=cut

sub results : Local {
  my( $this, $c ) = @_;

  # the template that will format the results  
  $c->stash->{template} = 'components/tools/seqViewAlignment.tt';

  # retrieve the job results
  $c->forward( 'JobManager', 'retrieveResults' );
  unless( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::Alignment::Builder::results: no results found' );
    $c->stash->{errorMsg} = 'No results found.';
    return;
  }   


  foreach my $jobId ( keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::Alignment::Builder:results: job results: |'
                   . $c->stash->{results}->{$jobId}->{rawData} . '|' );
  }
  
#  $c->forward( 'handleResults' );

}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getSequences : Private

Retrieves the sequences for the specified sequence accessions and drops them
into the stash as a single FASTA-format string.

=cut

sub getSequences : Private {
  my( $this, $c ) = @_;
  
  # detaint the list of sequence accessions. These accessions are passed as
  # a single value, space-separated and uri_encoded
  my @seqAccs;
  foreach ( split /\s+/, uri_unescape( $c->req->param('seqAccs') ) ) {
    next unless  m/^([AOPQ]\d[A-Z0-9]{3}\d)$/i;
    push @seqAccs, $1;
  }
  $c->log->debug( 'Family::Alignment::Builder::getSequences: found |' 
                  . scalar @seqAccs . '| valid sequence accessions' );
  $c->stash->{selectedSeqAccs} = \@seqAccs;

  # get each of the sequences in turn and turn them into FASTA format
  foreach my $seqAcc ( @seqAccs ) {
    my @rows = $c->model('PfamDB::PfamA_reg_full_significant')
                 ->search( { 'pfamseq.pfamseq_acc' => $seqAcc,
                             auto_pfamA            => $c->stash->{pfam}->auto_pfamA,
                             in_full               => 1 },
                           { join                  => [ qw( pfamseq ) ],
                             prefetch              => [ qw( pfamseq ) ] } );
    $c->stash->{numRows} = scalar @rows;

    foreach my $r (@rows){
      $c->stash->{fasta} .= '>'.$r->pfamseq_acc.'/'.$r->seq_start.'-'.$r->seq_end."\n";
      $c->stash->{fasta} .= substr( $r->sequence,
                                    $r->seq_start - 1,
                                    $r->seq_end - $r->seq_start + 1 )."\n";
    }
  }

#  $c->log->debug( 'Family::Alignment::Builder::generateAlignment: built a FASTA file: |'
#                  . $c->stash->{fasta} . '|' );
}

#-------------------------------------------------------------------------------

=head2 queueAlignment : Private

Queues the job that will actually generate the sequence alignment.

=cut

sub queueAlignment : Private {
  my($this, $c) = @_; 

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # set the options
  my $opts = '-acc ' . $c->stash->{acc} . '.' . $c->stash->{pfam}->version; 

  # guesstimate the time it will take to build the alignment
  my $estimatedTime = int( 1 + ( $c->stash->{numRows} / 100 ) ); 

  # add this job to the tracking tables
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
  # to the javascript on the client side. Because the queuing system allows
  # multiple Jobs in one page, the jobStatus JSON string needs to be an array
  # of hashes, each of which gives details of a separate job
  my $jobStatus = [
                    {
                      checkURI      => $c->uri_for( '/jobmanager/checkStatus' )
                                         ->as_string,
                      doneURI       => $c->uri_for( '/family/alignment/builder/results',
                                                    undef,
                                                    { acc => $c->stash->{acc} } )
                                         ->as_string,
                      estimatedTime => $estimatedTime,
                      interval      => $this->{pollingInterval},
                      jobId         => $jobId,
                      name          => 'Alignment',
                      jobClass      => 'Alignment',
                      opened        => $historyRow->opened,
                    }
                  ];
  $c->stash->{jobStatus} = objToJson( $jobStatus );

  $c->log->debug( 'Family::Alignment::Builder::queueAlignment: built a job status string: ',
                  dump( $jobStatus ) );
  $c->log->debug( 'Family::Alignment::Builder::queueAlignment: submitted job '
                  . "|$jobId| at |" . $historyRow->opened . '|' );
                  
  return 0;
} 

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
