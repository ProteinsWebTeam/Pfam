
# Builder.pm
# jt6 20070823 WTSI
#
# $Id: Builder.pm,v 1.9 2008-11-25 14:54:27 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Proteome::Alignment::Builder - generate sequence alignments

=cut

package PfamWeb::Controller::Proteome::Alignment::Builder;

=head1 DESCRIPTION

This controller is responsible for building sequence alignments based on a list
of sequence entry accessions.

$Id: Builder.pm,v 1.9 2008-11-25 14:54:27 jt6 Exp $

=cut

# TODO there's way too much overlap with Family::Alignment::Builder 

use strict;
use warnings;

use URI::Escape;
use JSON;
use Data::UUID;

use Data::Dump qw( dump );

use Bio::Pfam::ColourAlign;

use base 'PfamWeb::Controller::Proteome::Alignment';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 build : Path

Builds a sequence alignment from the specified sequences.

=cut

sub build : Path {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Proteome::Alignment::Builder::build: checking for sequences' )
    if $c->debug;

  # retrieve the sequences
  $c->forward( 'get_sequences' );
  
  # make sure we got something...
  unless ( length $c->stash->{fasta} ) {
    $c->log->debug( 'Proteome::Alignment::Builder::build: failed to get a FASTA sequence' )
      if $c->debug;
    $c->stash->{errorMsg} = 'We failed to get a FASTA format sequence file for your selected sequences.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # submit the job to actually build the alignment
  my $submissionStatus = $c->forward( 'queue_alignment' );

  # and see if we managed it...
  if( $submissionStatus < 0 ) {
    $c->log->debug( 'Proteome::Alignment::Builder::build: problem with submission; returning error page' )
      if $c->debug; 
    $c->stash->{errorMsg} = 'There was an error when submitting your sequences to be aligned.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
  } else {
    $c->log->debug( 'Proteome::Alignment::Builder::build: alignment job submitted; polling' )
      if $c->debug; 
    $c->stash->{template} = 'components/tools/seqViewAlignmentPolling.tt';
  }
}

#-------------------------------------------------------------------------------

=head2 view : Local

Retrieves the sequence alignment that we generated.

=cut

sub view : Local {
  my( $this, $c ) = @_;

  # retrieve the job results
  my( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/i;
  $c->forward( 'JobManager', 'retrieveResults', [ $jobId ] );
  
  unless( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Proteome::Alignment::Builder::view: no results found' )
      if $c->debug;
    $c->stash->{errorMsg} = 'No sequence alignment found.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }   

  # count the number of rows in the alignment. The raw alignment includes 
  # the consensus string as the last line
  my @rows = split /\n/, $c->stash->{results}->{$jobId}->{rawData};
  my $numRowsInAlignment = scalar @rows - 1;
  $c->log->debug( "Proteome::Alignment::Builder::view: alignment has |$numRowsInAlignment| rows" )
    if $c->debug;

  # configure the viewer...
  
  # a pretty title...
  my $title = 'Alignment for ' . $c->stash->{proteomeSpecies}->species 
              . ' sequences with Pfam domain ' . $c->stash->{pfam}->pfama_id
              . '(' . $c->stash->{pfam}->pfama_acc . ')';
  
  $c->stash->{params} = { source             => 'species',
                          pfamAcc            => $c->stash->{pfam}->pfama_acc,
                          taxId              => $c->stash->{taxId},
                          title              => $title,
                          jobId              => $jobId,
                          numRowsInAlignment => $numRowsInAlignment };

  # and hand off to it
  $c->forward( 'PfamViewer', 'showPfamViewer' );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_sequences : Private

Retrieves the sequences for the specified sequence accessions and drops them
into the stash as a single FASTA-format string.

=cut

# select s.auto_pfamA, seq_start, seq_end, c.ncbi_taxid from complete_proteomes c join proteome_regions r on c.auto_proteome = r.auto_proteome join pfamA_reg_full_significant s on r.auto_pfamseq = s.auto_pfamseq where c.ncbi_taxid=46125 and in_full=1 limit 10;

sub get_sequences : Private {
  my( $this, $c ) = @_;
  
  my @regions = $c->model('PfamDB::ProteomeRegions')
                  ->search( { 'auto_proteome.ncbi_taxid' => $c->stash->{taxId},
                              'me.auto_pfama'            => $c->stash->{pfam}->auto_pfama },
                            { join       => [ 'auto_proteome', { regions => 'pfamseq' } ],
                              prefetch   => [ 'auto_proteome', { regions => 'pfamseq' } ] } );

  $c->log->debug( 'Proteome::Alignment::Builder: found |' 
                  . scalar @regions . '| sequences for this taxId / family' )
    if $c->debug;

  $c->stash->{numRows} = scalar @regions; # used when estimating job runtime...

  foreach my $regions ( @regions ) {

    push @{ $c->stash->{selectedSeqAccs} }, $regions->regions->first->pfamseq_acc;
      
    # there could be multiple regions for a given combination of family and
    # sequence, so we need to loop over the list of regions from the 
    # ProteomeRegions table
    foreach my $region ( $regions->regions->all ) {
      $c->stash->{fasta} .= '>'.$region->pfamseq_acc.'/'.$region->seq_start.'-'.$region->seq_end."\n";
      $c->stash->{fasta} .= substr( $region->sequence,
                                    $region->seq_start - 1,
                                    $region->seq_end - $region->seq_start + 1 ) . "\n";
    }

  }

  $c->log->debug( 'Family::Alignment::Builder::get_sequences: built a FASTA file: |'
                  . $c->stash->{fasta} . '|' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 queue_alignment : Private

Queues the job that will actually generate the sequence alignment.

=cut

sub queue_alignment : Private {
  my ($this, $c) = @_; 

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # set the options
  my $opts = '-acc ' . $c->stash->{pfam}->pfama_acc . '.' . $c->stash->{pfam}->version; 

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
                      doneURI       => $c->uri_for( '/proteome/alignment/builder/view',
                                                    { pfamAcc => $c->stash->{pfam}->pfama_acc,
                                                      taxId   => $c->stash->{taxId} } )
                                         ->as_string,
                      estimatedTime => $estimatedTime,
                      interval      => $this->{pollingInterval},
                      jobId         => $jobId,
                      name          => 'Sequence alignment',
                      jobClass      => 'alignment',
                      opened        => $historyRow->opened,
                    }
                  ];
  $c->stash->{jobStatusJSON} = objToJson( $jobStatus );

  $c->log->debug( 'Proteome::Alignment::Builder::queueAlignment: built a job status string: ',
                  dump( $jobStatus ) ) if $c->debug;
  $c->log->debug( 'Proteome::Alignment::Builder::queueAlignment: submitted job '
                  . "|$jobId| at |" . $historyRow->opened . '|' ) if $c->debug;

  return 0;
} 

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
