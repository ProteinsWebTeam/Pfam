
# Builder.pm
# rdf 20070815 WTSI
#
# $Id: Builder.pm,v 1.7 2007-12-10 14:42:13 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Alignment::Builder - generate sequence alignments

=cut

package PfamWeb::Controller::Family::Alignment::Builder;

=head1 DESCRIPTION

This controller is responsible for building sequence alignments based on a list
of sequence entry accessions.

$Id: Builder.pm,v 1.7 2007-12-10 14:42:13 jt6 Exp $

=cut

# TODO there's way too much overlap with Proteome::Alignment::Builder 

use strict;
use warnings;

use URI::Escape;
use JSON;
use Data::UUID;

use Data::Dump qw( dump );

use Bio::Pfam::ColourAlign;

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
    $c->log->debug( 'Family::Alignment::Builder::build: failed to get a FASTA sequence' );
    $c->stash->{errorMsg} = 'We failed to get a FASTA format sequence file for your selected sequences.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }

  # submit the job to actually build the alignment
  my $submissionStatus = $c->forward( 'queueAlignment' );

  # and see if we managed it...
  if( $submissionStatus < 0 ) {
    $c->log->debug( 'Family::Alignment::Builder::build: problem with submission; returning error page' ); 
    $c->stash->{errorMsg} = 'There was an error when submitting your sequences to be aligned.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
  } else {
    $c->log->debug( 'Family::Alignment::Builder::build: alignment job submitted; polling' ); 
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
  my( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/;
  $c->forward( 'JobManager', 'retrieveResults', [ $jobId ] );
  
  unless( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::Alignment::Builder::view: no results found' );
    $c->stash->{errorMsg} = 'No sequence alignment found.';
    $c->stash->{template} = 'components/tools/seqViewAlignmentError.tt';
    return;
  }   

  # count the number of rows in the alignment. The raw alignment includes 
  # the consensus string as the last line
  my @rows = split /\n/, $c->stash->{results}->{$jobId}->{rawData};
  my $numRowsInAlignment = scalar @rows - 1;
  $c->log->debug( "Family::Alignment::Builder::view: alignment has |$numRowsInAlignment| rows" );

  # configure the viewer...
  $c->stash->{params} = { source             => 'species',
                          title              => 'Alignment for selected sequences',
                          jobId              => $jobId,
                          numRowsInAlignment => $numRowsInAlignment };

  # and hand off to it
  $c->forward( 'PfamViewer', 'showPfamViewer' );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getAlignment : Private

Builds an alignment of the selected sequences.

=cut

sub getAlignment : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Family::Alignment::Builder::getAlignment: retrieving alignment...' );

  # first get a job ID. The call to retrieve results will get the job ID for
  # itself, but we'll need it here anyway
  my( $jobId ) = $c->req->param('jobId') || '' =~ m/^([A-F0-9\-]{36})$/;

  unless( defined $jobId ) {
    $c->log->debug( 'Family::Alignment::Builder::getAlignment: no job ID found' );
    $c->stash->{errorMsg} = 'No job ID found for the sequence alignment job.';
    return;
  }   

  # retrieve the job results
  $c->forward( 'JobManager', 'retrieveResults', [ $jobId ] );
  unless( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Family::Alignment::Builder::getAlignment: no results found' );
    $c->stash->{errorMsg} = 'No sequence alignment found.';
    return;
  }   

#  $c->log->debug( 'Family::Alignment::Builder:getAlignment: job results: |'
#                  . $c->stash->{results}->{$jobId}->{rawData} . '|' );

  # the rawData is just a string containing the alignment lines
  my @alignmentRows = split /\n/, $c->stash->{results}->{$jobId}->{rawData};
  
  # the consensus string is the last row of the alignment
  my $consensusString = pop @alignmentRows;
  $consensusString =~ s/^ConSeq\s+(\S+)$/$1/;
  
  # take a slice of that array, based on the "rows" setting from PfamViewer.
  # Rows are numbered from 1, not zero, so we need to offset the row values
  my $from = $c->stash->{rows}->[0] - 1;
  my $to   = $c->stash->{rows}->[1] - 1;
  #$c->log->debug( 'Family::Alignment::Builder::getAlignment: showing rows |'
  #                . "$from| to |$to|" );
  
  my %alignment;
  my $length;
  foreach ( @alignmentRows[ $from .. $to ] ) {
    next unless m|(\S+/\d+\-\d+)\s+(\S+)|;
    $alignment{$1} = $2;
    $length++;
  }
  
  # parse the consensus string
  my $consensus = Bio::Pfam::ColourAlign::parseConsensus( $consensusString );
 
  # stash everything
  $c->stash->{alignments}->{rawAlignments} = [ \%alignment ];
  $c->stash->{alignments}->{lengths}       = [ $length ];
  $c->stash->{alignments}->{consensus}     = [ $consensus ];
}

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
                      doneURI       => $c->uri_for( '/family/alignment/builder/view',
                                                    { acc => $c->stash->{acc} } )
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

  $c->log->debug( 'Family::Alignment::Builder::queueAlignment: job status: ',
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
