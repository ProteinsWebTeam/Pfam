
# Batch.pm
# jt6 20061108 WTSI
#
# $Id: Batch.pm,v 1.2 2007-07-31 12:52:47 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Batch - perform batch sequence searches

=cut

package PfamWeb::Controller::Search::Batch;

=head1 DESCRIPTION

This controller is responsible for running batch sequence searches.

$Id: Batch.pm,v 1.2 2007-07-31 12:52:47 jt6 Exp $

=cut

use strict;
use warnings;

use Scalar::Util qw( looks_like_number );
use Email::Valid;

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Executes a batch search. 

=cut

sub search : Path {
  my( $this, $c ) = @_;

  # validate the input
  $c->forward( 'validateInput' );
  return if $c->stash->{batchSearchError};

  # submit the search
  $c->forward( 'queueSearch' );
  return if $c->stash->{batchSearchError};

  # set a refresh URI that will be picked up by head.tt and used in a 
  # meta refresh element
  $c->stash->{refreshUri} = $c->uri_for( '/search' ); 
  
  $c->log->debug( 'Search::Batch::search: batch search submitted' ); 
  $c->stash->{template} = 'pages/search/sequence/batchSubmitted.tt';
}

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validateInput : Private

Validate the form input.

=cut

sub validateInput : Private {
  my( $this, $c ) = @_;
  
  # the sequence itself
  $c->forward( 'parseUpload' );
  unless( $c->stash->{seqs} ) {
    $c->stash->{batchSearchError} =
      'No valid sequence found. Please enter a valid FASTA-format file and try again.';

    $c->log->debug( 'Search::Batch::search: bad FASTA file; returning to form' );
    return;
  }

  # sequence search options
  if( defined $c->req->param( 'batchOpts' ) ) {
    $c->stash->{batchOpts} = $c->req->param( 'batchOpts' );
    unless( $c->stash->{batchOpts} eq 'both' or
            $c->stash->{batchOpts} eq 'bothNoMerge' or
            $c->stash->{batchOpts} eq 'ls' or
            $c->stash->{batchOpts} eq 'fs' ) {
      $c->stash->{batchSearchError} = 'You must select a valid search option.';

      $c->log->debug( 'Search::Batch::search: bad search option; returning to form' );
      return;
    }
  } else {
    $c->log->debug( 'Search::Batch::search: search options not specified; returning to form' );
    $c->stash->{batchSearchError} = 'You must select a search option.';
    return;
  }

  # if we have an evalue, we'll use that, otherwise we'll use the gathering
  # threshold
  if( defined $c->req->param( 'ga' ) and $c->req->param( 'ga' ) ) {
    $c->stash->{ga} = 1;
  } else {
    if( defined $c->req->param( 'evalue' ) and 
        looks_like_number( $c->req->param( 'evalue' ) ) ) {
      $c->stash->{evalue} = $c->req->param( 'evalue' );
    } else {
      $c->stash->{batchSearchError} = 'You did not enter a valid E-value.';

      $c->log->debug( 'Search::Batch::search: bad evalue; returning to form' );
      return;
    }
  }

  # email address
  if( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email');
  } else {
    $c->stash->{batchSearchError} = 'You did not enter a valid email address.';
    $c->log->debug( 'Search::Batch::search: bad email address; returning to form' );
    return;
  }  

  # passed !
  $c->log->debug( 'Search::Batch::search: input parameters all validated' );
}

#-------------------------------------------------------------------------------

=head2 parseUpload : Attribute

Parses the uploaded file.

=cut

sub parseUpload : Private {
  my( $this, $c ) = @_;

  # check that we can get an Catalyst::Request::Upload object from the request 
  my $u;
  return unless( $u = $c->req->upload('batchSeq') );
  
  # check that the Upload object returns us a filehandle
  my $fh;
  unless( $fh = $u->fh ) {
    $c->log->warn( 'Search::Batch::parseUpload: couldn\'t open uploaded file' );
    return;
  }

  # read through the file and bail if we find any illegal characters in it  
  my $seqs;
  while( <$fh> ) {
    unless( /^>[A-Za-z0-9\-_]+$/ or /^[A-Za-z]+$/ ) {
      $c->log->debug( 'Search::Batch::parseUpload: illegal character in upload; bailing' );
      return;
    }
    $seqs .= $_;
  }

  # the upload passed the illegal characters test; stash it as a string
  $c->stash->{seqs} = $seqs;
}

#-------------------------------------------------------------------------------

=head2 queueSearch : Private

Queues a batch search.

=cut

sub queueSearch : Private {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Batch::queueSearch: queueing a batch search' );

  # generate a job ID
  my $jobId = Data::UUID->new()->create_str();

  # build the command to run
  my $cmd;
  $cmd  =  q(pfam_scan.pl -pvm -align -d ) . $this->{blastDb};
  $cmd .=  q( --mode ) . $c->stash->{batchOpts} if( $c->stash->{batchOpts} ne 'both' and 
                                                    $c->stash->{batchOpts} ne 'bothNoMerge' );
  $cmd .=  q( --no_merge )                      if( $c->stash->{batchOpts} eq 'bothNoMerge' );
  $cmd .=  q( -e )     . $c->stash->{evalue}    if( $c->stash->{evalue} and not $c->stash->{ga} );
  $cmd .=  q( --overlap )                       if( $c->stash->{showOverlap} );
  $cmd .= qq( /tmp/$jobId.fa);

  $c->log->debug( "Search::Batch::queueSearch: using command: |$cmd|" );

  # add this job to the tracking tables
  my $jobHistory = $c->model('WebUser::JobHistory')
                     ->create( { command        => $cmd,
                                 priority       => 'batch',
                                 job_id         => $jobId,
                                 opened         => \'NOW()',
                                 status         => 'PEND',
                                 email          => $c->stash->{email} } );

  unless( defined $jobHistory ) { 
    $c->log->warn( q(Search::Batch::queueSearch: couldn't submit ) );
    $c->stash->{batchSearchError} = 'There was a problem submitting your job.';
    return;
  }

  # add the sequences file to the data table     
  my $jobStream = $c->model('WebUser::JobStream')
                    ->create( { id    => $jobHistory->id,
                                stdin => $c->stash->{seqs} || q() } );

  unless( defined $jobStream ) { 
    $c->stash->{batchSearchError} = 'There was a problem submitting your job.';
    return;
  }
  
  # check the submission time with a separate query
  my $historyRow = $c->model( 'WebUser::JobHistory' )
                     ->find( { id => $jobHistory->id } );

  # count the number of jobs in the queue before this one
  # see how many jobs are pending
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->search( { status => 'PEND',
                         id     => { '<',        $jobHistory->id },
                         job_id => { 'not like', $jobHistory->job_id } },
                       { select => [
                                     { count => 'id' },
                                   ],
                         as     => [ qw( num ) ] }
                     );
  $c->stash->{numPending} = $rs->first()->get_column( 'num' );

  # stash the job submission time
  $c->stash->{opened} = $historyRow->opened;
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
