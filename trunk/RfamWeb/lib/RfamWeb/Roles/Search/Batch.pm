
# Batch.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Search::Batch - role containing actions related to batch
sequence searching

=cut

package RfamWeb::Roles::Search::Batch;

=head1 DESCRIPTION

A role to add batch sequence search-related methods to the main search
controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use Email::Valid;
use DateTime;

with 'PfamBase::Roles::Search::Batch';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 batch : Chained('search') PathPart('batch') Args(0)

Executes a batch sequence search. 

=cut

sub batch : Chained( 'search' )
            PathPart( 'batch' )
            Args(0) {
  my ( $this, $c ) = @_;

  # validate the input
  unless ( $c->forward('validate_batch_sequences') ) {
    $c->stash->{batchSearchError} = 
      $c->stash->{searchError} ||
      'There was an unknown problem when validating your search sequences.';

    # go back to the search form
    $c->forward( 'search_page' );

    return;
  }

  #---------------------------------------

  # before we actually run the search, check we didn't do it recently 
  unless ( $c->forward( 'check_unique' ) ) {
    $c->stash->{batchSearchError } = 
      $c->stash->{searchError} ||
      'You submitted exactly this search recently. Please try not to submit duplicate searches.';

    $c->forward( 'search_page' );

    return;
  }
  
  #---------------------------------------

  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  
  # set the queue
  $c->stash->{job_type} = 'rfam_batch';

  # and submit the job...
  unless ( $c->forward( 'queue_search_transaction' ) ) {
    $c->stash->{batchSearchError } = 
      $c->stash->{searchError} ||
      'There was a problem queueing your search.';

    $c->forward( 'search_page' );

    return;
  }

  #----------------------------------------

  # if we get to here then the job was submitted successfully. Before handing
  # off to the template, set a refresh URI that will be picked up by head.tt 
  # and used in a meta refresh element
  $c->stash->{refreshUri}   = $c->uri_for( '/search' );
  $c->stash->{refreshDelay} = 300;
  
  $c->log->debug( 'Search::batch: protein batch search submitted' )
    if $c->debug; 
  $c->stash->{template} = 'pages/search/sequence/batchSubmitted.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validate_batch_sequences : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "searchError". 

=cut

sub validate_batch_sequences : Private {
  my( $this, $c ) = @_;

  # do the quick checks first...
  
  # email address
  if ( Email::Valid->address( -address => $c->req->param('email') ) ) {
    $c->stash->{email} = $c->req->param('email');
  }
  else {
    $c->stash->{searchError} = 'You did not enter a valid email address.';

    $c->log->debug( 'Search::validate_batch_sequences: bad email address; returning to form' )
      if $c->debug;
    
    return 0;
  }  

  # finally, the most time-consuming tests: check the sequence upload itself
  unless ( $c->forward( 'parse_upload' ) ) {

    $c->stash->{searchError} = 
         $c->stash->{searchError}
      || 'No valid sequence file found. Please enter a valid FASTA-format file and try again.';

    $c->log->debug( 'Search::validate_batch_sequences: bad FASTA file; returning to form' )
      if $c->debug;
      
    return 0;
  }

  # passed !
  $c->log->debug( 'Search::validate_batch_sequences: input parameters all validated' )
    if $c->debug;
    
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_rfam : Private

Submits an Rfam batch search.

=cut

sub queue_rfam : Private {
  my ( $this, $c ) = @_;
  
  # no user-configurable options for these jobs. Yet.

  # generate a job ID
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  
  # set the queue
  $c->stash->{job_type} = 'rfam';

  # make a guess at the runtime for the job
  $c->stash->{estimated_time} = int( $this->{search_multiplier} * length( $c->stash->{input} ) / 100 ); 

  # make sure we're not going to claim to have this search done without
  # at least one polling interval going by
  if ( $c->stash->{estimated_time} < $this->{pollingInterval} ) {
    $c->log->debug( 'Search::queue_rfam: resetting estimated search time to polling interval' )
      if $c->debug;
    $c->stash->{estimated_time} = $this->{pollingInterval};
  }

  $c->log->debug( 'Search::queue_rfam: estimated search time: |'
                  . $c->stash->{estimated_time} . '| seconds' ) if $c->debug;
  

  unless ( $c->forward('queue_search_transaction') ) {
    $c->log->debug( 'Search::queue_rfam: submission failed' )
      if $c->debug;
    
    $c->stash->{rest}->{error} = 'There was an error when registering your Rfam search.';
    
    return 0;
  }

  $c->log->debug( 'Search::queue_rfam: successfully queued job' )
    if $c->debug;
  
  #----------------------------------------
  
  # build a job status data structure that we'll convert to JSON and hand back
  # to the client in the RESTful response
  $c->stash->{jobStatus} = { estimatedTime => $c->stash->{estimated_time},
                             resultURL     => $c->uri_for( '/search/sequence', $c->stash->{jobId} )->as_string,
                             jobId         => $c->stash->{jobId},
                             opened        => $c->stash->{history_row}->opened };
    # {
    #   checkURI      => $c->uri_for( '/jobmanager/checkStatus' )->as_string,
    #   doneURI       => $c->uri_for( '/search/sequence/results' )->as_string,
    #   estimatedTime => $c->stash->{estimated_time},
    #   interval      => $this->{pollingInterval},
    #   jobId         => $c->stash->{jobId},
    #   name          => 'Rfam search',
    #   jobClass      => 'rfamSearch',
    #   opened        => $c->stash->{history_row}->opened
    # };
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_search_transaction : Private

Queues a search job. This requires new rows to be added to both the job_history
and the job_stream tables. We add these in a transaction block, rolling back if
either of the two fails. Adds the DBIC ResultSet from the job_history table to
the stash and returns 1 if the submission is successful, returns 0 otherwise.

=cut

sub queue_search_transaction : Private {
  my ( $this, $c ) = @_;
  
  # set up an anonymous code block to define a transaction. We want to make sure
  # that we can add a row to both tables before we know that this job has been 
  # successfully queued

  # somewhere to stuff the rows from the job_history and job_stream tables, 
  # if we get them
  my ( $job_history, $job_stream );

  my $transaction = sub {
    $c->log->debug( 'Search::queue_search_transaction: starting transaction...' )
      if $c->debug;

    # add this job to the tracking table
    $job_history = $c->model('WebUser::JobHistory')
                     ->create( { options        => $c->stash->{options},
                                 job_type       => $c->stash->{job_type},
                                 job_id         => $c->stash->{jobId},
                                 estimated_time => $c->stash->{estimated_time},
                                 opened         => DateTime->now, # \'NOW()',
                                 status         => 'PEND',
                                 email          => $c->stash->{email} } );  
    
    die 'error: failed to add job_history row' unless defined $job_history;
    
    $c->log->debug( 'Search::queue_search_transaction: added row to job_history' )
      if $c->debug;
    
    # and to the input/output table
    $job_stream = $c->model( 'WebUser::JobStream' )
                    ->create( { id    => $job_history->id,
                                stdin => $c->stash->{input} || q() } );
    
    die 'error: failed to add job_stream row' unless defined $job_stream;
    
    $c->log->debug( 'Search::queue_search_transaction: added row to job_stream' )
      if $c->debug;
    
    # check the submission time with a separate query. We need to do this so
    # that we get the "opened" time that is inserted by the database engine. The
    # job_history object that we have doesn't contain that at this point
    my $history_row = $c->model( 'WebUser::JobHistory' )
                        ->find( { id => $job_history->id } );
    
    die "error: couldn't retrieve job history row" unless defined $history_row;
    
    $c->log->debug( 'Search::queue_search_transaction: job opened: |'
                    . $history_row->opened . '|' ) if $c->debug;
    
    return $history_row; # return from anonymous transaction sub 
  };
  
  # execute the transaction
  my $history_row;
  eval {
    $history_row = $c->model('WebUser')->schema->txn_do( $transaction );
  };

  # there was a problem...
  if ( $@ ) {
    $c->log->error( "Search::queue_search_transaction: error in transaction: |$@|" )
      if $c->debug;

    # if the first query worked, we should have a row from the job_history 
    # table, which we can modify to set the job status to "FAIL"
    if ( defined $job_history ) {
      
      # set the status on the object...
      $job_history->status('FAIL');
      
      # .. and see if we can actually update that row in the DB
      if ( $job_history->update ) {
        $c->log->debug( 'Search::queue_search_transaction: successfully rolled back job_history' )
          if $c->debug;
      }
      else {
        $c->log->warn( 'Search::queue_search_transaction: failed to roll back job_history' )
          if $c->debug;
      }
    }

    return 0;
  }
 
  $c->stash->{history_row} = $history_row;

  return 1;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012 Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk)

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

