
# Search.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

PfamBase::Roles::Search - role with low-level search-related actions

=cut

package PfamBase::Roles::Search;

=head1 DESCRIPTION

This role provides low-level methods that are needed for submitting sequence 
searches via the "web_user" database.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use DateTime;

with 'PfamBase::Roles::Section';

#-------------------------------------------------------------------------------

=head2 METHODS

=head2 begin : Private

Sets up the stash for all types of search.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'search';
  
  # tell the layout template to disable the summary icons
  $c->stash->{iconsDisabled} = 1;
  
  # decide what format to emit. The default is HTML, in which case
  # we don't set a template here, but just let the "end" method on
  # the Section controller take care of us
  $c->stash->{output_xml} = ( $c->req->param('output') || '' ) eq 'xml';

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
    $c->log->debug( 'Roles::Search::queue_search_transaction: starting transaction...' )
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
    
    $c->log->debug( 'Roles::Search::queue_search_transaction: added row to job_history' )
      if $c->debug;
    
    # and to the input/output table
    $job_stream = $c->model( 'WebUser::JobStream' )
                    ->create( { id    => $job_history->id,
                                stdin => $c->stash->{input} || q() } );
    
    die 'error: failed to add job_stream row' unless defined $job_stream;
    
    $c->log->debug( 'Roles::Search::queue_search_transaction: added row to job_stream' )
      if $c->debug;
    
    # check the submission time with a separate query. We need to do this so
    # that we get the "opened" time that is inserted by the database engine. The
    # job_history object that we have doesn't contain that at this point
    my $history_row = $c->model( 'WebUser::JobHistory' )
                        ->find( { id => $job_history->id } );
    
    die "error: couldn't retrieve job history row" unless defined $history_row;
    
    $c->log->debug( 'Roles::Search::queue_search_transaction: job opened: |'
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
        $c->log->debug( 'Roles::Search::queue_search_transaction: successfully rolled back job_history' )
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
