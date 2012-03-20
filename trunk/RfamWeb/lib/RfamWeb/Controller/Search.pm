
# Search.pm
# jt6 20061108 WTSI
#
# $Id: Search.pm,v 1.1 2008-09-12 09:13:10 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Search - top-level platform for performing various searches

=cut

package RfamWeb::Controller::Search;

=head1 DESCRIPTION

This controller is responsible for running searches. This is actually just an
empty wrapper around a shared base class. See
L<PfamBase::Controller::Search> for more details.

$Id: Search.pm,v 1.1 2008-09-12 09:13:10 jt6 Exp $

=cut

use strict;
use warnings;

use JSON;
use Scalar::Util qw( looks_like_number );
use Data::UUID;
use Moose;
use namespace::autoclean;
use Email::Valid;
use Data::Dump qw( dump );

BEGIN {
  extends 'Catalyst::Controller::REST';
}

use base 'PfamBase::Controller::Search::BatchSearch';

# set up the list of content-types that we handle
__PACKAGE__->config(
  'default' => 'text/html',
  'map'     => {
    'text/html'          => [ 'View', 'TT' ],
    'text/xml'           => [ 'View', 'TT' ],
    'text/plain'         => [ 'SeqSearchResultsFormatter', 'tsv' ],
    'application/x-gff3' => [ 'SeqSearchResultsFormatter', 'gff' ],
    'application/json'   => 'JSON',
  }
);

# set the name of the section
__PACKAGE__->config( SECTION => 'family' );

#-------------------------------------------------------------------------------

=head2 METHODS

=head2 begin : Private

Sets up the stash for all types of search.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'search';
  
  # tell the layout template to disable the summary icons
  $c->stash->{iconsDisabled} = 1;
}

#-------------------------------------------------------------------------------
#- single-sequence searches ----------------------------------------------------
#-------------------------------------------------------------------------------

=head2 search : Chained('/') PathPart('search') CaptureArgs(0)

Start of a chain for handling search actions.

=cut

sub search : Chained( '/' )
             PathPart( 'search' )
             CaptureArgs( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Search::search: start of chain' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 search : Chained('search') PathPart('') Args(0)

Shows the Rfam search page.

=cut

sub search_page : Chained( 'search' )
                  PathPart( '' )
                  Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Search::search_page: end of chain; showing search page' )
    if $c->debug;

  $c->stash->{pageType} = 'search';
  $c->stash->{template} = 'pages/layout.tt';
}

#-------------------------------------------------------------------------------

=head2 sequence : Chained('search') PathPart('sequence') Args ActionClass('REST::ForBrowsers')

Hook for RESTful actions for submitting searches (POST) and retrieving results
(GET).

=cut

sub sequence : Chained( 'search' )
               PathPart( 'sequence' )
               Args
               ActionClass( 'REST::ForBrowsers' ) { }

#-------------------------------------------------------------------------------

sub sequence_POST : Private {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Search::sequence_POST: submitting search' )
    if $c->debug;

  # validate the input
  unless ( $c->forward('validate_single_sequence') ) {

    if ( $c->req->looks_like_browser ) {
      # copy the error message to where it will be picked up by the search form
      # template
      $c->stash->{seqSearchError} = 
        $c->stash->{rest}->{error} || 
        'There was an unknown problem when validating your search sequence.';

      # go back to the search form
      $c->forward( 'search_page' );
    }
    else {
      $this->status_bad_request( # 400 Bad request
        $c,
        message => ( $c->stash->{rest}->{error} ||
                     'There was an unknown problem when validating your search.' )
      );

      # set a template, which will be used only if we're serialising to XML
      $c->stash->{template} = 'rest/search/error_xml.tt';
    }

    return;
  }

  # no errors with the input; try to submit the search

  if ( $c->forward('queue_seq_search') ) {
    # success
    $c->stash->{template} = $c->req->looks_like_browser
                          ? 'pages/search/sequence/results.tt'
                          : 'rest/search/poll_xml.tt';
  }
  else {
    # failure
    if ( $c->req->looks_like_browser ) {
      $c->stash->{seqSearchError} = 
        $c->stash->{rest}->{error} || 
        'There was an unknown problem when submitting your search.';
      $c->forward( 'search_page' );
    }
    else {
      # the "queue_seq_search" method will set an error status and a message if
      # there's a problem

      # set a template, which will be used only if we're serialising to XML
      $c->stash->{template} = 'rest/search/error_xml.tt';
    }
  }

  $c->log->debug( 'Search::sequence_POST: serialising results' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

before qr/sequence_GET/ => sub {
  my $this = shift;
  my ( $c, $job_id ) = @_;

  $c->log->debug( 'Search::before "sequence_GET*": checking job ID' )
    if $c->debug;

  # does the job ID look sensible ?
  unless ( $job_id =~ m/^\w{8}\-(\w{4}\-){3}\w{12}$/ ) {
    $c->log->debug( 'Search::before "sequence_GET*": no job ID or bad job ID' )
      if $c->debug;

    $this->status_bad_request( # 400 Bad request
      $c, 
      message => 'No valid job ID' 
    );

    return;
  }

  $c->stash->{jobId} = $job_id;

  $c->log->debug( qq(Search::before "sequence_GET*": got a valid job ID ($job_id)) )
    if $c->debug;
};

#---------------------------------------

sub sequence_GET_html : Private {
  my ( $this, $c, $job_id ) = @_;

  $c->log->debug( 'Search::sequence_GET_html: building HTML results page' )
    if $c->debug;

  $c->stash->{template} = 'pages/search/sequence/results.tt';
}

#---------------------------------------

sub sequence_GET : Private {
  my ( $this, $c, $job_id ) = @_;

  $c->log->debug( 'Search::sequence_GET: trying to retrieve results' )
    if $c->debug;

  # try to retrieve results
  $c->forward( 'JobManager', 'retrieveResults', [ $job_id  ] );

  # we should get *something*, even if there are no results, but let's just
  # check quickly
  unless ( $c->stash->{results}->{$job_id} ) {
    $c->log->debug( 'Search::sequence_GET: no results retrieved' )
      if $c->debug;

    $this->status_no_content( # 204 No content
      $c, 
      message => "We could not find any results for job ID $job_id" 
    );

    return;
  }

  my $results = $c->stash->{results}->{$job_id};

  # search is complete
  if ( $results->{status} eq 'DONE' ) {
    $c->log->debug( 'Search::sequence_GET: search complete, results retrieved' )
      if $c->debug;

    # parse the results
    $c->forward( 'handle_results', [ $job_id  ] );
  }

  # on hold
  elsif ( $results->{status} eq 'HOLD' ) {
    $c->log->debug( 'Search::sequence_GET": search on hold' )
      if $c->debug;
    
    $c->res->status( 503 ); # 503 Service unavailable
    $c->stash->{rest}->{error} = 'HOLD';
  }

  # deleted
  elsif ( $results->{status} eq 'DEL' ) {
    $c->log->debug( 'Search::sequence_GET": search deleted' )
      if $c->debug;
    
    $this->status_gone(
      $c,
      message => 'DEL'
    );
  }

  # either pending or running
  elsif ( $results->{status} eq 'PEND' or
          $results->{status} eq 'RUN' ) {
    $c->log->debug( 'Search::sequence_GET": search pending or running' )
      if $c->debug;
    
    $this->status_accepted( # 202 Accepted
      $c,
      location => $c->req->uri,
      entity => {
          status => $results->{status}
      }
    );
  }

  # any other status code indicates some terminal problem. Retrieve the 
  # contents of the stderr column from job_stream and return it in the
  # response
  else {
    $c->log->debug( 'Search::sequence_GET: search failed' )
      if $c->debug;

    # retrieve error message from STDERR
    my $error_message = $results->{job}->job_stream->stderr ||
                        'There was a problem running your job.';

    $c->res->status( 500 ); # Internal server error
    $c->stash->{rest}->{error} = $error_message;
  }

  # set the XML template. We may not use it, if the requested output format
  # is JSON, say, but it does no harm to set it. The same template should
  # handle the situation where there are no hits from the search
  $c->stash->{template} = 'rest/search/results_xml.tt';

  my $filename;
  if ( $c->req->accepts( 'application/json') ) {
    $filename = "rfam_search_$job_id.json";
  }
  elsif ( $c->req->accepts( 'text/xml') ) {
    $filename = "rfam_search_$job_id.xml";
  }

  $c->res->header( 'Content-Disposition' => "attachment; filename=$filename" )
    if $filename;

  # make sure there were some results
  unless ( scalar keys %{ $c->stash->{results} } ) {
    $c->log->debug( 'Search::sequence_GET: no results found' ) 
      if $c->debug;

    $this->status_no_content( # 204 No content
      $c, 
      message => "There there were no hits for job ID $job_id" 
    );

    return;
  }

  $c->log->debug( 'Search::sequence_GET: serialising results' )
    if $c->debug;
}

#-------------------------------------------------------------------------------
#- batch searches --------------------------------------------------------------
#-------------------------------------------------------------------------------

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
#- type searches ---------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 type_search : Chained('search') PathPart('type') Args(0)

Search based on family type.

=cut

sub type_search : Chained( 'search' )
                  PathPart( 'type' )
                  Args( 0 ) {
  my ( $this, $c ) = @_;

  my @paths;
  if ( defined $c->req->param('paths') ) {
    $c->log->debug( 'Search::Type::type: paths parameters: |' 
                    . $c->req->param('paths') . '|' ) if $c->debug;

    if ( $c->req->param('paths') =~ m/^([\s\w\;\-\,]+)$/ ) {
      @paths = split ",", $1;         
    }
    else {
      $c->stash->{typeSearchError} = 'You did not supply a valid list of family types.';

      $c->log->debug( 'Search::Type::type: bad paths list' )
        if $c->debug;

      return;
    }
    
  }
  else {
    $c->stash->{typeSearchError} = 'You did not supply a list of family types.';

    $c->log->debug( 'Search::Type::type: no paths list' )
      if $c->debug;

    return;
  }

  unless ( scalar @paths ) {
    $c->stash->{typeSearchError} = 'We did not find a list of family types.';

    $c->log->debug( 'Search::Type::type: empty paths list' )
      if $c->debug;

    return;
  }

  my @hits = $c->model('RfamDB::Rfam')
               ->search( { type => { 'IN', \@paths } },
                         {} );

  $c->log->debug( 'Search::Type::type: found ' . scalar @hits . ' hits' )
    if $c->debug;

  $c->stash->{paths}    = \@paths;
  $c->stash->{results}  = \@hits;
  $c->stash->{template} = 'pages/search/type/results.tt';
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 validate_single_sequence : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "searchError". 

=cut

sub validate_single_sequence : Private {
  my ( $this, $c ) = @_;
  
  # parse and validate the sequence itself
  unless ( $c->forward('parse_sequence') ) {
    $c->stash->{rest}->{error} ||= 'Invalid sequence. Please try again with a valid RNA sequence.';

    $c->log->debug( 'Search::validate_single_sequence: sequence parsing failed' )
      if $c->debug;

    return 0;
  }

  # no options to check right now

  $c->log->debug( 'Search::validate_single_sequence: validating input was successful' )
    if $c->debug;
      
  return 1;
}

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
  
=head2 parse_sequence : Private

Parses the sequence supplied by the CGI parameter "seq". Drops the sequence 
into the stash if it passed validation. Sets an error message in the stash if 
there was a specific problem.

=cut

sub parse_sequence : Private {
  my ( $this, $c ) = @_;

  # make sure we actually have a sequence...
  unless ( defined $c->req->param('seq') and
           $c->req->param('seq') ne '' ) {
    $c->stash->{rest}->{error} = 'You did not supply a nucleic-acid sequence.';
    
    $c->log->debug( 'Search::parse_sequence: no sequence; failed' )
      if $c->debug;
      
    return 0;
  }
  
  # break the string into individual lines and get parse any FASTA header lines
  # before recombining. If there is no user-supplied FASTA header, one will be
  # supplied for them
  my @seqs = split /\n/, $c->req->param('seq');

  my $header;
  if ( $seqs[0] =~ /^\>([\w\s]+)/ ) {
    $c->log->debug( 'Search::parse_sequence: found a user-supplied FASTA header; stripping it' )
      if $c->debug;
    
    shift @seqs;
  }

  my $seq = uc( join '', @seqs );

  # handle various line endings. No need to worry about \n, since we got rid of
  # that with the "split" above
  $seq =~ s/[\s\r\d]+//g;

  # check the length of the sequence at this point. If it's too long, bail
  my $length = length $seq;
  if ( $length > $this->{maxSeqLength} ) {
    $c->stash->{rest}->{error} = 
        'Your sequence is too long. The maximum length of search sequences is '
      . $this->{maxSeqLength} . ' bases. Please try again with a shorter '
      . 'sequence, or use the batch search form and get your results by email.';
    
    $c->log->debug( 'Search::parse_sequence: sequence is too long; failed' )
      if $c->debug;
    
    return 0;
  }

  # check that the sequence string contains only the appropriate letters. Bail
  # if it has anything else in it
  unless ( $seq =~ m/^[ACGUTSWMKRYBDHVN\-\.]+$/ ) {
    $c->stash->{rest}->{error} = 
      'Invalid sequence. Please try again with a valid nucleic-acid sequence';
    
    $c->log->debug( 'Search::parse_sequence: sequence contains illegal characters' )
      if $c->debug;
    
    return 0;
  }

  # passed all checks; stuff the header (either user-supplied or generated here)
  # and the sequence into the stash
  $c->stash->{input}  = "> UserSeq\n" . $seq;

  $c->log->debug( 'Search::parse_sequence: parsing sequence was successful' )
    if $c->debug;
      
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_seq_search : Private

Queues an Rfam search. Sets the HTTP response status and body appropriately.

=cut

sub queue_seq_search : Private {
  my ( $this, $c ) = @_;
  
  # first, check there's room on the queue
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->find( { status   => 'PEND',
                       job_type => 'rfam' },
                     { select => [ { count => 'status' } ],
                       as     => [ 'numberPending' ] } );
  
  $c->stash->{numberPending} = $rs->get_column( 'numberPending' );
  $c->log->debug( 'Search::queue_seq_search: |' . 
                  $c->stash->{numberPending} . '| jobs pending' ) if $c->debug;
  
  if ( $c->stash->{numberPending} >= $this->{pendingLimit} ) {
    $c->log->debug( 'Search::queue_seq_search: too many Rfam jobs in queue ('
                    . $c->stash->{numberPending} . ')' ) if $c->debug;

    $c->res->status( 503 ); # 503 Service unavailable
    $c->stash->{rest}->{error} = 
      'There are currently too many Rfam jobs in the sequence search queue. ' . 
      'Please try again in a little while';

    return 0;
  }
  
  #----------------------------------------

  # ok. There's room on the queue, so we can submit the hmmer job and, if 
  # required, the blast job
  my @jobs;
  
  unless ( $c->forward('queue_rfam') ) {
    $c->log->debug( 'Search::queue_seq_search: problem submitting Rfam search' )
      if $c->debug;
    
    $c->res->status( 500 ); # 500 Internal server error
    $c->stash->{rest}->{error} ||= 'There was a problem queuing your Rfam search';

    return 0;
  }

  #----------------------------------------

  # if we get to here, the job submission worked
  $c->stash->{jobStatusJSON} = to_json( $c->stash->{jobStatus} );
  
  $c->log->debug( 'Search::queue_seq_search: json string: |' 
                  . $c->stash->{jobStatusJSON} . '|' ) if $c->debug;

  $this->status_created( # 201 Created
    $c,
    location => $c->uri_for( $c->action, $c->stash->{jobId} ),
    entity   => $c->stash->{jobStatus}
  );

  $c->log->debug( 'Search::queue_seq_search: sequence search submitted')
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
                                 opened         => \'NOW()',
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

=head2 handle_results : Private

Parses the results and filter based on the the users defined parameters. The 
parsed results are put in a very generic format so that they can then be used 
for generating the results tables and graphics.

=cut

sub handle_results : Private {
  my ( $this, $c, $job_id ) = @_;
  
  $c->log->debug( "Search::handle_results: handling results for |$job_id|" )
    if $c->debug;

  # parse the log into a sensible data structure  
  $c->forward( 'parse_log', [ $job_id ] );
#  $c->log->debug( 'Search::Sequence::handle_results: results data structure: ' .
#                  dump( $c->stash->{rest}->{hits} ) ) if $c->debug;  

  # foreach my $hit ( @{ $c->stash->{rest}->{hits} } ) {
  foreach my $id ( keys %{ $c->stash->{rest}->{hits} } ) {
    foreach my $hit ( @{ $c->stash->{rest}->{hits}->{$id} } ) {
      $c->log->debug( 'Search::handle_results: hit: ', dump( $hit ) )
        if $c->debug;
      
      $hit->{alignment}->{ss}       = '           ';
      $hit->{alignment}->{hit_seq}  = sprintf '%10d ', $hit->{blocks}->[0]->{hit}->{start};
      $hit->{alignment}->{match}    = '           ';
      $hit->{alignment}->{user_seq} = sprintf '%10d ', $hit->{start};
          
      foreach my $block ( @{ $hit->{blocks} } ) {
        $hit->{alignment}->{ss}       .= $block->{ss};
        $hit->{alignment}->{hit_seq}  .= $block->{hit}->{seq};
        $hit->{alignment}->{match}    .= $block->{match};
        $hit->{alignment}->{user_seq} .= $block->{user}->{seq};
      }
          
      $hit->{alignment}->{ss}       .= '           ';
      $hit->{alignment}->{hit_seq}  .= sprintf ' %-10d', $hit->{blocks}->[-1]->{hit}->{end};
      $hit->{alignment}->{match}    .= '           ';
      $hit->{alignment}->{user_seq} .= sprintf ' %-10d', $hit->{end};

      # the blocks are really only need for building up the complete alignment
      # snippet so we'll remove them here to avoid having them clutter up output
      # formats like JSON
      delete $hit->{blocks};

      # keep track of the total number of matches
      $c->stash->{rest}->{numHits}++;
    }
  }

  # need to hash the hits according to rfam_acc, so that in the XML output
  # there will be one <match> per rfam family, under which there will be
  # one <location> for each hit for that family on the search sequence.

  # add some metadata
  my $raw = $c->stash->{results}->{$job_id}->{job};
  $c->stash->{rest}->{jobId}   = $job_id;
  $c->stash->{rest}->{opened}  = $raw->opened;
  $c->stash->{rest}->{started} = $raw->started;
  $c->stash->{rest}->{closed}  = $raw->closed;
  ( $c->stash->{rest}->{searchSequence} ) = $raw->stdin =~ m/^\> UserSeq\n(.*)/;
  # (strip off the FASTA header that was added before storing the sequence
  # in job_stream.)
  
  $c->log->debug( 'Search::handle_results: modified results data structure: ' .
                  dump( $c->stash->{rest}->{hits} ) ) if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 parse_log : Private

Parses the output from the rfam_scan script and dumps the resulting data 
structure into the stash.

=cut

sub parse_log : Private {
  my ( $this, $c, $job_id ) = @_;
  
  # we need to look up the accession for a family, based on the ID
  $c->forward( 'get_id_acc_mapping' );

  # split the log into individual lines and parse them 
  my @lines = split /\n/, $c->stash->{results}->{$job_id}->{rawData};
  
  my $hits     = {}; # everything...
  my $hit;           # an individual hit
  my $id       = ''; # the current family
  my $strand   = ''; # the current strand, plus or minus
  my ( $seq_start, $seq_end ); # sequence start/end positions
  my ( $hit_start, $hit_end ); # hit start/end positions

  for ( my $n = 0; $n < scalar @lines; $n++ ) {
    my $line = $lines[$n];
    
    # $c->log->debug( sprintf "Search::Sequence::parse_log: line % 3d: %s",
    #                         $n, $line ) if $c->debug;
    
    # store the name of the family for this hit
    if ( $line =~ m/^CM: ([\w\-]+)/ ) {
      # $c->log->debug( "Search::Sequence::parse_log: results for |$1|" )
      #   if $c->debug;
      $id = $1;
    }

    # sequence start/end
    elsif ( $line =~ m|^>.*?/(\d+)\-(\d+)$| ) {
      # $c->log->debug( "Search::Sequence::parse_log: sequence start-end: |$1-$2|" )
      #   if $c->debug;

      # store the ID for later
      $seq_start = $1;
      $seq_end   = $2;
    }

    # plus or minus strand
    elsif ( $line =~ m/^\s*(.*?) strand results/ ) {
      # $c->log->debug( "Search::Sequence::parse_log: results for |$1| strand" )
      #   if $c->debug;

      # store the strand
      $strand = $1 eq 'Plus' ? '+' : '-';
    }

    # get the sequence, start and end
    elsif ( $line =~ m|Query = (\d+) - (\d+), Target = (\d+) - (\d+)| ) {
      # if ( $c->debug ) {
      #   $c->log->debug( "Search::Sequence::parse_log: query start-end:  |$1 - $2|" );
      #   $c->log->debug( "Search::Sequence::parse_log: target start-end: |$3 - $4|" );
      # }

      if ( $4 > $3 ) {
        $hit_start = $3;
        $hit_end   = $4;
      }
      else {
        $hit_start = $4;
        $hit_end   = $3;
      }
    }

    # get the score, etc.
    elsif ( $line =~ m|Score = (.*?), E = (.*?), P = (.*?), GC =\s+(.*)$| ) {
      # if ( $c->debug ) {
      #   $c->log->debug( "Search::Sequence::parse_log: score: |$1|" );
      #   $c->log->debug( "Search::Sequence::parse_log: E:     |$2|" );
      #   $c->log->debug( "Search::Sequence::parse_log: P:     |$3|" );
      #   $c->log->debug( "Search::Sequence::parse_log: GC:    |$4|" );
      # }

      $hit = {
        id     => $id,
        strand => $strand,
        start  => $hit_start + $seq_start - 1,
        end    => $hit_end   + $seq_start - 1,
        score  => $1,
        E      => $2,
        P      => $3,
        GC     => $4,
        blocks => []
      };

      # try to map the family ID to its accession and store that too
      if ( $c->stash->{id_acc_mapping} and
           $c->stash->{id_acc_mapping}->{$id}) {
        $hit->{acc} = $c->stash->{id_acc_mapping}->{$id};
      }

      push @{ $hits->{$id} }, $hit;
      
      # parse the alignment blocks
      for ( my $b = $n + 2; $b < scalar @lines; $b += 5 ) {
        last unless $lines[$b+1] =~ m/^\s+\d+.*?\s+\d+\s*$/;
        
        # $c->log->debug( 'Search::Sequence::parse_log: block constitutes lines ' .
        #                 "$b - " . ( $b + 3 ) ) if $c->debug; 
        
        my $block = read_block( [ @lines[ $b .. $b+3 ] ] );
  
        # $c->log->debug( 'Search::Sequence::parse_log: block: ' .
        #                 dump( $block ) ) if $c->debug; 
        
        push @{ $hit->{blocks} }, $block;
      }
    }
  }
  
  # stash the parsed results
  $c->stash->{rest}->{hits} = $hits;
}

#-------------------------------------------------------------------------------

sub get_id_acc_mapping : Private {
  my ( $this, $c ) = @_;

  my $cache_key = 'id_acc_mapping';
  my $mapping = $c->cache->get( $cache_key );

  if ( $mapping ) {
    $c->log->debug( 'Search::get_id_acc_mapping: retrieved ID-to-acc mapping from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Search::get_id_acc_mapping: failed to retrieve ID-to-acc mapping from cache; going to DB' )
      if $c->debug;

    my @families = $c->model( 'RfamDB::Rfam' )
                     ->search( {},
                               { columns => [ qw( rfam_acc rfam_id ) ] } );

    unless ( @families ) {
      $c->log->debug( 'Search::get_id_acc_mapping: failed to get list of families from DB' )
        if $c->debug;
      return;
    }

    %$mapping = map { $_->rfam_id => $_->rfam_acc } @families;

    unless ( scalar( keys %$mapping ) ) {
      $c->log->debug( 'Search::get_id_acc_mapping: no keys in mapping hash; not storing mapping' )
        if $c->debug;
      return;
    }

    # cache for one week
    $c->cache->set( $cache_key, $mapping, 604800 ) unless $ENV{NO_CACHE};
  }

  $c->log->debug( 'Search::get_id_acc_mapping: found ' . scalar( keys %$mapping ) 
                  . ' families in the mapping' )
    if $c->debug;

  $c->stash->{id_acc_mapping} = $mapping;
}

#-------------------------------------------------------------------------------
#- subroutines -----------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 NON-ACTION METHODS

=head2 read_block

Given a reference to an array containing the four lines of an alignment block,
this function parses the block and returns a reference to a hash containing all 
of the relevant information.

=cut

sub read_block {
  my $lines = shift;

  my $block = {};

  $lines->[0] =~ s/^\s*(.*?)\s*$/$1/;
  $block->{ss} = $1;

  $lines->[1] =~ m/^\s+(\d+)\s+(.*?)\s+(\d+)\s*$/;
  $block->{hit}->{seq}   = $2;
  $block->{hit}->{start} = $1;
  $block->{hit}->{end}   = $3;

  $block->{match} = substr $lines->[2], 11, length $2;

  $lines->[3] =~ m/^\s+(\d+)\s+(.*?)\s+(\d+)\s*$/;
  $block->{user}->{seq}   = $2;
  $block->{user}->{start} = $1;
  $block->{user}->{end}   = $3;

  return $block;
}

#o-------------------------------------------------------------------------------

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
