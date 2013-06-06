
# SunburstMethods.pm
# jt6 20120703 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Family::SunburstMethods - role to add various sunburst-related
methods to the family page controller

=cut

package RfamWeb::Roles::Family::SunburstMethods;

=head1 DESCRIPTION

This is a role to add various sunburst-related methods to the Family controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use Data::Dump qw( dump );
use Text::Wrap qw( $columns wrap );
use Data::UUID;
use Try::Tiny;

$Text::Wrap::columns = 60;

#-------------------------------------------------------------------------------

=head2 accessions : Chained('sunburst') PathPart('accessions')

Stub to add REST hooks at "family/ENTRY/sunburst/storeaccessions". The concrete
methods will accept a list of sequence accessions and store them, returning a
UUID to identify that list.

=cut

sub accessions : Chained( 'sunburst' )
                 PathPart( 'accessions' )
                 Args
                 ActionClass( 'REST::ForBrowsers' ) { }

#---------------------------------------

=head2 accessions_POST

Stores the supplied list of accessions and returns a job ID. If the accessions
list contained invalid accessions, a "400 Bad request" response is returned.

=cut

sub accessions_POST {
  my ( $this, $c ) = @_;

  # the client POSTs the list of accessions as a JSON string in the request 
  # body. That list is deserialised and stuffed into the request by the
  # "Deserialize" ActionClass
  my $accessions_list = $c->req->data;
  
  if ( my $job_id = $c->forward( 'store_accessions', [ $accessions_list ] ) ) {

    $c->log->debug( 'Family::SunburstMethods::accessions_POST: successfully stored accessions; job ID '
                    . $job_id ) if $c->debug;

    $this->status_created( # 201 Created
      $c,
      location => $c->req->uri . "/$job_id",
      entity   => { jobId => $job_id,
                    acc   => $c->stash->{acc} }
    );
   
  } 
  else {
    $c->log->debug( 'Family::SunburstMethods::accessions_POST: failed to store accessions' )
      if $c->debug;

    $this->status_bad_request( # 400 Bad request
      $c,
      message => ( $c->stash->{errorMsg} ||
                   'There was an unknown problem when storing your sequence accessions.' )
    );
  }
}

#---------------------------------------

=head2 before accessions_GET*

Validates job ID being handed to the methods that care about valid ID. Stashes
a valid ID as "job_id". That slot is empty if the ID was invalid.

=cut

before [ qw( accessions_GET_html 
             accessions_GET ) ] => sub {
  my ( $this, $c, $job_id ) = @_;

  # the job ID is going to the template and the client side shortly, so we'll 
  # call it "jobId" rather than the more comfortable "job_id"... just to add
  # to the confusion...

  ( $c->stash->{jobId} ) = $job_id =~ m/^([A-F0-9]{8}\-([A-F0-9]{4}\-){3}[A-F0-9]{12})$/i;

  unless ( $c->stash->{jobId} ) {
    $c->log->debug( 'Family::SunburstMethods::before "accessions_GET*": bad job id' )
      if $c->debug;
  }
};

#---------------------------------------

=head2 accessions_GET_html

Generates a tool window for showing the alignment. If there's no valid job ID, an 
error message will be shown in that window.

=cut

sub accessions_GET_html {
  my ( $this, $c, $job_id ) = @_;

  $c->stash->{template} = 'components/tools/sunburst_alignment.tt';

  if ( $c->stash->{jobId} ) {
    $c->log->debug( "Family::SunburstMethods::accessions_GET_html: building tool window for job $job_id (" . $c->stash->{jobId} .')' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::SunburstMethods::accessions_GET_html: bad job id' )
      if $c->debug;
    $c->stash->{errorMsg} = 'Invalid job ID';
  }
}

#---------------------------------------

=head2 accessions_GET

Returns the list of stored accessions to a non-browser client.

=cut

sub accessions_GET {
  my ( $this, $c, $job_id ) = @_;

  unless ( $c->stash->{jobId} ) {
    $c->log->debug( 'Family::SunburstMethods::accessions_GET: no job id' )
      if $c->debug;

    $this->status_bad_request(
      $c,
      message => 'Invalid job ID'
    );

    return;
  }
   
  my $accessions = $c->forward( 'retrieve_accessions', [ $job_id ] );

  if ( $c->stash->{errorMsg} ) {
    $this->status_not_found(
      $c,
      message => $c->stash->{errorMsg}
    );
    return;
  }

  $this->status_ok(
    $c,
    entity => $accessions
  );
}
  
#-------------------------------------------------------------------------------

sub fasta : Chained( 'sunburst' )
            PathPart( 'fasta' )
            Args {
  my ( $this, $c, $jobId ) = @_;

  ( $c->stash->{jobId} ) = $jobId =~ m/^([A-F0-9]{8}\-([A-F0-9]{4}\-){3}[A-F0-9]{12})$/i;

  unless ( $c->stash->{jobId} ) {
    $c->log->debug( 'Family::SunburstMethods::fasta: bad job id' )
      if $c->debug;

    # invalid job ID; bail now
    $c->res->status( 400 ); # Bad request
    $c->res->body( 'No valid job ID' );

    return;
  }

  $c->log->debug( "Family::SunburstMethods::fasta: got a valid job ID $jobId" ) 
    if $c->debug;

  # see if we can recover the accessions...
  my $accessions = $c->forward( 'retrieve_accessions', [ $jobId ] );

  # and build the FASTA file
  my $fasta = $c->forward( 'build_fasta', [ $accessions, 1 ] );

  unless ( $fasta and
           $fasta =~ m/^>/ ) {
    $c->log->debug( 'Family::SunburstMethods::fasta: failed to generate fasta file' )
      if $c->debug;

    $c->res->status( 500 ); # Internal server error
    $c->res->body( 'Failed to generate a FASTA file' );

    return;
  }

  my $filename = $c->stash->{acc} . '_selected_sequences.fa';
  $c->res->headers->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->content_type( 'text/plain' );
  $c->res->status( 200 ); # OK
  $c->res->body( $fasta );
}

#-------------------------------------------------------------------------------

=head2 align : Chained('sunburst') PathPart('alignment') Args ActionClass('REST::ForBrowsers')

Stub to set up RESTful dispatching to methods that handle generation, polling
and return of an alignment of sequences that were selected in the sunburst.

=cut

sub align : Chained( 'sunburst' )
            PathPart( 'alignment' )
            Args
            ActionClass( 'REST::ForBrowsers' ) { }

#---------------------------------------

# given a job ID that points to a set of stored sequences, this method submits
# those sequences to be aligned to the CM

sub align_POST {
  my ( $this, $c, $jobId ) = @_;

  ( $c->stash->{jobId} ) = $jobId =~ m/^([A-F0-9]{8}\-([A-F0-9]{4}\-){3}[A-F0-9]{12})$/i;

  unless ( $c->stash->{jobId} ) {
    $c->log->debug( 'Family::SunburstMethods::align_POST: bad job id' )
      if $c->debug;

    # invalid job ID; bail now
    $c->res->status( 400 ); # Bad request
    $c->res->body( 'No valid job ID' );

    return;
  }

  $c->log->debug( "Family::SunburstMethods::align_POST: got a valid job ID $jobId" ) 
    if $c->debug;

  # see if we can recover the accessions...
  my $accessions = $c->forward( 'retrieve_accessions', [ $jobId ] );

  if ( $c->stash->{errorMsg} ) {
    $c->log->debug( 'Family::SunburstMethods::align_POST: no job; returning 404' )
      if $c->debug;

    $c->res->status( 404 ); # Not found
    $c->res->body( $c->stash->{errorMsg} );

    return;
  }

  $c->log->debug( 'Family::SunburstMethods::align_POST: got ' . scalar @$accessions 
                  . ' accessions for job ID ' . $c->stash->{jobId} )
    if $c->debug;

  # convert the list of accessions into a FASTA-format file
  $c->stash->{fasta} = $c->forward( 'build_fasta', [ $accessions, 0 ] );

  # the "queue_alignment_job" method will set the response status and body
  # for both successful and unsuccessful submissions
  if ( $c->forward('queue_alignment_job') ) {
    $c->log->debug( 'Family::SunburstMethods::align_POST: successfully queued alignment job' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::SunburstMethods::align_POST: FAILED to successfully queue alignment job' )
      if $c->debug;
  }

}

#---------------------------------------

# returns the alignment of a set of selected sequences. Returns status 204 if
# the alignment is not yet complete

sub align_GET {
  my ( $this, $c, $jobId ) = @_;

  my $poll_only = $c->req->params->{poll} || 0;

  unless ( $jobId =~ m/^([A-F0-9]{8}\-([A-F0-9]{4}\-){3}[A-F0-9]{12})$/i ) {
    $c->log->debug( 'Family::SunburstMethods::align_GET: bad job id' )
      if $c->debug;

    $c->res->status( 400 ); # Bad request
    $c->res->body( 'No valid job ID' )
      if not $poll_only;

    return;
  }

  $c->log->debug( 'Family::SunburstMethods::align_GET: got a valid job ID ' . $jobId ) 
    if $c->debug;

  my $job = $c->model( 'WebUser::JobHistory' )
              ->search( { job_id => $jobId },
                        { prefetch => [ 'job_stream' ] } )
              ->first;
  
  $c->log->debug( 'Family::SunburstMethods::align_GET: method: ' . $c->req->method )
    if $c->debug;

  # no job; throw an error
  unless ( defined $job ) {
    $c->log->debug( 'Family::SunburstMethods::align_GET: no job; returning 404' )
      if $c->debug;

    $c->res->status( 404 ); # Not found
    $c->res->body( "No alignment with that job ID ($jobId)" )
      if not $poll_only;

    return;
  }

  if ( $job->status eq 'DONE' ) {
    $c->log->debug( 'Family::SunburstMethods::align_GET: job complete' )
      if $c->debug;

    $c->res->status( 200 ); # OK

    if ( $poll_only ) {
      $c->log->debug( 'Family::SunburstMethods::align_GET: this is a polling request; returning just status 200' )
        if $c->debug;
      $c->res->body( 'DONE' );
    }
    else {
      $c->log->debug( 'Family::SunburstMethods::align_GET: not a polling request; returning alignment' )
        if $c->debug;
      my $filename = $c->stash->{acc} . '_sequence_alignment.txt';
      $c->res->headers->header( 'Content-disposition' => "attachment; filename=$filename" );
      $c->res->content_type( 'text/plain' );
      $c->res->body( $job->stdout );
    }

    return;
  }

  if ( $job->status eq 'PEND' or 
       $job->status eq 'RUN' ) {
    $c->log->debug( 'Family::SunburstMethods::align_GET: job is pending or running; returning 204' )
      if $c->debug;

    $c->res->status( 204 ); # No content

    return;
  }

  $c->log->debug( 'Family::SunburstMethods::align_GET: job failed or deleted; returning 500 error' )
    if $c->debug;

  $c->res->status( 500 ); # Internal server error
  $c->res->body( 'There was a problem generating your alignment' )
    if not $poll_only;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 store_accessions : Private

Given a list of sequences accessions, this method stores them in a tracking
table and returns the UUID that identifies that particular set.

=cut

sub store_accessions : Private {
  my ( $this, $c, $accessions_list ) = @_;
  
  unless ( scalar @$accessions_list ) {
    $c->log->debug( 'Family::SunburstMethods::store_accessions: not a valid accessions list' )
      if $c->debug;

    $c->stash->{errorMsg} = 'Not a valid list of accessions';

    return 0;
  }

  $c->log->debug( 'Family::SunburstMethods::store_accessions: found ' . scalar @$accessions_list .
                  ' accession(s) in parameter' )
    if $c->debug;

  # crudely validate the accessions
  foreach ( @$accessions_list ) {
    next if m/^(\w+)$/;
    $c->log->debug( "Family::SunburstMethods::store_accessions: not a valid accession ($_)" )
      if $c->debug;
    $c->stash->{errorMsg} = 'Not a valid accessions list';
    return 0;
  }
  my $accessions = join ',', @$accessions_list;
  
  # join the accessions into a string and validate that string
  # my $accessions = join ',', @$accessions_list;
  # unless ( $accessions =~ m/^((\w+,\s*)+)?\w+$/ ) {
  #   $c->log->debug( 'Family::SunburstMethods::store_accessions: not a valid accessions string' )
  #     if $c->debug;

  #   $c->stash->{errorMsg} = 'Not a valid accessions list';

  #   return 0;
  # }
  
  $c->log->debug( 'Family::SunburstMethods::store_accessions: got some valid accessions' ) 
    if $c->debug;
  
  # build an ID for this set of IDs
  my $job_id = Data::UUID->new()->create_str();
  
  # add it to the DB
  my $row;
  eval {
    # we use "update_or_create" because we don't really care if this ID has 
    # been used before; it's not important enough to spend time making sure 
    # it's unique
    $row = $c->model('WebUser::Species_collection')
             ->update_or_create( { job_id  => $job_id,
                                   id_list => $accessions } );
  };
  if ( $@ ) {
    # oops...
    $c->log->error( "Family::SunburstMethods::store_accessions: error from query: |$@|" )
      if $c->debug;

    $c->stash->{errorMsg} = 'Failed to store accessions list';

    return 0;
  }

  # the row was successfully added to the table. Hand back the job ID
  return $job_id;
}

#-------------------------------------------------------------------------------

=head2 retrieve_accessions : Private

Retrieves the set of sequence accessions with the specified UUID.

=cut

sub retrieve_accessions : Private {
  my ( $this, $c, $job_id ) = @_;

  $c->log->debug( "Family::SunburstMethods::retrieve_accessions: looking up job ID, $job_id" )
    if $c->debug;

  my $job = $c->model( 'WebUser::Species_collection' )
              ->find( $job_id );

  unless ( $job ) {
    $c->log->debug( 'Family::SunburstMethods::retrieve_accessions: no row for that job id' )
      if $c->debug;

    $c->stash->{errorMsg} = 'We could not find a set of accessions using that ID';

    return;
  }

  my @accessions = split /,/, $job->id_list;

  unless ( scalar @accessions ) {
    $c->log->debug( 'Family::SunburstMethods::retrieve_accessions: no accessions in that job' )
      if $c->debug;

    $c->stash->{errorMsg} = 'There were no accessions for that job';

    return;
  }

  return \@accessions;
}

#-------------------------------------------------------------------------------

=head2 queue_alignment_job : Private

Checks that the queue isn't full and, if not, submits the set of sequences 
for alignment.

=cut

sub queue_alignment_job : Private {
  my ( $this, $c ) = @_;
  
  # first, check there's room on the queue
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->find( { status   => 'PEND',
                       job_type => $c->stash->{alignment_job_type} },
                     { select => [ { count => 'status' } ],
                       as     => [ 'number_pending' ] } );
  
  $c->stash->{number_pending} = $rs->get_column( 'number_pending' );

  $c->log->debug( 'Family::SunburstMethods::queue_alignment_job: |' . 
                  $c->stash->{number_pending} . '| jobs pending' ) if $c->debug;
  
  if ( $c->stash->{number_pending} >= ( $this->{pendingLimit} || 100 ) ) {
    $c->log->debug( 'Family::SunburstMethods::queue_alignment_job: too many Rfam jobs in queue ('
                    . $c->stash->{number_pending} . ')' ) if $c->debug;

    $c->res->status( 503 ); # Service unavailable
    $c->res->body( 'There are currently too many alignment jobs in the queue. ' 
                   . 'Please try again in a little while' );

    return 0;
  }
  
  #----------------------------------------

  # ok. There's room on the queue. Check if we've seen this job ID recently. If
  # we have, the user probably hit "reload" in the submission page. Don't queue 
  # the same query again, just point to it
  my $exists = $c->model( 'WebUser::JobHistory' )
                 ->search( { job_id => $c->stash->{jobId},
                             opened => { '>', \'DATE_SUB( NOW(), INTERVAL 1 DAY )' } } );
  if ( defined $exists and
       $exists > 0 ) { 
    $c->log->debug( 'Family::SunburstMethods::queue_alignment_job: query '
                    . $c->stash->{jobId} . ' has already been submitted within the last day; skipping submission' )
      if $c->debug;
  }
  else {
    # we haven't seen this particular job before; submit the job
    if ( $c->forward('enqueue_alignment') ) {
      $c->log->debug( 'Family::SunburstMethods::queue_alignment_job: alignment job '
                      . $c->stash->{jobId} . ' submitted' )
        if $c->debug;
    }
    else {
      $c->log->debug( 'Family::SunburstMethods::queue_alignment_job: problem submitting alignment job '
                      . $c->stash->{jobId} )
        if $c->debug;
      
      $c->res->status( 500 ); # Internal server error
      $c->res->body( $c->stash->{errorMsg} 
                     || 'There was a problem queuing your Rfam search' );

      return 0;
    }
  }

  #----------------------------------------

  # if we get to here, the job submission worked
  $this->status_created( # 201 Created
    $c,
    location => $c->req->uri,
    entity   => {}
  );

  $c->log->debug( 'Family::SunburstMethods::queue_alignment_job: sequence search submitted')
    if $c->debug;

  return 1;
}

#-------------------------------------------------------------------------------

=head2 enqueue_alignment : Private

Inserts the necessary rows into the JobHistory and JobStream tables in order
to submit a set of sequences for alignment.

=cut

sub enqueue_alignment : Private {
  my ( $this, $c ) = @_; 

  # set the options. For the alignment job we need to pass in the family 
  # accession, so that the dequeuer can find the family CM.
  my $opts = $c->stash->{acc};

  # guesstimate the time it will take to build the alignment
  my $estimated_time = 0; # TODO see if we can find a sensible formula for this

  my $txn = sub {
    # add this job to the tracking tables
    my $job_history = $c->model('WebUser::JobHistory')
                        ->create( { options        => $opts,
                                    job_type       => $c->stash->{alignment_job_type},
                                    estimated_time => $estimated_time,
                                    job_id         => $c->stash->{jobId},
                                    opened         => \'NOW()',
                                    status         => 'PEND' } );

    my $job_stream = $c->model('WebUser::JobStream')
                       ->create( { id    => $job_history->id,
                                   stdin => $c->stash->{fasta} || q() } );

    # check the submission time with a separate query
    my $history_row = $c->model( 'WebUser::JobHistory' )
                        ->find( { id => $job_history->id } );
    
    return $history_row;
  };

  my $history_row;
  try {
    $history_row = $c->model('WebUser')->txn_do( $txn );
  } catch {
    $c->log->debug( 'Family::SunburstMethods::enqueue_alignment: problem submitting alignment job: $_' )
      if $c->debug;

    $c->stash->{errorMsg} = 'There was a problem submitting the alignment job';

    return 0;
  };

  $c->log->debug( 'Family::SunburstMethods::enqueue_alignment: submitted job |'
                  . $c->stash->{jobId} .'| at |' . $history_row->opened . '|' )
    if $c->debug;

  return 1;
} 

#-------------------------------------------------------------------------------

=head2 show_alignment : Chained('family') PathPart('sunburst/alignment') Args(1)

=cut

# sub show_alignment_tool : Chained( 'sunburst' )
#                           PathPart( 'alignment' )
#                           Args( 1 ) {
#   my ( $this, $c, $job_id ) = @_;
# 
#   unless ( $job_id =~ m/^([A-F0-9\-]{36})$/i ) {
#     $c->log->debug( 'Family::SunburstMethods::show_alignment_tool: bad job id' )
#       if $c->debug;
# 
#     $c->stash->{errorMsg} = 'Invalid job ID';
# 
#     return;
#   }
#    
#   $c->stash->{jobId} = $job_id;
#   $c->stash->{template} = 'components/tools/sunburst_alignment.tt';
# }

#-------------------------------------------------------------------------------

# sub selection : Chained( 'sunburst' ) 
#                 PathPart( 'selection' )
#                 Args( 0 ) 
#                 ActionClass( 'REST::ForBrowsers' ) { }
# 
# before 'selection' => sub {
#   my ( $this, $c ) = @_;
# 
#   $c->log->debug( 'Family::SunburstMethods::before "selection"' )
#     if $c->debug;
# 
#   # should we wrap the lines at a sensible length ?
#   my $pretty = $c->req->params->{dl} ? 1 : 0;
# 
#   # build a fasta file. Don't bother to wrap the lines
#   $c->stash->{sequences} = $c->forward( 'build_fasta', [ $c->req->params->{accessions}, $pretty ] );
# };
# 
# sub selection_POST : Private {
#   my ( $this, $c ) = @_;
# 
#   $c->log->debug( 'Family::SunburstMethods::selection_POST' )
#     if $c->debug;
# 
#   $c->res->content_type( 'application/x-fasta' );
#   $c->res->header( 'Content-disposition' => 'attachment; filename=selected_sequences_' . $c->stash->{acc} . '.fasta' );
#   $c->res->body( $c->stash->{sequences} );
# }

#-------------------------------------------------------------------------------

=head2 get_sequences : Private

Retrieves a list of sequence accessions for the specified job ID and gets the
sequence for all regions on each sequence. Returns the sequences as a FASTA
file.

=cut

sub get_sequences : Private {
  my ( $this, $c, $job_id, $pretty ) = @_;

  $c->log->debug( 'Family::SunburstMethods::get_sequences: wrapping sequence lines' )
    if ( $c->debug and $pretty );

  my $accessions_list = $c->forward( 'retrieve_ids', [ $job_id ] );
  unless ( $accessions_list ) {
    $c->stash->{errorMsg} ||= 'Could not retrieve sequences for that job ID';
    return;
  }

  $c->log->debug( 'Family::SunburstMethods::get_sequences: found |' 
                  . scalar @$accessions_list . '| sequence accessions' )
    if $c->debug;

  my $sequences = '';
  foreach my $acc ( @$accessions_list ) {
    next unless $acc =~ m/^\w+$/;
    my $rs = $c->model( 'RfamDB::SeqInfo' )
               ->search( { rfamseq_acc => $acc },
                         { columns => [ qw( rfamseq_acc_v
                                            seq_start 
                                            seq_end 
                                            sequence
                                            description ) ] } );
    while ( my $row = $rs->next ) {
      my $header = '>' . 
                   $row->rfamseq_acc_v . '/' .
                   $row->seq_start . '-' . $row->seq_end . ' ' .
                   $row->description;
      my $sequence = uc $row->sequence;
      $sequence =~ s/[-.]//g;
      $sequence = wrap( '', '', $sequence ) if $c->debug;

      $sequences .= "$header\n$sequence\n";
    }
  }

  return $sequences;
}

#-------------------------------------------------------------------------------

=head2 queue_alignment : Private

Queues the job that will actually generate the sequence alignment.

=cut

sub queue_alignment : Private {
  my ( $this, $c ) = @_; 

  # generate a job ID
  my $job_id = Data::UUID->new()->create_str();

  # add this job to the tracking tables
  my $job_history = $c->model('WebUser::JobHistory')
                      ->create( { options        => $c->stash->{acc},
                                  job_type       => $c->stash->{alignment_job_type},
                                  estimated_time => 0,
                                  job_id         => $job_id,
                                  opened         => \'NOW()',
                                  status         => 'PEND' } );

  my $job_stream = $c->model('WebUser::JobStream')
                     ->create( { id    => $job_history->id,
                                 stdin => $c->stash->{fasta} || q() } );

  # check the submission time with a separate query
  my $history_row = $c->model( 'WebUser::JobHistory' )
                      ->find( { id => $job_history->id } );

  # build a job status data structure that we'll convert to JSON and hand back
  # to the javascript on the client side. Because the queuing system allows
  # multiple Jobs in one page, the job_status JSON string needs to be an array
  # of hashes, each of which gives details of a separate job
  my $job_status = [
                    {
                      checkURI      => $c->uri_for( '/jobmanager/checkStatus' )
                                         ->as_string,
                      doneURI       => $c->uri_for( '/family/'.$c->stash->{acc}.'/alignment/view' )->as_string,
                      estimatedTime => 0,
                      interval      => $this->{pollingInterval},
                      jobId         => $job_id,
                      name          => 'Sequence alignment',
                      jobClass      => 'alignment',
                      opened        => $history_row->opened,
                    }
                  ];
  $c->stash->{jobStatusJSON} = to_json( $job_status );

  if ( $c->debug ) {
    $c->log->debug( 'Family::SunburstMethods::queue_alignment: job status: ', dump( $job_status ) );
    $c->log->debug( 'Family::SunburstMethods::queue_alignment: submitted job '
                    . "|$job_id| at |" . $history_row->opened . '|' );
  }
                  
  return 0;
} 

#-------------------------------------------------------------------------------

=head2 build_fasta : Private

Builds a FASTA-format sequence file from containing the region sequences for
the supplied accessions. Takes two arguments: ref to an array with the list of
accessions; boolean specifying whether or not to "pretty print" the sequences
by wrapping at 60 characters per line.

=cut

sub build_fasta : Private {
  my ( $this, $c, $accessions, $pretty ) = @_;

  $c->log->debug( 'Family::SunburstMethods::build_fasta: wrapping sequence lines' )
    if ( $c->debug and $pretty );

  my $fasta = '';
  foreach ( @$accessions ) {
    next unless m/^\w+$/;
    my $rs = $c->model( 'RfamDB::SeqInfo' )
               ->search( { rfamseq_acc => $_,
                           rfam_acc    => $c->stash->{acc} },
                         { columns => [ qw( rfamseq_acc_v
                                            seq_start 
                                            seq_end 
                                            sequence
                                            description ) ] } );
    while ( my $row = $rs->next ) {
      my $header = '>' . 
                   $row->rfamseq_acc_v . '/' .
                   $row->seq_start . '-' . $row->seq_end . ' ' .
                   $row->description;
      my $sequence = uc $row->sequence;
      $sequence =~ s/[-.]//g;
      $sequence = wrap( '', '', $sequence ) if $pretty;

      $fasta .= "$header\n$sequence\n";
    }
  }

  return $fasta;
}

#-------------------------------------------------------------------------------

#
# this version of the "build_fasta" method uses a three-table join to get 
# everything it needs. That should, in principle, be slower than getting it all
# from a single table (shouldn't it ?).
# sub old_build_fasta : Private {
#   my ( $this, $c, $accessions, $pretty ) = @_;
# 
#   $c->log->debug( 'Family::SunburstMethods::build_fasta: wrapping sequence lines' )
#     if ( $c->debug and $pretty );
# 
#   my $sequences = '';
#   foreach my $acc ( split m/,/, $accessions ) {
#     next unless $acc =~ m/^\w+$/;
#     my $rs = $c->model( 'RfamDB::RfamRegFull' )
#                ->search( { 'auto_rfamseq.rfamseq_acc' => $acc },
#                          { join     => [ qw( auto_rfam auto_rfamseq ) ],
#                            select   => [ qw( auto_rfamseq.rfamseq_acc 
#                                              auto_rfamseq.version 
#                                              seq_start 
#                                              seq_end 
#                                              sequence
#                                              auto_rfamseq.description ) ],
#                            as       => [ qw( rfamseq_acc 
#                                              version 
#                                              seq_start 
#                                              seq_end 
#                                              sequence
#                                              description ) ] } );
#     while ( my $row = $rs->next ) {
#       my $header = '>' . 
#                    $row->get_column('rfamseq_acc') . '.' . $row->get_column('version') . '/' .
#                    $row->seq_start . '-' . $row->seq_end . ' ' .
#                    $row->get_column('description');
#       my $sequence = $row->sequence;
#       $sequence =~ s/[-.]//g;
#       $sequence = wrap( '', '', $sequence ) if $c->debug;
#       # TODO should the sequence be forced to upper case ?
# 
#       $sequences .= "$header\n$sequence\n";
#     }
#   }
# 
#   return $sequences;
# }

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Sarah Burge, C<sb30@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Sarah Burge (sb30@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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

