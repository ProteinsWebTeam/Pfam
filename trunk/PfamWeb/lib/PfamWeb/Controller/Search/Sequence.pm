
# Sequence.pm
# jt6 20061108 WTSI
#
# $Id: Sequence.pm,v 1.40 2009-10-28 11:56:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Sequence - perform sequence searches

=cut

package PfamWeb::Controller::Search::Sequence;

=head1 DESCRIPTION

This controller is responsible for running sequence searches. Takes care
of outputting results as HTML or, if running via a script, as XML.

=head2 Catalyst::Controller::REST and the lack thereof

Ideally, we would have built this on top of Catalyst::Controller::REST, which
takes care of dispatching requests to the various methods, depending on the
request method (e.g. "GET", "POST"), and rendering the results in the
appropriate output format, depending on the requested content-type (e.g.
"JSON", "XML"). Unfortunately, because of issues with installing C::C::REST
when we were running on the main mod_perl servers, we had to hack around
C::C::REST and do the donkey work ourselves.


$Id: Sequence.pm,v 1.40 2009-10-28 11:56:58 jt6 Exp $

=cut

# TODO: make this use C::C::REST !

use strict;
use warnings;

use Scalar::Util qw( looks_like_number );
use Data::UUID;
use Storable qw( thaw );

use Data::Dump qw( dump );

use Bio::Pfam::Sequence;
use Bio::Pfam::Sequence::Region;
use Bio::Pfam::Sequence::MetaData;
use Bio::Pfam::Drawing::Layout::LayoutManager;
use JSON qw( -convert_blessed_universally );

use base qw( PfamBase::Controller::Search::InteractiveSearch
             PfamWeb::Controller::Search );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Retrieves the job parameters from request params and stuff them into the stash.

=cut

sub auto : Private {
  my ( $this, $c ) = @_;

  foreach my $slot ( qw( seq ga evalue altoutput ) ) {
    $c->stash->{data}->{$slot} = $c->req->param($slot) || '';
  }

  1;
}

#-------------------------------------------------------------------------------

=head2 search : Path

Queues a sequence search job and redirects to a page that polls the server for
results. 

=cut

sub search : Path {
  my ( $this, $c ) = @_;

  $c->stash->{pageType} = 'search';
  $c->stash->{template} = 'pages/layout.tt';
  
  $c->log->debug( 'Search::Sequence::search: set template to layout' )
    if $c->debug;

  # validate the input
  unless ( $c->forward('validate_input') ) {
    $c->log->debug( 'Search::Sequence::search: problem validating input' )
      if $c->debug;

    $c->stash->{seqSearchError} = $c->stash->{searchError} ||
                                  'There was an unknown problem when validating your sequence.';

    return;
  }

  # no errors with the input; try to submit the search
  $c->log->debug( 'Search::Sequence::search_POST: input is valid; queueing search' )
    if $c->debug;

  unless ( $c->forward('queue_seq_search') ) {
    $c->log->debug( 'Search::Sequence::search: problem submitting search' )
      if $c->debug;

    $c->stash->{seqSearchError} = $c->stash->{searchError}
                                  || 'There was an unknown problem when submitting your search.';

    return;
  }

  # success !
  $c->log->debug( 'Search::Sequence::search: submission successful' )
    if $c->debug;

  $c->forward( 'results', [ $c->stash->{jobId} ] );
}

#   http://onlamp.com/pub/a/onlamp/2008/02/19/developing-restful-web-services-in-perl.html?page=2

#-------------------------------------------------------------------------------

=head2 results : Local

Builds a page that will hold the results of the search(es). 

=cut

sub results : Local {
  my ( $this, $c, $arg ) = @_;

  # get hold of the job ID
  my $job_id = $c->req->param('jobId') ||
               $arg                    ||
               '';

  # decide which template to use. Both of these templates will handle showing 
  # an error message, in case something goes wrong when submitting the search
  if ( $c->stash->{output_xml} || '' ) {
    $c->log->debug( 'Search::Sequence::results: returning polling page as XML' )
      if $c->debug;
    $c->stash->{template} = 'rest/search/poll_xml.tt';
  }

  # output just the job ID, for use by PfamAlyzer
  elsif ( $c->stash->{data}->{altoutput} || 0 ) {
    $c->log->debug( 'Search::Sequence::results: returning just the job ID for PfamAlyzer' )
      if $c->debug;
    $c->res->content_type( 'text/plain' );
    $c->res->body( $c->stash->{jobId} );
    return;
  }

  else {
    $c->log->debug( 'Search::Sequence::results: returning polling page as HTML' )
      if $c->debug;
    $c->stash->{template} = 'pages/search/sequence/results.tt';
  }
  
  $c->log->debug( 'Search::Sequence::results: loading results page' )
    if $c->debug;

  # retrieve job details
  unless ( $c->forward( 'get_job_details', [ $job_id ] ) ) {
    $c->log->debug( 'Search::Sequence::results: problems getting job details' )
      if $c->debug;

    if ( $c->stash->{data}->{altoutput} || 0 ) {
      $c->log->debug( 'Search::Sequence::results: outputting to PfamaAlyzer; returning empty response' )
        if $c->debug;
      $c->res->content_type( 'text/plain' );
      $c->res->body( '' );
    }
    else {
      $c->stash->{seqSearchError} = $c->stash->{searchError}
                                    || 'There was an unknown problem when retrieving your results.';
    }
  }

}

#-------------------------------------------------------------------------------

=head2 resultset : Local

Returns the HTML table containing the results of the specified job(s).

=cut

sub resultset : Local {
  my ( $this, $c, $arg ) = @_;

  # start by setting the template that we'll use to render error messages if 
  # the request asks for HTML. We'll reset the template name once we've made
  # sure that the job was successful
  $c->stash->{template} = 'pages/search/sequence/error.tt';
  
  # TODO fix up this template to make it return a simple error message

  # retrieve job details
  unless ( $c->forward( 'get_job_details', [ $arg ] ) ) {
    $c->log->debug( 'Search::Sequence::resultset: problems getting job details' )
      if $c->debug;

    if ( $c->stash->{data}->{altoutput} || 0 ) {
      $c->log->debug( 'Search::Sequence::resultset: outputting to PfamaAlyzer; returning empty response' )
        if $c->debug;
      $c->res->content_type( 'text/plain' );
      $c->res->body( '' );
    }
    else {
      $c->stash->{seqSearchError} = $c->stash->{searchError}
                                    || 'There was an unknown problem when retrieving your result set.';
    }

    return;
  }

  # save some typing...
  my $jobId = $c->stash->{jobId};
  my $jobs  = $c->stash->{results}->{$jobId};

  # make sure the job(s) actually exist(s); throw an error otherwise
  unless ( scalar @{ $jobs->{rows} } ) {
    $c->log->debug( 'Search::Sequence::resultset: job(s) not found' )
      if $c->debug;

    $c->stash->{seqSearchError} = "Job(s) $jobId not found";

    return;
  }
  
  $c->log->debug( 'Search::Sequence::resultset: got a valid job ID and retrieved results' )
    if $c->debug;

  #----------------------------------------

  foreach my $job ( @{ $jobs->{rows} } ) {
    my $status   = $job->status;
    my $job_type = $job->job_type;

    if ( $status eq 'PEND' or
         $status eq 'RUN' ) {
      $c->log->debug( 'Search::Sequence::resultset: one or more jobs (' 
                      . $job->id . ') is not yet complete' )
        if $c->debug;

      $c->res->status( 202 ); # 'Accepted'
      $c->res->body( $status );

      # we don't care what other jobs are doing right now. We know that we're
      # still waiting for this one, so just stop processing and return the status
      # message immediately
      return;
    }

    # we need to return these error status messages and codes manually, since
    # there are no helpers for them in C::C::REST

    # check for a failure
    if ( $status eq 'FAIL' ) {
      $c->log->debug( 'Search::Sequence::resultset: job failed in the search system' )
        if $c->debug;

      $c->res->status( '502' ); # 'Bad gateway'
      $c->stash->{rest} = { error => "Job $jobId ($job_type) failed to complete successfully" };

      return;
    }
    
    # check for the job being put on HOLD
    if ( $status eq 'HOLD' ) {
      $c->log->debug( 'Search::Sequence::resultset: job is on hold' )
        if $c->debug;

      $c->res->status( '503' ); # 'Service unavailable'
      $c->stash->{rest} = { error => "Job $jobId ($job_type) is on hold" }; 

      return;
    }
    
    # check for the job being flagged as deleted
    if ( $status eq 'DEL' ) {
      $c->log->debug( 'Search::Sequence::resultset: job has been deleted' )
        if $c->debug;

      $c->res->status( '410' ); # 'Gone'
      $c->stash->{rest} = { error => "Job $jobId ($job_type) has been deleted" }; 

      return;
    }
    
    # anything other than DONE probably means that the job failed to 
    # complete successfully
    if ( $status ne 'DONE' ) {
      $c->log->debug( 'Search::Sequence::resultset: job failed in a strange and unusual fashion' )
        if $c->debug;
      
      $c->res->status( '500' ); # 'Internal server error'
      $c->stash->{rest} = { error => "Job $jobId ($job_type) failed with an unknown error" };
      
      return;
    }
 
  } # end of "foreach my $job"

  #----------------------------------------
  
  # all parts of the job completed successfully !
  $c->log->debug( 'Search::Sequence::resultset: all jobs completed; setting template' )
    if $c->debug;

  # decide which template to use. All of these templates will handle showing 
  # an error message, in case something goes wrong when submitting the search
  if ( $c->stash->{output_xml} ) {
    $c->log->debug( 'Search::Sequence::results: returning result set as XML' )
      if $c->debug;
    $c->stash->{template} = 'rest/search/results_xml.tt';
  }
  elsif ( $c->stash->{data}->{altoutput} || 0 ) {
    $c->log->debug( 'Search::Sequence::results: returning result set for PfamAlyzer' )
      if $c->debug;
    $c->stash->{template} = 'rest/search/pfamalyzer_results.tt';
  }
  else {
    $c->log->debug( 'Search::Sequence::results: returning result set as HTML' )
      if $c->debug;
    $c->stash->{template} = 'pages/search/sequence/results_table.tt';

    # push the config setting for the maximum desc string length into the stash
    $c->stash->{max_desc_length} = $this->{max_desc_length};
  }

  # parse the results
  $c->forward( 'handle_results' );

  # build the domain graphics description
  $c->forward( 'layout_dg' ) unless $c->stash->{data}->{altoutput};

  # put a reference to the results data structure in the "rest" slot in the 
  # stash, which is where the serialisers will be looking for it
  $c->stash->{rest} = $c->stash->{results}->{$jobId}->{hits};
  # TODO figure out where this should really be...
}

#-------------------------------------------------------------------------------

=head2 graphic : Local

Returns the raw JSON string describing the domain graphic for the specified job
result set.

=cut

sub graphic : Local {
  my ( $this, $c, $arg ) = @_;

  # retrieve job details
  unless ( $c->forward( 'get_job_details', [ $arg ] ) ) {
    $c->log->debug( 'Search::Sequence::graphic: problems getting job details' )
      if $c->debug;

    $c->res->status( 404 ); # "Not found"
    $c->res->body( 'There is no search job with that ID' );

    return;
  }

  # retrieve results
  $c->forward( 'handle_results' );

  # build the domain graphics description
  $c->forward( 'layout_dg' );

  # make sure we got something...
  unless ( $c->stash->{dg_layout} ) {
    $c->res->status( 500 ); # "Not found"
    $c->res->body( 'Failed to build domain graphics description' );
    return;
  }

  $c->log->debug( 'Search::Sequence::graphic: layout: ' . $c->stash->{dg_layout} )
    if $c->debug;

  $c->res->content_type( 'application/json' );
  $c->res->body( $c->stash->{dg_layout} );
}

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_job_details : Private

Given a job ID, this action checks it looks sensible and then tries to retrieve
the details of that job from the job tracking tables. Stashes the job ID and, if
found, the job details.

=cut

sub get_job_details : Private {
  my ( $this, $c, $arg ) = @_;

  # get hold of the job ID
  my $jobId = $c->req->param('jobId') ||
              $arg                    ||
              '';

  # make sure we have a job ID
  unless ( $jobId ) {
    $c->log->debug( 'Search::Sequence::get_job_details: no job ID' )
      if $c->debug;

    $c->stash->{searchError} = 'You did not supply a job ID';

    return 0;
  }
  
  # make sure it's valid
  unless ( $jobId =~ s/^([A-F0-9\-]{36})$/$1/i ) {
    $c->log->debug( 'Search::Sequence::get_job_details: bad job ID' )
      if $c->debug;

    $c->stash->{searchError} = 'You did not supply a valid job ID';

    return 0;
  }
  
  # we got an ID and it's valid; stash it and try to retrieve results
  $c->stash->{jobId} = $jobId;
  
  $c->log->debug( "Search::Sequence::get_job_details: checking job ID |$jobId|" )
    if $c->debug;

  # get the raw database objects for the results
  $c->forward( 'JobManager', 'retrieve_result_rows', [ $jobId  ] );

  #   
  $c->forward( 'handle_options' );

  return 1;
}

#-------------------------------------------------------------------------------

=head2 validate_input : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "searchError". 

=cut

sub validate_input : Private {
  my ( $this, $c ) = @_;
  
  # parse and validate the sequence itself
  unless ( $c->forward('parse_sequence') ) {

    # the parse_sequence method will put the sequence into the stash if it
    # passes validation, but if the sequence looks like DNA or if it's
    # too long, we also get an error message in the stash. So, we only set 
    # a general error message here if we don't already have one
    $c->stash->{searchError} ||= 'Invalid sequence. Please try again with a valid amino-acid sequence.';

    $c->log->debug( 'Search::Sequence::validate_input: sequence parsing failed' )
      if $c->debug;

    return 0;
  }

  # somewhere to stash the user options
  $c->stash->{user_options} = {};
  
  # should we use the gathering threshold ?
  if ( defined $c->req->param('ga') and 
       $c->req->param('ga') ) {
    $c->log->debug( 'Search::Sequence::validate_input: using ga' )
      if $c->debug;
    $c->stash->{user_options}->{ga} = 1;
  }

  # or should we use an E-value ?
  elsif ( defined $c->req->param('evalue') ) {
    $c->log->debug( 'Search::Sequence::sequence_search: got an evalue' )
      if $c->debug;

    # firstly, it has to be a number
    unless ( looks_like_number( $c->req->param('evalue') ) ) {
      $c->stash->{searchError} = 'The E-value must be a valid positive number <= 10.0.';

      $c->log->debug( 'Search::Sequence::validate_input: bad evalue (NaN); returning to form' )
        if $c->debug;

      return 0;
    }
    
    # secondly, it has to be positive...
    unless ( $c->req->param('evalue') > 0 ) {
      $c->stash->{searchError} = 'The E-value must be a positive number. '
                                 . 'Negative E-values values are meaningless.';

      $c->log->debug( 'Search::Sequence::validate_input: bad evalue (-ve); returning to form' )
        if $c->debug;

      return 0;
    }

    # thirdly and finally, it has to be less than 10.0
    unless ( $c->req->param('evalue') <= 10.0 ) {
      $c->stash->{searchError} = 'The E-value must be <= 10.0. Large E-values '
                                 . 'result in large numbers of meaningless Pfam '
                                 . 'hits and cause severe problems for our '
                                 . 'search system.';

      $c->log->debug( 'Search::Sequence::validate_input: bad evalue ( > 10.0 ); returning to form' )
        if $c->debug;

      return 0;
    }

    $c->log->debug( 'Search::Sequence::validate_input: evalue looks like a positive number <= 10.0; stashing' )
      if $c->debug;
    $c->stash->{user_options}->{evalue} = $c->req->param('evalue');

  }
  
  # search for Pfam-Bs ?
  $c->stash->{user_options}->{searchBs} = ( defined $c->req->param('searchBs') and
                                            $c->req->param('searchBs') );

  # should we search for Pfam-As or just skip them and search only Pfam-Bs ?
  if ( defined $c->req->param('skipAs') and
       $c->req->param('skipAs') ) {
    
    $c->log->debug( 'Search::Sequence::validate_input: skipping Pfam-A search' )
      if $c->debug;
    
    # flag up the fact that we want to skip Pfam-A searches
    $c->stash->{user_options}->{skipAs} = 1;
    
    # and force a search for Pfam-Bs
    $c->stash->{user_options}->{searchBs} = 1;
  }

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
  unless ( $c->stash->{data}->{seq} and $c->stash->{data}->{seq} ne '' ) {
    $c->stash->{searchError} = 'You did not supply an amino-acid sequence.';

    $c->log->debug( 'Search::Sequence::parse_sequence: no sequence supplied; failed' )
      if $c->debug;

    return 0;
  }
  
  # break the string into individual lines and get rid of any FASTA header lines
  # before recombining
  # my @seqs = split /\n/, $c->req->param('seq');
  my @seqs = split /\n/, $c->stash->{data}->{seq};
  shift @seqs if $seqs[0] =~ /^\>/;
  my $seq = uc( join '', @seqs );

  # handle various line endings. No need to worry about \n, since we got rid of
  # that with the "split" above
  $seq =~ s/[\s\r\d]+//g;

  # check the length of the sequence at this point. If it's too long, bail
  my $length = length $seq;
  if ( $length > $this->{maxSeqLength} ) {
    $c->stash->{searchError} = 
      'Your sequence is too long. The maximum length of search sequences is ' .
      $this->{maxSeqLength} . '. Please try again with a shorter sequence';

    $c->log->debug( 'Search::Sequence::parse_sequence: sequence is too long; failed' )
      if $c->debug;

    return 0;
  }

  # check that the sequence string contains only letters. Bail if it has 
  # anything else in it
  if ( $seq =~ m/[^ABCDEFGHIJKLMNOPQRSTUVWXYZ\*]/g ) {
    $c->stash->{searchError} = 'Invalid sequence; illegal character at position ' 
      . pos($seq) . ' (&quot;' . substr( $seq, pos($seq) - 1, 1 ) 
      . '&quot;). Please try again with a valid amino-acid sequence';

    $c->log->debug( 'Search::Sequence::parse_sequence: sequence contains illegal characters; failed' )
      if $c->debug;

    return 0;
  }

  # we need to make sure that the sequence is really protein and not, as we
  # commonly get, a bloody great DNA sequence. Count the number of potential 
  # nucleotides in the sequence and see what proportion of the total sequence
  # that makes
  my ( $nucleotide_count ) = $seq =~ tr/ATCGU/ATCGU/;
  
  # if the sequence is more than 100 residues (or bases) and is more than
  # 95% nucleotides, there's a problem
  if ( $length > 100 and $nucleotide_count / $length > 0.95 ) {
    $c->stash->{searchError} = 
      'Your sequence does not look like protein. Please upload a protein sequence';

    $c->log->debug( "Search::Sequence::parse_sequence: sequence doesn't look like protein; failed" )
      if $c->debug;

    return 0;
  }

  # passed all checks; stuff the sequence into the stash
  $c->stash->{input} = $seq;
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_seq_search : Private

Executes a protein sequence search. Queues a Pfam A search to one queue
and, if the appropriate box was checked in the submission form, a Pfam B search
to another queue.

=cut

sub queue_seq_search : Private {
  my ( $this, $c ) = @_;
  
  # first, check there's room on the queue
  my $rs = $c->model( 'WebUser::JobHistory' )
             ->search( { -and => [ { status => 'PEND' },
                                    -or => [ { job_type => 'A' },
                                             { job_type => 'B' } ] ] },
                       { select => [ { count => 'status' } ],
                         as     => [ 'numberPending' ] } )
             ->single;
  
  $c->stash->{number_pending} = $rs->get_column( 'numberPending' );
  
  $c->log->debug( 'Search::Sequence::queue_seq_search: |' . 
                  $c->stash->{number_pending} . '| jobs pending' ) if $c->debug;
  
  if ( $c->stash->{number_pending} >= $this->{pendingLimit} ) {
    $c->stash->{searchError} = 
      'There are currently too many jobs in the sequence search queue. ' . 
      'Please try again in a little while.';

    # TODO send an email to the admins about the full queue

    $c->log->debug( 'Search::Sequence::queue_seq_search: too many Pfam jobs in queue ('
                    . $c->stash->{number_pending} . ')' ) if $c->debug;

    return 0;
  }
  
  # ok. There's room on the queue, so we can submit the jobs. Generate a UUID for the job
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  $c->log->debug( 'Search::Sequence::queue_seq_search: generated job ID: |'
                  . $c->stash->{jobId} . '|' ) if $c->debug;
  
  #----------------------------------------

  # keep track of queued jobs
  my $queued = 0;

  # submit a Pfam-A job, unless we've been asked to skip it  
  if ( not $c->stash->{user_options}->{skipAs} ) {

    # set the job type
    $c->stash->{job_type} = 'A';

    # convert the options hash into JSON
    $c->stash->{options} = to_json( $c->stash->{user_options} );
    
    # submit the search and make sure that operation was a success
    unless ( $c->forward( 'queue_search_transaction' ) ) {
      $c->stash->{searchError} ||= 'There was a problem queuing your Pfam-A search.';

      $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-A search' )
        if $c->debug;

      return 0;
    } 

    $c->log->debug( 'Search::Sequence::queue_seq_search: successfully queued Pfam-A search' )
      if $c->debug;

    $queued++;
  } 
  
  #----------------------------------------

  # submit a Pfam-B job, if we've been asked to run one
  if ( $c->stash->{user_options}->{searchBs} ) {
    
    # set the job type
    $c->stash->{job_type} = 'B';

    # for Pfam-Bs, there are no options.
    $c->stash->{options} = '{}';
    
    # submit the search and make sure that operation was a success
    unless ( $c->forward( 'queue_search_transaction' ) ) {
      $c->stash->{searchError} ||= 'There was a problem queuing your Pfam-B search.';

      $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-B search' )
        if $c->debug;

      return 0;
    } 

    $c->log->debug( 'Search::Sequence::queue_seq_search: successfully queued Pfam-B search' )
      if $c->debug;

    $queued++;
  }
  
  #----------------------------------------

  # make sure we have at least one job...
  unless ( $queued ) {
    $c->stash->{searchError} = 'You must run at least one type of search.';
    
    $c->log->warn( 'Search::Sequence::queue_seq_search: no searches submitted' )
      if $c->debug;
      
    return 0;
  }
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 handle_results : Private

Parse the results and filter based on the the users defined parameters. The 
parsed results are put in a very generic format so that they can then be used 
for generating the results tables and graphics.

=cut

sub handle_results : Private {
  my ( $this, $c ) = @_;

  my $jobs = $c->stash->{results}->{ $c->stash->{jobId} };

  foreach my $job ( @{ $jobs->{rows} } ) {
    my $job_type = $job->job_type;

    my $results;
    eval {
      $results = thaw( $job->stdout );
    };
    if ( $@ ) {
      die "error retrieving Pfam-$job_type results: $@";
    }
    
    $c->log->debug( 'Search::Sequence::handle_results: got ' . scalar @$results 
                    . " Pfam-$job_type results" )
      if $c->debug;
    
    $jobs->{hits}->{$job_type} = $results;

		$c->log->debug( 'Search::Sequence::handle_results: ' . dump( $results ) )
			if $c->debug;

  } # end of "foreach my $job"

  $c->log->debug( 'Search::Sequence::handle_results: stashed hits for '
                  . scalar( keys %{ $jobs->{hits} } ) . ' jobs' )
    if $c->debug;
    
  # we also need to add the options to the stash, so that we can render
  # them in the template that builds the results page
  $c->forward( 'handle_options' );
}

#-------------------------------------------------------------------------------

=head2 handle_options: Private

Retrieves the job options for the specified job and stashes them. Note that the
options are extracted only from the Pfam-A search, since the Pfam-B search
(currently) has no user-specified options.

=cut

sub handle_options : Private {
  my ( $this, $c ) = @_;

  foreach my $job ( @{ $c->stash->{results}->{ $c->stash->{jobId} }->{rows} } ) {

    # options are only retrieved from Pfam-A jobs
    next unless $job->job_type eq 'A';

    # stash the options as a JSON string, straight from the database. We also
    # convert the JSON string back into perl and stash that, just to make life
    # easier in the template
    $c->stash->{json_options} = $job->options;
    $c->stash->{options}      = from_json( $c->stash->{json_options} );

    last;
  }

}

#-------------------------------------------------------------------------------

=head2 layout_dg : Private

Generates a JSON description of the domain graphics for the given results.

=cut

sub layout_dg : Private {
  my ( $this, $c ) = @_;

  my $jobs = $c->stash->{results}->{ $c->stash->{jobId} };

  my ( @regions, @motifs, @markups, $seq );
  foreach my $job ( @{ $jobs->{rows} } ) {
    my $job_type = $job->job_type;

    $seq = $job->stdin if not defined $seq;

    HIT: foreach my $hit ( @{ $jobs->{hits}->{$job_type} } ) {
      # $c->log->debug( 'Search::Sequence::handle_results: hit: '
      #                 . dump( $hit ) ) if $c->debug;

      # only add significant hits to the domain graphic
      next unless $hit->{sig};

      if ( $job_type eq 'A' ) {
        # $c->log->debug( 'Search::Sequence::layout_dg: adding Pfam-A region' )
        #   if $c->debug;

        push @regions, new Bio::Pfam::Sequence::Region( {
          start       => $hit->{env}->{from},
          end         => $hit->{env}->{to},
          aliStart    => $hit->{seq}->{from},
          aliEnd      => $hit->{seq}->{to},
          modelStart  => $hit->{hmm}->{from},
          modelEnd    => $hit->{hmm}->{to},
          modelLength => $hit->{model_length},
          type        => 'pfama',
          metadata    => new Bio::Pfam::Sequence::MetaData( {
            accession   => $hit->{acc},
            identifier  => $hit->{name},
            type        => $hit->{type},
            description => $hit->{desc},
            score       => $hit->{evalue},
            scoreName   => 'e-value',
            start       => $hit->{env}->{from},
            end         => $hit->{env}->{to},
            aliStart    => $hit->{seq}->{from},
            aliEnd      => $hit->{seq}->{to},
            database    => 'pfam'
          } )
        } );

      }
      elsif ( $job_type eq 'B' ) {
        # $c->log->debug( 'Search::Sequence::layout_dg: adding Pfam-B motif' )
        #   if $c->debug;

        push @motifs, new Bio::Pfam::Sequence::Motif( {
          start       => $hit->{env}->{from},
          end         => $hit->{env}->{to},
          modelStart  => $hit->{hmm}->{from},
          modelEnd    => $hit->{hmm}->{to},
          modelLength => $hit->{model_length},
          type        => 'pfamb',
          metadata    => new Bio::Pfam::Sequence::MetaData( {
            accession   => $hit->{acc},
            identifier  => $hit->{name},
            type        => $hit->{type} || 'Pfam-B',
            description => $hit->{desc} || '',
            score       => $hit->{evalue},
            scoreName   => 'e-value',
            start       => $hit->{env}->{from},
            end         => $hit->{env}->{to},
            database    => 'pfam'
          } )
        } );

      }
      else {
        $c->log->debug( "Search::Sequence::layout_dg: unknown job_type: |$job_type|" )
          if $c->debug;
        next HIT;
      }

      if ( $hit->{act_site} ) {
        # $c->log->debug( 'Search::Sequence::layout_dg: adding active sites' )
        #   if $c->debug;

        foreach my $site_residue ( @{ $hit->{act_site} } ) {
          push @markups, new Bio::Pfam::Sequence::Markup( {
            start    => $site_residue,
            residue  => substr( $seq, $site_residue - 1, 1 ),
            type     => 'Pfam predicted active site',
            metadata => Bio::Pfam::Sequence::MetaData->new( {
              start       => $site_residue,
              type        => 'Pfam predicted active site',
              description => 'Residue ' . substr( $seq, $site_residue - 1, 1 ) . $site_residue,
              database    => 'pfam'
            } )
          } );
        }

      }

    } # end of "foreach hit"

  } # end of "foreach job"

  my $sequence = new Bio::Pfam::Sequence( {
    length  => length( $seq ),
    regions => \@regions,
    motifs  => \@motifs,
    markups => \@markups 
  } );
  # $c->log->debug( 'Search::Sequence::layout_dg: sequence object: '
  #                 . dump( $sequence ) ) if $c->debug;

  my $lm = new Bio::Pfam::Drawing::Layout::LayoutManager;
  my $sequences = [ $sequence ];
  $lm->layoutSequences( $sequences );

  my $json = new JSON;
  # $json->pretty( 1 );
  $json->allow_blessed;
  $json->convert_blessed;

  $c->stash->{dg_layout} = $json->encode( $sequences );
  # $c->log->debug( "Search::Sequence::layout_dg: JSON sequence object: |$json_layout|" ) 
  #   if $c->debug;

} # end of "sub layout_dg"

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