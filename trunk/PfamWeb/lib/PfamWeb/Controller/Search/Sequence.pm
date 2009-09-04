
# Sequence.pm
# jt6 20061108 WTSI
#
# $Id: Sequence.pm,v 1.34 2009-09-04 09:53:39 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Sequence - perform sequence searches

=cut

package PfamWeb::Controller::Search::Sequence;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: Sequence.pm,v 1.34 2009-09-04 09:53:39 jt6 Exp $

=cut

use strict;
use warnings;

use Bio::SearchIO::blast;
use File::Temp qw( tempfile );
use JSON;
use YAML qw( Dump );
use Scalar::Util qw( looks_like_number );
use Data::UUID;
use Data::Dump qw( dump );
use Storable qw( thaw );

use base qw( Catalyst::Controller::REST
             PfamBase::Controller::Search::InteractiveSearch
             PfamWeb::Controller::Search );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Queues a sequence search job and redirects to a page that polls the server for
results. 

=cut

sub search : Path : ActionClass( 'REST' ) { }

#----------------------------------------

sub search_POST {
  my ( $this, $c ) = @_;

  # retrieve the job parameters from either the request or, if the request has
  # been deserialised for us, from the stash 
  $c->stash->{data} = {};
  foreach my $slot ( qw( seq ga evalue ) ) {
    $c->stash->{data}->{$slot} = $c->req->param($slot)
                                 || ( defined $c->req->data ? $c->req->data->{$slot} : '' );
  }
  
  # validate the input
  unless ( $c->forward('validate_input') ) {

    $this->status_bad_request(
      $c,
      message => $c->stash->{searchError}
                 || 'There was an unknown problem when validating your sequence.'
    );

    return;
  }

  # no errors with the input; try to submit the search

  if ( $c->forward('queue_seq_search') ) {

    # success !

    my $data = [];
    foreach my $job_id ( @{ $c->stash->{queued_jobs} } ) {
      push @$data, { jobId => $job_id,
                     uri   => $c->uri_for( '/search/sequence/resultset', $job_id )->as_string };
    }
    $c->stash->{rest} = $data;

    $this->status_accepted(
      $c,
      entity => $data
    );

    $c->forward( 'results', $c->stash->{queued_jobs} );

  }
  else {

    # failure

    $this->status_bad_request(
      $c,
      message => $c->stash->{searchError}
                 || 'There was an unknown problem when submitting your search.'
    );
    
  }

  $c->log->debug( 'Search::Sequence::search_POST: template set to ' . $c->stash->{template} )
    if $c->debug;
}

#   http://onlamp.com/pub/a/onlamp/2008/02/19/developing-restful-web-services-in-perl.html?page=2

#-------------------------------------------------------------------------------

=head2 results : Local

Builds a page that will hold the results of the search(es). 

=cut

sub results : Local {
  my ( $this, $c, @args ) = @_;

  $c->stash->{template} = 'pages/search/sequence/results.tt';  
  
  $c->log->debug( 'Search::Sequence::results: loading results page' )
    if $c->debug;

  # accept job IDs either as parameters or arguments
  my @ids;
  if ( $c->req->param('jobId') ) {
    $c->log->debug( 'Search::Sequence::results: getting job IDs from parameters' )
      if $c->debug;
    push @ids, $c->req->param('jobId');
  }
  elsif ( scalar @args ) {
    $c->log->debug( 'Search::Sequence::results: getting job IDs from arguments' )
      if $c->debug;
    push @ids, @args;
  }

  # make sure we got some job IDs
  unless ( scalar @ids ) {
    $c->stash->{seqSearchError} = 'You did not supply any job IDs.';

    $c->log->debug( 'Search::Sequence::results: no job IDs provided' )
      if $c->debug;

    return 0;
  }  

  # retrieve the results
  foreach my $job_id ( @ids ) {
    
    # check the job IDs first...
    unless ( $job_id =~ m/^[A-F0-9\-]{36}$/i ) {
      $c->stash->{seqSearchError} = 'Invalid job IDs';

      $c->log->debug( 'Search::Sequence::show_results: bad job id' )
        if $c->debug;

      return 0;
    }

    # the job ID is at least sensible

    # get the row of the tracking table for this job
    $c->forward( 'JobManager', 'retrieveResults', [ $job_id  ] );
    
    # we don't need to explicitly stash the job row objects, since the 
    # JobManager stuffs them into the stash itself, in:
    #   $c->stash->{results}->{$job_id}->{job}
    # we need the job objects so that the template that build the results 
    # page can keep track of the type of each job, amongst other things
  }
}

#-------------------------------------------------------------------------------

=head2 resultset : Local

Returns the HTML table containing the results of the specified job(s).

=cut

sub resultset : Local : ActionClass('REST') { }

#----------------------------------------

sub resultset_GET {
  my ( $this, $c, $arg ) = @_;

  # start by setting the template, which we'll use to render error messages if 
  # the request asks for HTML. We'll reset the template name once we've made
  # sure that the job was successful
  $c->stash->{template} = 'pages/search/sequence/error.tt';
  
  # TODO fix up this template to make it return a simple error message

  # get hold of the job ID
  my $job_id = $c->req->param('jobId') 
               || $arg
               || ( defined $c->req->data ? $c->req->data->{jobId} : '' );

  # make sure we have a job ID
  unless ( $job_id ) {
    $c->log->debug( 'Search::Sequence::resultset: no job IDs' )
      if $c->debug;

    $this->status_bad_request(
      $c,
      message => 'You did not supply any job IDs'
    );

    return;
  }
  
  # make sure it's valid
  unless ( $job_id =~ s/^([A-F0-9\-]{36})$/$1/i ) {
    $c->log->debug( 'Search::Sequence::resultset: bad job ID' )
      if $c->debug;

    $this->status_bad_request(
      $c,
      message => 'You did not supply a valid job ID'
    );

    return;
  }
  
  #----------------------------------------
  
  # we got an ID and it's valid; try to retrieve results
  $c->log->debug( "Search::Sequence::resultset: checking job ID |$job_id|" )
    if $c->debug;

  $c->forward( 'JobManager', 'retrieveResults', [ $job_id  ] );

  # get the raw database row and check the status
  my $job = $c->stash->{results}->{$job_id}->{job};

  # make sure the job actually exists; throw an error otherwise
  unless ( defined $job  ) {
    $c->log->debug( 'Search::Sequence::resultset: job not found' )
      if $c->debug;

    $this->status_not_found(
      $c,
      message => "Job $job_id not found"
    );

    return;
  }

  #----------------------------------------

  # next check the status
  my $status = $job->status;
  $c->log->debug( "Search::Sequence::resultset: job status: |$status|" )
    if $c->debug;

  # check explicitly for PEND or RUN
  if ( $status eq 'PEND' or $status eq 'RUN' ) {
    $c->log->debug( 'Search::Sequence::resultset: job not yet complete' )
      if $c->debug;

    $this->status_accepted(
      $c,
      entity => {
        status => $status
      }
    );

    return;
  }

  # we need to return these error status messages and codes manually, since
  # there are no helpers for them in C::C::REST

  # check for a failure
  if ( $status eq 'FAIL' ) {
    $c->log->debug( 'Search::Sequence::resultset: job failed in the search system' )
      if $c->debug;

    $c->res->status( '502' ); # 'Bad gateway'
    $c->stash->{rest} = { error => "Job $job_id failed to complete successfully" };

    return;
  }
  
  # check for the job being put on HOLD
  if ( $status eq 'HOLD' ) {
    $c->log->debug( 'Search::Sequence::resultset: job is on hold' )
      if $c->debug;

    $c->res->status( '503' ); # 'Service unavailable'
    $c->stash->{rest} = { error => "Job $job_id is on hold" }; 

    return;
  }
  
  # check for the job being deleted
  if ( $status eq 'DEL' ) {
    $c->log->debug( 'Search::Sequence::resultset: job has been deleted' )
      if $c->debug;

    $c->res->status( '410' ); # 'Gone'
    $c->stash->{rest} = { error => "Job $job_id has been deleted from the search system" }; 

    return;
  }
  
  # Anything other than DONE probably means that the job failed to 
  # complete successfully
  if ( $status eq 'DEL' ) {
    $c->log->debug( 'Search::Sequence::resultset: job failed in a strange and unusual fashion' )
      if $c->debug;
    
    $c->res->status( '500' ); # 'Internal server error'
    $c->stash->{rest} = { error => "Job $job_id failed with an unknown error" };
    
    return;
  }
 
  #----------------------------------------
  
  # job completed successfully !

  $c->stash->{template} = 'pages/search/sequence/results_table.tt';

  # parse the results
  $c->forward( 'handle_results', [ $job ] );

  # put a reference to the results data structure in the "rest" slot in the 
  # stash, which is where the serialisers will be looking for it
  $c->stash->{rest} = $c->stash->{results};

}

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
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
    
    $c->log->debug( 'Search::Sequence::sequence_search: skipping Pfam-A search' )
      if $c->debug;
    
    # flag up the fact that we want to skip Pfam-A searches
    $c->stash->{user_options}->{skipAs} = 1;
    
    # and force a search for Pfam-Bs
    $c->stash->{user_options}->{searchBs} = 1;
    
    # no need to check the remaining parameters, since they apply only to
    # Pfam-A searches and we're not actually doing a Pfam-A search...
    return 1;
    
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
  unless ( $seq =~ m/^[ABCDEFGHIKLMNPQRSTUVWXYZ\-\*]+\r?$/ ) {
    $c->stash->{seqSearchError} = 
      'Invalid sequence. Please try again with a valid amino-acid sequence';

    $c->log->debug( 'Search::Sequence::parse_sequence: sequence contains illegal characters; failed' )
      if $c->debug;

    return 0;
  }

  # we need to make sure that the sequence is really protein and not, as we
  # commonly get, a bloody great DNA sequence. Count the number of potential 
  # nucleotides in the sequence and see what proportion of the total sequence
  # that makes
  my ( $nucleotide_count )= $seq =~ tr/ATCGU/ATCGU/;
  
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
  
  $c->stash->{numberPending} = $rs->get_column( 'numberPending' );
  
  $c->log->debug( 'Search::Sequence::queue_seq_search: |' . 
                  $c->stash->{numberPending} . '| jobs pending' ) if $c->debug;
  
  if ( $c->stash->{numberPending} >= $this->{pendingLimit} ) {
    $c->stash->{searchError} = 
      'There are currently too many jobs in the sequence search queue. ' . 
      'Please try again in a little while.';

    $c->log->debug( 'Search::Sequence::queue_seq_search: too many Pfam jobs in queue ('
                    . $c->stash->{numberPending} . ')' ) if $c->debug;

    return 0;
  }
  
  # ok. There's room on the queue, so we can submit the jobs
  
  #----------------------------------------

  # submit a Pfam-A job, unless we've been asked to skip it  
  if ( not $c->stash->{user_options}->{skipAs} ) {

    # generate a job ID and set the job type
    $c->stash->{jobId}    = Data::UUID->new()->create_str();
    $c->stash->{job_type} = 'A';

    # convert the options hash into JSON
    $c->stash->{options} = to_json( $c->stash->{user_options} );
    
    # submit the search and make sure that operation was a success
    if ( $c->forward( 'queue_search_transaction' ) ) {

      $c->log->debug( 'Search::Sequence::queue_seq_search: successfully queued Pfam-A search' )
        if $c->debug;

      push @{ $c->stash->{queued_jobs} }, $c->stash->{jobId}; 
      
    }
    else {
      $c->stash->{searchError} ||= 'There was a problem queuing your Pfam-A search.';

      $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-A search' )
        if $c->debug;

      return 0;
    } 

  } 
  
  #----------------------------------------

  # submit a Pfam-B job, if we've been asked to run one
  if ( $c->stash->{user_options}->{searchBs} ) {
    
    # generate a job ID and set the job type
    $c->stash->{jobId}    = Data::UUID->new()->create_str();
    $c->stash->{job_type} = 'B';

    # convert the options hash into JSON; for Pfam-Bs, there are no options.
    # Set the "options" value to an empty hash/object
    $c->stash->{options} = '{}';
    
    # submit the search and make sure that operation was a success
    if ( $c->forward( 'queue_search_transaction' ) ) {

      $c->log->debug( 'Search::Sequence::queue_seq_search: successfully queued Pfam-B search' )
        if $c->debug;

      push @{ $c->stash->{queued_jobs} }, $c->stash->{jobId}; 
      
    }
    else {
      $c->stash->{searchError} ||= 'There was a problem queuing your Pfam-B search.';

      $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-B search' )
        if $c->debug;

      return 0;
    } 

  }
  
  #----------------------------------------

  # make sure we have at least one job...
  unless ( scalar @{ $c->stash->{queued_jobs} } ) {
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
  my ( $this, $c, $job ) = @_;

  # we're handed the row DBIC row object; retrieve the job ID and type
  my $job_id   = $job->job_id;
  my $job_type = $job->job_type;

  $c->log->debug( "Search::Sequence::handle_results: handling results for |$job_id|" )
    if $c->debug;
  
  #----------------------------------------
  
  if ( $job_type eq 'A' ) {
    $c->log->debug( "Search::Sequence::handle_results: job |$job_id| is a Pfam-A job" )
      if $c->debug;

    my $evalue_cutoff = $c->stash->{results}->{$job_id}->{user_options}->{evalue};
    my $ga            = $c->stash->{results}->{$job_id}->{user_options}->{ga};
    
    if ( defined $evalue_cutoff and not $ga ) {
      $c->log->debug( "Search::Sequence::handle_results: setting an E-value cutoff of $evalue_cutoff" )
        if $c->debug;
      $c->stash->{evalue_cutoff} = $evalue_cutoff;
    }
      
    my $results;
    eval {
      $results = thaw( $job->stdout );
    };
    if ( $@ ) {
      die "error retrieving Pfam-A results: $@";
    }
    
    $c->log->debug( 'Search::Sequence::handle_results: got '
                    . scalar @$results . ' Pfam-A results' ) if $c->debug;
    
    $c->stash->{jobId}    = $job_id;
    $c->stash->{job_type} = $job_type;
    $c->stash->{results}  = $results;
  }

  #----------------------------------------
  
  elsif ( $job_type eq 'B' ) {
    $c->log->debug( "Search::Sequence::handle_results: job |$job_id| is a Pfam-B job" )
      if $c->debug;

    my $results;
    eval {
      $results = thaw( $job->stdout );
    };
    if ( $@ ) {
      die "error retrieving Pfam-B results: $@";
    }
    
    $c->log->debug( 'Search::Sequence::handle_results: got '
                    . scalar @$results . ' Pfam-B results' ) if $c->debug;
    
    $c->stash->{jobId}    = $job_id;
    $c->stash->{job_type} = $job_type;
    $c->stash->{results}  = $results;
  }

  #----------------------------------------
  
  else {
    $c->log->error( "Search::Sequence::handle_results: job |$job_id| is of an unrecognised type: |$job_type|" )
      if $c->debug;
    die "error retrieving results for an unknown job_type"; 
  }

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
