
# Sequence.pm
# jt6 20061108 WTSI
#
# $Id: Sequence.pm,v 1.33 2009-08-06 13:15:20 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Sequence - perform sequence searches

=cut

package PfamWeb::Controller::Search::Sequence;

=head1 DESCRIPTION

This controller is responsible for running sequence searches.

$Id: Sequence.pm,v 1.33 2009-08-06 13:15:20 jt6 Exp $

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

use base qw( PfamBase::Controller::Search::InteractiveSearch
             PfamWeb::Controller::Search );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 search : Path

Queues a sequence search job and redirects to a page that polls the server for
results. 

We redirect to a page so that we can convert the POST that gets us here into 
a GET that can be bookmarked.

=cut

sub search : Path {
  my ( $this, $c ) = @_;

  # validate the input
  unless ( $c->forward('validate_input') ) {

    # copy the error message into the slot in the stash where the templates
    # expect to find it
    $c->stash->{seqSearchError} = $c->stash->{searchError}
      || 'There was an unknown problem when validating your sequence.';

    # if we're returning XML, we need to set a template to render the error
    # message. If we're emitting HTML, the end action (ultimately on Section)
    # will take of us and return us to the HTML page containing search form
    # and show the error message
    if ( $c->stash->{output_xml} ) {
      $c->stash->{template} = 'rest/search/error_xml.tt';
      $c->res->content_type('text/xml');
    }

    return;
  }

  # no errors with the input; try to submit the search

  # success !
  if ( $c->forward('queue_seq_search') ) {

    $c->log->debug( 'Search::Sequence::search: redirecting to results page' )
      if $c->debug;

    if ( $c->req->param('output') eq 'xml' ) {
      $c->stash->{template} = 'rest/search/poll_xml.tt';
      $c->res->status(202); # 'Accepted'
      $c->res->content_type('text/xml');
    }
    elsif ( $c->req->param('output') eq 'yaml' or
            $c->req->param('output') eq 'json' ) {
              
      my $data_type = $c->req->param('output');

      $c->res->status(202); # 'Accepted'

      my $data = {};
      foreach my $job_id ( @{ $c->stash->{queued_jobs} } ) {
        $data->{$job_id} = $c->uri_for( '/search/sequence/resultset', $job_id,
                                        { output => $data_type } )->as_string; 
      }

      if ( $data_type eq 'yaml' ) {
        require YAML;
        $c->res->content_type('text/yaml');
        $c->res->body( Dump( $data ) );
      }
      elsif (  $data_type eq 'json' ) {
        # require JSON;
        $c->res->content_type('text/json');
        $c->res->body( to_json( $data ) );
      }
    }
    else {
      $c->res->redirect( $c->uri_for( 'results', 
                                      { jobId => $c->stash->{queued_jobs} } ) );
    }

  }

  # failure...
  else {

    $c->stash->{seqSearchError} = $c->stash->{searchError}
      || 'There was an unknown problem when submitting your search.';

    $c->log->debug(
      'Search::Sequence::search: problem with submission; re-rendering form')
      if $c->debug;

    # point to the XML error template if emitting XML, otherwise, we're just
    # done here
#    if ( $c->stash->{output_xml} ) {
#      $c->stash->{template} = 'rest/search/error_xml.tt';
#      $c->res->content_type('text/xml');
#    }

  }

}

#   http://onlamp.com/pub/a/onlamp/2008/02/19/developing-restful-web-services-in-perl.html?page=2

#-------------------------------------------------------------------------------

=head2 results : Local

Builds a page that will hold the results of the search(es). 

=cut

sub results : Local {
  my ( $this, $c, @args ) = @_;

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

  foreach my $job_id ( @ids ) {
    
    # we shouldn't really need to check the IDs here, but since this is a
    # separate request, it could be hit directly. We're not going to bother
    # with a full error message though, since an error here is the result of
    # someone farting around... 
       
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
  
  $c->stash->{template} = 'pages/search/sequence/results.tt';  
}

#-------------------------------------------------------------------------------

=head2 resultset : Local

Returns the HTML table containing the results of the specified job(s).

=cut

sub resultset : Local {
  my ( $this, $c, $arg ) = @_;

  # make sure we have a job ID
  my $job_id;
  unless ( $job_id = $c->req->param('jobId') || $arg ) {
    $c->log->debug( 'Search::Sequence::resultset: no job IDs' )
      if $c->debug;
    $c->res->status( '400' ); # 'Bad Request'
    $c->res->body( 'You did not supply any job IDs' );
    return;
  }
  
  # make sure it's valid
  unless ( $job_id =~ s/^([A-F0-9\-]{36})$/$1/i ) {
    $c->log->debug( 'Search::Sequence::resultset: bad job ID' )
      if $c->debug;
    $c->res->status( '400' ); # 'Bad Request'
    $c->res->body( 'You did not supply a valid job ID' );
    return;
  }
  
  #----------------------------------------
  
  # we got an ID and it's valid; try to retrieve results
  $c->log->debug( "Search::Sequence::resultset: checking job ID |$job_id|" )
    if $c->debug;

  $c->forward( 'JobManager', 'retrieveResults', [ $job_id  ] );

  # get the raw database row and check the status
  my $job = $c->stash->{results}->{$job_id}->{job};

  # first, make sure the job actually exists
  unless ( defined $job  ) {
    $c->log->debug( 'Search::Sequence::resultset: job not found' )
      if $c->debug;
    $c->res->status( '404' ); # 'No content'
    $c->res->body( "Job $job_id not found" );
    return;
  }

  # next check the status
  my $status = $job->status;
  $c->log->debug( "Search::Sequence::resultset: job status: |$status|" )
    if $c->debug;

  # check explicitly for PEND or RUN
  if ( $status eq 'PEND' or $status eq 'RUN' ) {
    $c->log->debug( 'Search::Sequence::resultset: job not yet complete' )
      if $c->debug;
    $c->res->status( '202' ); # 'Accepted'
    $c->res->body( "Job $job_id not yet complete" );
    return;
  }

  # check for a failure
  if ( $status eq 'FAIL' ) {
    $c->log->debug( 'Search::Sequence::resultset: job failed in the search system' )
      if $c->debug;
    $c->res->status( '502' ); # 'Bad gateway'
    $c->res->body( "Job $job_id failed to complete successfully" ); 
    return;
  }
  
  # check for the job being put on HOLD
  if ( $status eq 'HOLD' ) {
    $c->log->debug( 'Search::Sequence::resultset: job is on hold' )
      if $c->debug;
    $c->res->status( '503' ); # 'Service unavailable'
    $c->res->body( "Job $job_id is on hold" ); 
    return;
  }
  
  # check for the job being deleted
  if ( $status eq 'DEL' ) {
    $c->log->debug( 'Search::Sequence::resultset: job has been deleted' )
      if $c->debug;
    $c->res->status( '410' ); # 'Gone'
    $c->res->body( "Job $job_id has been deleted from the search system" ); 
    return;
  }
  
  # Anything other than DONE probably means that the job failed to 
  # complete successfully
  if ( $status eq 'DEL' ) {
    $c->log->debug( 'Search::Sequence::resultset: job failed in a strange and unusual fashion' )
      if $c->debug;
    $c->res->status( '500' ); # 'Internal server error'
    $c->res->body( "Job $job_id failed with an unknown error" );
    return;
  }
 
#    $c->stash->{template} = 'components/blocks/search/error.tt';

  #----------------------------------------
  
  # job completed successfully !

  # parse the results
  $c->forward( 'handle_results', [ $job ] );

  # should we output XML ?
  if ( $c->stash->{output_xml} ) {
    $c->stash->{template} = 'rest/search/results_xml.tt';
    $c->res->content_type('text/xml');    
  }
  
  # no; render the HTML template
  else {
    $c->stash->{template} = 'pages/search/sequence/results_table.tt';
  }

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
  unless ( defined $c->req->param('seq') and
           $c->req->param('seq') ne '' ) {
    $c->stash->{searchError} = 'You did not supply an amino-acid sequence.';

    $c->log->debug( 'Search::Sequence::parse_sequence: no sequence supplied; failed' )
      if $c->debug;

    return 0;
  }
  
  # break the string into individual lines and get rid of any FASTA header lines
  # before recombining
  my @seqs = split /\n/, $c->req->param('seq');
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

__DATA__
[
  {
    acc      => "PF02600",
    act_site => undef,
    align    => [
                  "#HMM       vlglePCpLCi",
                  "#MATCH     ++glePC++Ci",
                  "#PP        689*******8",
                  "#SEQ       LRGLEPCAICI",
                ],
    bits     => "0.6",
    clan     => "No_clan",
    env      => { from => 1079, to => 1093 },
    evalue   => "3e+02",
    hmm      => { from => 28, to => 38 },
    name     => "DsbB",
    seq      => { from => 1082, name => "UserSeq", to => 1092 },
  },
  {
    acc      => "PF00168",
    act_site => undef,
    align    => [
                  "#HMM       evtvieAknLpkkdkngksdpyvkvklggekk..qkkkTkvvkkt.lnPvWn.etfefevseeelqeleieVydkdrlgkddflGev",
                  "#MATCH     +++v++A++Lpk +++g+++p+v+++++g++++++k+kT++v++++lnPvW++++f+f++s++e+++l+++Vy++d++++++fl+++",
                  "#PP        589********9.7799************9999889****999999******999***************************99987",
                  "#SEQ       CIEVLGARHLPK-NGRGIVCPFVEIEVAGAEYdsIKQKTEFVVDNgLNPVWPaKPFHFQISNPEFAFLRFVVYEEDMFSDQNFLAQA",
                ],
    bits     => "49.3",
    clan     => "CL0154",
    env      => { from => 1090, to => 1177 },
    evalue   => "2.3e-13",
    hmm      => { from => 2, to => 84 },
    name     => "C2",
    seq      => { from => 1091, name => "UserSeq", to => 1176 },
  },
  {
    acc      => "PF06054",
    act_site => undef,
    align    => [
                  "#HMM       iEiQcsklsikelkerTegykreglkvlW",
                  "#MATCH     +Ei++++++++++k++Te+++++gl+++W",
                  "#PP        89***************************",
                  "#SEQ       VEIEVAGAEYDSIKQKTEFVVDNGLNPVW",
                ],
    bits     => "0.9",
    clan     => "CL0236",
    env      => { from => 1105, to => 1153 },
    evalue   => "1.5e+02",
    hmm      => { from => 106, to => 134 },
    name     => "CoiA",
    seq      => { from => 1112, name => "UserSeq", to => 1140 },
  },
]

sub old_validate_input : Private {
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
  
  # search for Pfam-Bs ?
  $c->stash->{searchBs} = ( defined $c->req->param('searchBs') and
                            $c->req->param('searchBs') );

  # should we search for Pfam-As or just skip them and search only Pfam-Bs ?
  if ( defined $c->req->param('skipAs') and
       $c->req->param('skipAs') ) {
    
    $c->log->debug( 'Search::Sequence::sequence_search: skipping Pfam-A search' )
      if $c->debug;
    
    # flag up the fact that we want to skip Pfam-A searches
    $c->stash->{skipAs} = 1;
    
    # and force a search for Pfam-Bs
    $c->stash->{searchBs} = 1;
    
    # no need to check the remaining parameters, since they apply only to
    # Pfam-A searches and we're not actually doing a Pfam-A search...
    return 1;
    
  }
    
  # available sequence search options
  my %available_options = ( both        => 1,
                            bothNoMerge => 1,
                            ls          => 1,
                            fs          => 1 );

  # somewhere to stash the search options
  $c->stash->{options} = {};

  # the user supplied an option; check it's valid
  if ( defined $c->req->param('seqOpts') ) {
    
    unless ( defined $available_options{ $c->req->param('seqOpts') } ) {
      $c->stash->{searchError} = 'You must use a valid search option.';

      $c->log->debug( 'Search::Sequence::validate_inputh: bad search option; returning to form' )
        if $c->debug;

      return 0;
    }
    
    $c->stash->{options}->{seqOpts} = $c->req->param('seqOpts');
  }

  # default to "both"
  else {
    $c->stash->{options}->{seqOpts} = 'both';

    $c->log->debug( 'Search::Sequence::validate_input: setting search option to default of "both"' )
      if $c->debug;
  }

  # if we're supplied with an E-value, we'll use that, unless we've been asked 
  # to use the gathering threshold. Default to using an evalue of 1.0
  if ( defined $c->req->param('evalue') ) {
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
    $c->stash->{options}->{evalue} = $c->req->param('evalue');

  }
  elsif ( defined $c->req->param('ga') and 
          $c->req->param('ga') ) {
    $c->log->debug( 'Search::Sequence::validate_input: using ga' )
      if $c->debug;
    $c->stash->{options}->{ga} = 1;
    
  }
  else {
    $c->log->debug( 'Search::Sequence::validate_inputh: using default evalue' )
      if $c->debug;
    $c->stash->{options}->{evalue} = 1.0;
  }

  # convert the options hash into JSON
  $c->stash->{options} = to_json( $c->stash->{options} );

  return 1;  
}

