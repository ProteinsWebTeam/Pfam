
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
use File::Temp;
use IPC::Run qw( start finish );

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

  # break the sequence into individual lines and get rid of any FASTA header lines
  # before recombining
  if ( $c->stash->{data}->{seq} ) {
    $c->log->debug( 'Search::Sequence::auto: stripping FASTA header line from sequence' )
      if $c->debug;
    my @seqs = split /\n/, $c->stash->{data}->{seq};
    shift @seqs if $seqs[0] =~ /^\>/;
    $c->stash->{data}->{seq} = uc( join '', @seqs );
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
  my $valid = $c->forward('validate_input');
  unless ( $valid ) {
    $c->stash->{seqSearchError} ||= 'There was an unknown problem when validating your sequence.';

    $c->log->debug( 'Search::Sequence::search: problem validating input' )
      if $c->debug;

    return;
  }

  # no errors with the input; try to submit the search
  $c->log->debug( 'Search::Sequence::search_POST: input is valid; queueing search' )
    if $c->debug;

  # handle DNA versus protein
  my $submitted = $c->stash->{sequence_type} eq 'dna'
                ? $c->forward( 'queue_dna_search' )
                : $c->forward( 'queue_protein_search' );

  unless ( $submitted ) {
    $c->stash->{seqSearchError} ||= 'There was an unknown problem when submitting your search.';

    $c->log->debug( 'Search::Sequence::search: problem submitting search' )
      if $c->debug;

    return;
  }

  # success !
  $c->log->debug( 'Search::Sequence::search: submission successful' )
    if $c->debug;

  # handle DNA versus protein
  if ( $c->stash->{sequence_type} eq 'dna' ) {
    $c->forward( 'PfamWeb::Controller::Search::Dna', 'results', [ $c->stash->{jobId} ] );
  }
  else {
    $c->forward( 'results', [ $c->stash->{jobId} ] );
  }
}

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
  elsif ( $c->stash->{data}->{altoutput} and
          $c->stash->{data}->{altoutput} == 1 ) {
    $c->log->debug( 'Search::Sequence::results: returning just the job ID for PfamAlyzer' )
      if $c->debug;
    $c->res->content_type( 'text/plain' );
    $c->res->body( $c->stash->{jobId} );
    return;
  }

  # don't set the template, just stash everything. For use by NCBISeq controller
  elsif ( $c->stash->{data}->{altoutput} and
          $c->stash->{data}->{altoutput} == 2 ) {
    $c->log->debug( 'Search::Sequence::results: stashing the job ID for NCBISeq, etc.' )
      if $c->debug;

    sleep(3);

    # because the other controller needs to set these values for itself but
    # they've already been set by the "search" method, we'll undef them here
    $c->stash->{pageType} = undef;
    $c->stash->{template} = undef;
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
      $c->stash->{seqSearchError} ||= 'There was an unknown problem when retrieving your results.';
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
      $c->stash->{seqSearchError} ||= 'There was an unknown problem when retrieving your result set.';
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

=head2 type : Local

Returns the type (alphabet) of the provided sequence.

=cut

sub type : Local {
  my ( $this, $c ) = @_;

  # if the sequence box is blank, as it will be when the form is reset, we
  # need to simply return "protein", which is the default state of the form
  unless ( $c->stash->{data}->{seq} ) {
    $c->res->content_type( 'text/plain' );
    $c->res->body( "protein" );
    return;
  }

  # now we know there's a sequence to work with, hand it off to esl-seqstat
  # to determine its alphabet
  my $rv = $c->forward('get_seq_type');

  if ( $rv < 0 ) {
    $c->res->status( 500 ); # Internal Server Error
    $c->res->body( $c->stash->{seqSearchError} );
  }
  elsif ( $rv == 0 ) {
    $c->res->status( 400 ); # Bad Request
    $c->res->body( $c->stash->{seqSearchError} );
  }
  else {
    $c->res->content_type( 'text/plain' );
    $c->res->body( $c->stash->{sequence_type} );
  }
}

#-------------------------------------------------------------------------------
#- private actions--------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_seq_type : Private

Uses the easel tool C<esl-seqstat> to try to guess the alphabet in the input
sequence. Returns "amino" if it's a protein sequence, "dna" if it's DNA, undef
otherwise. If there's a problem running C<esl-seqstat> or if the alphabet can't
be guessed, the stash value "seqSearchError" is populated with an error message.
Uses only first 500 characters of the sequence. Surely, if we can't guess it
from that, we're not guessing it at all, right ?

=cut

sub get_seq_type : Private {
  my ( $this, $c ) = @_;

  # we only need a temporary file
  my $seq_file = File::Temp->new( UNLINK => 1 );

  # turn sequence into a FASTA file...
  print $seq_file ">user_seq\n";
  print $seq_file substr( $c->stash->{data}->{seq}, 0, 500 );
  
  $c->log->debug( 'Search::Sequence::get_seq_type: esl-seqstat command: '
                  . $this->{seqstatBinary} . ' ' . $seq_file->filename . ' |')
    if $c->debug;
  my $rv = open ( SEQSTAT, $this->{seqstatBinary} . ' ' . $seq_file->filename . ' |' );

  unless ( $rv ) {
    $c->stash->{seqSearchError} = "Couldn't determine sequence type";
    $c->log->debug( "Search::Sequence::get_seq_type: couldn't do 'open': $!" )
      if $c->debug;
    return -1;
  }
  
  my $type;
  while ( <SEQSTAT> ) {
    next unless m/^Alphabet type:\s+(.*)/;
    $type = $1;
  }

  $rv = close SEQSTAT;

  unless ( $rv ) {
    $c->stash->{seqSearchError} = 'There was a problem determining the sequence type';
    if ( $! ) {
      $c->log->debug( "Search::Sequence::get_seq_type: couldn't do 'close': $!" )
        if $c->debug;
      return -1; # flag this as a server error, but...
    }
    else {
      $c->log->debug( "Search::Sequence::get_seq_type: exit status from esl-seqstat: $?" )
        if $c->debug;
      return 0; # ... this as a user error. Whatever they provided made esl-seqstat barf
    }
  }

  unless ( $type and
           $type eq 'DNA' or $type eq 'amino' ) {
    $c->stash->{seqSearchError} = "Not a recognised sequence type (protein/DNA)";
    $c->log->debug( "Search::Sequence::get_seq_type: neither protein nor DNA ('$type')" )
      if $c->debug;
    return 0;
  }

  $c->log->debug( "Search::Sequence::get_seq_type: detected sequence type: $type" )
    if $c->debug;

  $c->stash->{sequence_type} = $type eq 'amino' 
                             ? 'protein'
                             : 'dna';
  return 1;
}

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

    $c->stash->{seqSearchError} = 'You did not supply a job ID';

    return 0;
  }
  
  # make sure it's valid
  unless ( $jobId =~ s/^([A-F0-9\-]{36})$/$1/i ) {
    $c->log->debug( 'Search::Sequence::get_job_details: bad job ID' )
      if $c->debug;

    $c->stash->{seqSearchError} = 'You did not supply a valid job ID';

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

Decides if the input sequence is protein or DNA and validates it accordingly.
Returns true is it's a valid sequence, false otherwise. If the sequence is
not valid, the stash key seqSearchError is populated with a sensible error 
message.

=cut

sub validate_input : Private {
  my ( $this, $c ) = @_;

  # quick validations first; handle the form options

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
      $c->stash->{seqSearchError} = 'The E-value must be a valid positive number <= 10.0.';

      $c->log->debug( 'Search::Sequence::validate_input: bad evalue (NaN); returning to form' )
        if $c->debug;

      return 0;
    }
    
    # secondly, it has to be positive...
    unless ( $c->req->param('evalue') > 0 ) {
      $c->stash->{seqSearchError} = 'The E-value must be a positive number. '
                                    . 'Negative E-values values are meaningless.';

      $c->log->debug( 'Search::Sequence::validate_input: bad evalue (-ve); returning to form' )
        if $c->debug;

      return 0;
    }

    # thirdly and finally, it has to be less than 10.0
    unless ( $c->req->param('evalue') <= 10.0 ) {
      $c->stash->{seqSearchError} = 'The E-value must be <= 10.0. Large E-values '
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

  #---------------------------------------

  # then the slower validation step. Figure out what type of sequence we've got here
  $c->forward('get_seq_type');

  unless ( $c->stash->{sequence_type} ) {
    $c->stash->{seqSearchError} ||= 'We could not determine the type of sequence entered (protein/DNA)';

    $c->log->debug( 'Search::Sequence::validate_input: could not determine sequence type' )
      if $c->debug;

    return 0;
  }

  $c->log->debug( 'Search::Sequence::validate_input: sequence type: ' . $c->stash->{sequence_type} )
    if $c->debug;

  # handle DNA versus protein
  my $valid_seq = $c->stash->{sequence_type} eq 'dna'
                ? $c->forward( 'validate_dna_seq' )
                : $c->forward( 'validate_protein_seq' );

  unless ( $valid_seq ) {
    $c->stash->{seqSearchError} ||= 'There was a problem with your input parameters.';

    $c->log->debug( 'Search::Sequence::validate_input: there was a problem with the sequence' )
      if $c->debug;

    return 0;
  }

  return 1;
}

#-------------------------------------------------------------------------------

=head2 validate_protein_seq : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "seqSearchError". 

=cut

sub validate_protein_seq : Private {
  my ( $this, $c ) = @_;
  
  # make sure we actually have a sequence...
  unless ( $c->stash->{data}->{seq} and $c->stash->{data}->{seq} ne '' ) {
    $c->stash->{seqSearchError} = 'You did not supply an amino-acid sequence.';

    $c->log->debug( 'Search::Sequence::validate_protein_seq: no sequence supplied; failed' )
      if $c->debug;

    return 0;
  }
  
  $c->stash->{user_options}->{dna} = 0;

  # handle various line endings. No need to worry about \n, since we got rid of
  # that with the "split" above
  my $seq = $c->stash->{data}->{seq};
  $seq =~ s/[\s\r\d]+//g;

  # check the length of the sequence at this point. If it's too long, bail
  my $length = length $seq;
  if ( $length > $this->{maxProteinSeqLength} ) {
    $c->stash->{seqSearchError} = 
      'Your sequence is too long. The maximum length of search sequences is ' .
      $this->{maxProteinSeqLength} . '. Please try again with a shorter sequence';

    $c->log->debug( 'Search::Sequence::validate_protein_seq: sequence is too long; failed' )
      if $c->debug;

    return 0;
  }

  # check that the sequence string contains only letters. Bail if it has 
  # anything else in it
  if ( $seq =~ m/[^ABCDEFGHIJKLMNOPQRSTUVWXYZ\*]/g ) {
    $c->stash->{seqSearchError} = 'Invalid sequence; illegal character at position ' 
      . pos($seq) . ' (&quot;' . substr( $seq, pos($seq) - 1, 1 ) 
      . '&quot;). Please try again with a valid amino-acid sequence';

    $c->log->debug( 'Search::Sequence::validate_protein_seq: sequence contains illegal characters; failed' )
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
    $c->stash->{seqSearchError} = 
      'Your sequence does not look like protein. Please upload a protein sequence';

    $c->log->debug( "Search::Sequence::validate_protein_seq: sequence doesn't look like protein; failed" )
      if $c->debug;

    return 0;
  }

  # passed all checks; stuff the sequence into the stash
  $c->stash->{input} = $seq;
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 validate_dna_seq : Private

Validate the form input. Error messages are returned in the stash as
"seqSearchError".

=cut

sub validate_dna_seq : Private {
  my( $this, $c ) = @_;
  
  # make sure we got a parameter first
  unless ( defined $c->req->param('seq') ) {
    $c->stash->{seqSearchError} =
      'You did not supply a valid DNA sequence. Please try again.';

    $c->log->debug( 'Search::Sequence::validate_dna_seq: no DNA sequence; returning to form' )
      if $c->debug;

    return 0;
  }

  $c->stash->{user_options}->{dna} = 1;

  # check it's not too long
  if ( length $c->req->param('seq') > 80_000 ) {
    $c->stash->{seqSearchError} =
      'Your sequence was too long. We can only accept DNA sequences upto 80kb.';

    $c->log->debug( 'Search::Sequence::validate_dna_seq: sequence too long; returning to form' )
      if $c->debug;
      
    return 0;
  }

  # tidy up the sequence and make sure it's only got the valid DNA characters
  my @seqs = split /\n/, $c->req->param('seq');
  shift @seqs if $seqs[0] =~ m/^\>/;
  my $seq = uc( join '', @seqs );
  $seq =~ s/[\s\r\n]+//g;
  
  unless ( $seq =~ m/^[ACGTRYKMSWBDHVN]+$/ ) {
    $c->stash->{seqSearchError} =
      'No valid DNA sequence found. Please enter a valid sequence and try again.';

    $c->log->debug( 'Search::Sequence::validate_dna_seq: invalid DNA sequence; returning to form' )
      if $c->debug;
      
    return 0;
  }

  # store the valid sequence. Up until this point there was no need to have it 
  # in the stash, since it might have been invalid. Now that it's validated, 
  # however, we actually need it
  $c->log->debug( "Search::Sequence::validate_dna_seq: sequence looks ok: |$seq|" )
    if $c->debug;
    
  # passed ! 
  $c->stash->{input} = $seq;
 
  return 1;
}

#-------------------------------------------------------------------------------

=head2 validate_input : Private

Validate the form input. Returns 1 if all input validated, 0 otherwise.
Error messages are returned in the stash as "searchError". 

=cut

# sub validate_input : Private {
#   my ( $this, $c ) = @_;
#   
#   # parse and validate the sequence itself
#   unless ( $c->forward('parse_sequence') ) {
# 
#     # the parse_sequence method will put the sequence into the stash if it
#     # passes validation, but if the sequence looks like DNA or if it's
#     # too long, we also get an error message in the stash. So, we only set 
#     # a general error message here if we don't already have one
#     $c->stash->{searchError} ||= 'Invalid sequence. Please try again with a valid amino-acid sequence.';
# 
#     $c->log->debug( 'Search::Sequence::validate_input: sequence parsing failed' )
#       if $c->debug;
# 
#     return 0;
#   }
# 
#   # somewhere to stash the user options
#   $c->stash->{user_options} = {};
#   
#   # should we use the gathering threshold ?
#   if ( defined $c->req->param('ga') and 
#        $c->req->param('ga') ) {
#     $c->log->debug( 'Search::Sequence::validate_input: using ga' )
#       if $c->debug;
#     $c->stash->{user_options}->{ga} = 1;
#   }
# 
#   # or should we use an E-value ?
#   elsif ( defined $c->req->param('evalue') ) {
#     $c->log->debug( 'Search::Sequence::sequence_search: got an evalue' )
#       if $c->debug;
# 
#     # firstly, it has to be a number
#     unless ( looks_like_number( $c->req->param('evalue') ) ) {
#       $c->stash->{searchError} = 'The E-value must be a valid positive number <= 10.0.';
# 
#       $c->log->debug( 'Search::Sequence::validate_input: bad evalue (NaN); returning to form' )
#         if $c->debug;
# 
#       return 0;
#     }
#     
#     # secondly, it has to be positive...
#     unless ( $c->req->param('evalue') > 0 ) {
#       $c->stash->{searchError} = 'The E-value must be a positive number. '
#                                  . 'Negative E-values values are meaningless.';
# 
#       $c->log->debug( 'Search::Sequence::validate_input: bad evalue (-ve); returning to form' )
#         if $c->debug;
# 
#       return 0;
#     }
# 
#     # thirdly and finally, it has to be less than 10.0
#     unless ( $c->req->param('evalue') <= 10.0 ) {
#       $c->stash->{searchError} = 'The E-value must be <= 10.0. Large E-values '
#                                  . 'result in large numbers of meaningless Pfam '
#                                  . 'hits and cause severe problems for our '
#                                  . 'search system.';
# 
#       $c->log->debug( 'Search::Sequence::validate_input: bad evalue ( > 10.0 ); returning to form' )
#         if $c->debug;
# 
#       return 0;
#     }
# 
#     $c->log->debug( 'Search::Sequence::validate_input: evalue looks like a positive number <= 10.0; stashing' )
#       if $c->debug;
#     $c->stash->{user_options}->{evalue} = $c->req->param('evalue');
# 
#   }
#   
#   # search for Pfam-Bs ?
#   $c->stash->{user_options}->{searchBs} = ( defined $c->req->param('searchBs') and
#                                             $c->req->param('searchBs') );
# 
#   # should we search for Pfam-As or just skip them and search only Pfam-Bs ?
#   if ( defined $c->req->param('skipAs') and
#        $c->req->param('skipAs') ) {
#     
#     $c->log->debug( 'Search::Sequence::validate_input: skipping Pfam-A search' )
#       if $c->debug;
#     
#     # flag up the fact that we want to skip Pfam-A searches
#     $c->stash->{user_options}->{skipAs} = 1;
#     
#     # and force a search for Pfam-Bs
#     $c->stash->{user_options}->{searchBs} = 1;
#   }
# 
#   return 1;  
# }

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
    $c->stash->{seqSearchError} = 'You did not supply an amino-acid sequence.';

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
  if ( $length > $this->{maxProteinSeqLength} ) {
    $c->stash->{seqSearchError} = 
      'Your sequence is too long. The maximum length of search sequences is ' .
      $this->{maxProteinSeqLength} . '. Please try again with a shorter sequence';

    $c->log->debug( 'Search::Sequence::parse_sequence: sequence is too long; failed' )
      if $c->debug;

    return 0;
  }

  # check that the sequence string contains only letters. Bail if it has 
  # anything else in it
  if ( $seq =~ m/[^ABCDEFGHIJKLMNOPQRSTUVWXYZ\*]/g ) {
    $c->stash->{seqSearchError} = 'Invalid sequence; illegal character at position ' 
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
    $c->stash->{seqSearchError} = 
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

=head2 queue_protein_search : Private

Executes a protein sequence search. Queues a Pfam A search to one queue
and, if the appropriate box was checked in the submission form, a Pfam B search
to another queue.

=cut

sub queue_protein_search : Private {
  my ( $this, $c ) = @_;
  
  # first, check there's room on the queue
  my $rv = $c->forward('check_queue');
  unless ( $rv ) {
    $c->stash->{seqSearchError} ||= 'There were too many jobs in the queue.';

    $c->log->debug( 'Search::Sequence::queue_protein_search: queue too full; not submitting' )
      if $c->debug;

    return 0;
  }
  
  # ok. There's room on the queue, so we can submit the jobs. Generate a UUID for the job
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  $c->log->debug( 'Search::Sequence::queue_protein_search: generated job ID: |'
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
    $rv = $c->forward('queue_search_transaction');
    unless ( $rv ) {
      $c->stash->{seqSearchError} ||= 'There was a problem queuing your Pfam-A search.';

      $c->log->warn( 'Search::Sequence::queue_protein_search: problem submitting Pfam-A search' )
        if $c->debug;

      return 0;
    } 

    $c->log->debug( 'Search::Sequence::queue_protein_search: successfully queued Pfam-A search' )
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
    my $rv = $c->forward('queue_search_transaction');
    unless ( $rv ) {
      $c->stash->{seqSearchError} ||= 'There was a problem queuing your Pfam-B search.';

      $c->log->warn( 'Search::Sequence::queue_protein_search: problem submitting Pfam-B search' )
        if $c->debug;

      return 0;
    } 

    $c->log->debug( 'Search::Sequence::queue_protein_search: successfully queued Pfam-B search' )
      if $c->debug;

    $queued++;
  }
  
  #----------------------------------------

  # make sure we have at least one job...
  unless ( $queued ) {
    $c->stash->{seqSearchError} = 'You must run at least one type of search.';
    
    $c->log->warn( 'Search::Sequence::queue_protein_search: no searches submitted' )
      if $c->debug;
      
    return 0;
  }
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_dna_search : Private

Executes a DNA sequence search. Translates the submitted DNA sequence and
submits each frame as a separate protein sequence search.

=cut

sub queue_dna_search : Private {
  my ( $this, $c ) = @_;
  
  # first, check there's room on the queue
  my $rv = $c->forward('check_queue');
  unless ( $rv ) {
    $c->stash->{seqSearchError} ||= 'There were too many jobs in the queue.';

    $c->log->debug( 'Search::Sequence::queue_dna_search: queue too full; not submitting' )
      if $c->debug;

    return 0;
  }
  
  # translate sequence
  $rv = $c->forward('translate_dna');
  unless ( $rv ) {
    $c->stash->{seqSearchError} ||= 'Could not translate DNA sequence.';
    $c->log->debug( 'Search::Sequence::queue_dna_search: failed to translate sequence using external binary' )
      if $c->debug;
    return 0;
  }

  # split the translated file into individual frames
  $rv = $c->forward('split_sequence_file');
  unless ( $rv ) {
    $c->stash->{seqSearchError} ||= 'Could not generate the individual protein sequences.';
    $c->log->debug( 'Search::Sequence::queue_dna_search: failed to split protein sequence using external binary' )
      if $c->debug;
    return 0;
  }

  # this will be the UUID for both the DNA and the protein search rows
  $c->stash->{jobId} = Data::UUID->new()->create_str();
  
  # first, add the raw DNA sequence the queue, but with its status set to "DONE",
  # so that the dequeuers don't try to execute it. This row is simply to give us
  # somewhere to store the untranslated sequence
  $c->stash->{job_type} = 'dna';
  $c->stash->{queued_status} = 'DONE';
  # (the DNA input sequence is put into the stash by "validate_dna_seq")

  $rv = $c->forward('queue_search_transaction');
  unless ( $rv ) {
    $c->stash->{seqSearchError} = 'There was a problem storing your DNA sequence.';
    $c->log->warn( "Search::Sequence::queue_dna_search: error when queueing the DNA sequence" )
      if $c->debug;

    return 0;
  } 
  
  # now queue the separate protein searches

  # keep track of how many we successfully queue
  my $queued = 0;

  # reset these stash values in order to get the next set of searches submitted
  # as protein rather than DNA, and to make sure they actually run
  $c->stash->{job_type} = 'A';
  $c->stash->{queued_status} = undef;

  FRAME: foreach my $i ( 0..5 ) {
    my $frame_file = $c->stash->{translated_fasta} . $i;
    next FRAME unless -e $frame_file;

    # get the input sequence
    $rv = open ( FRAME, $frame_file );
    unless ( $rv ) {
      $c->stash->{seqSearchError} = 'We could not read one of the frame sequences when submitting your search.';
      $c->log->warn( "Search::Sequence::queue_dna_search: couldn't read sequence file for frame $i ($frame_file)" )
        if $c->debug;

      unlink( $c->stash->{translated_fasta} . $_ ) for ( '', 0..5 );

      return 0;
    } 

    $c->stash->{input} = join '', <FRAME>;
    close FRAME;

    # tidy up the sequence. Convert the FASTA format that we get back from
    # "translate" into a plain sequence
    $c->stash->{input} =~ tr|\n||d;
    $c->stash->{input} =~ s/^.*?\d(.*)/$1/;

    $c->stash->{options} = to_json( { dna => 1, frame => $i } );

    # submit the search and make sure that operation was a success
    $rv = $c->forward('queue_search_transaction');
    unless ( $rv ) {
      $c->stash->{seqSearchError} ||= 'There was a problem queuing one of the frames sequences from your DNA search.';
      $c->log->warn( "Search::Sequence::queue_dna_search: problem submitting search for frame $i" )
        if $c->debug;

      unlink( $c->stash->{translated_fasta} . $_ ) for ( '', 0..5 );

      return 0;
    } 

    $c->log->debug( "Search::Sequence::queue_dna_search: successfully queued search for frame $i" )
      if $c->debug;

    $queued++;
  } 
  
  #----------------------------------------

  unlink( $c->stash->{translated_fasta} . $_ ) for ( '', 0..5 );

  # make sure we have at least one job...
  unless ( $queued ) {
    $c->stash->{seqSearchError} = 'We did not successfully queue searches for any of the frames.';
    $c->log->warn( 'Search::Sequence::queue_dna_search: no searches submitted' )
      if $c->debug;

    return 0;
  }

  return 1;
}

#-------------------------------------------------------------------------------

=head2 check_queue : Private

Checks the search queue and makes sure it isn't too full to submit accept new
jobs. If there are too many jobs, the stash key "seqSearchError" is set to an
appropriate error message and the return value is set to false. If there is
room on the queue, returns true.

=cut

sub check_queue : Private {
  my ( $this, $c ) = @_;

  my $rs = $c->model( 'WebUser::JobHistory' )
             ->search( { -and => [ { status => 'PEND' },
                                    -or => [ { job_type => 'A' },
                                             { job_type => 'B' } ] ] },
                       { select => [ { count => 'status' } ],
                         as     => [ 'numberPending' ] } )
             ->single;
  
  $c->stash->{number_pending} = $rs->get_column( 'numberPending' );
  
  $c->log->debug( 'Search::Sequence::check_queue: |' . 
                  $c->stash->{number_pending} . '| jobs pending' ) if $c->debug;
  
  if ( $c->stash->{number_pending} >= $this->{pendingLimit} ) {
    $c->stash->{seqSearchError} = 
      'There are currently too many jobs in the sequence search queue. ' . 
      'Please try again in a little while.';

    # TODO send an email to the admins about the full queue

    $c->log->debug( 'Search::Sequence::check_queue: too many Pfam jobs in queue ('
                    . $c->stash->{number_pending} . ')' ) if $c->debug;

    return 0;
  }
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 queue_seq_search : Private

Executes a protein sequence search. Queues a Pfam A search to one queue
and, if the appropriate box was checked in the submission form, a Pfam B search
to another queue.

=cut

# sub queue_seq_search : Private {
#   my ( $this, $c ) = @_;
#   
#   # first, check there's room on the queue
#   my $rs = $c->model( 'WebUser::JobHistory' )
#              ->search( { -and => [ { status => 'PEND' },
#                                     -or => [ { job_type => 'A' },
#                                              { job_type => 'B' } ] ] },
#                        { select => [ { count => 'status' } ],
#                          as     => [ 'numberPending' ] } )
#              ->single;
#   
#   $c->stash->{number_pending} = $rs->get_column( 'numberPending' );
#   
#   $c->log->debug( 'Search::Sequence::queue_seq_search: |' . 
#                   $c->stash->{number_pending} . '| jobs pending' ) if $c->debug;
#   
#   if ( $c->stash->{number_pending} >= $this->{pendingLimit} ) {
#     $c->stash->{searchError} = 
#       'There are currently too many jobs in the sequence search queue. ' . 
#       'Please try again in a little while.';
# 
#     # TODO send an email to the admins about the full queue
# 
#     $c->log->debug( 'Search::Sequence::queue_seq_search: too many Pfam jobs in queue ('
#                     . $c->stash->{number_pending} . ')' ) if $c->debug;
# 
#     return 0;
#   }
#   
#   # ok. There's room on the queue, so we can submit the jobs. Generate a UUID for the job
#   $c->stash->{jobId} = Data::UUID->new()->create_str();
#   $c->log->debug( 'Search::Sequence::queue_seq_search: generated job ID: |'
#                   . $c->stash->{jobId} . '|' ) if $c->debug;
#   
#   #----------------------------------------
# 
#   # keep track of queued jobs
#   my $queued = 0;
# 
#   # submit a Pfam-A job, unless we've been asked to skip it  
#   if ( not $c->stash->{user_options}->{skipAs} ) {
# 
#     # set the job type
#     $c->stash->{job_type} = 'A';
# 
#     # convert the options hash into JSON
#     $c->stash->{options} = to_json( $c->stash->{user_options} );
#     
#     # submit the search and make sure that operation was a success
#     unless ( $c->forward( 'queue_search_transaction' ) ) {
#       $c->stash->{searchError} ||= 'There was a problem queuing your Pfam-A search.';
# 
#       $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-A search' )
#         if $c->debug;
# 
#       return 0;
#     } 
# 
#     $c->log->debug( 'Search::Sequence::queue_seq_search: successfully queued Pfam-A search' )
#       if $c->debug;
# 
#     $queued++;
#   } 
#   
#   #----------------------------------------
# 
#   # submit a Pfam-B job, if we've been asked to run one
#   if ( $c->stash->{user_options}->{searchBs} ) {
#     
#     # set the job type
#     $c->stash->{job_type} = 'B';
# 
#     # for Pfam-Bs, there are no options.
#     $c->stash->{options} = '{}';
#     
#     # submit the search and make sure that operation was a success
#     unless ( $c->forward( 'queue_search_transaction' ) ) {
#       $c->stash->{searchError} ||= 'There was a problem queuing your Pfam-B search.';
# 
#       $c->log->warn( 'Search::Sequence::queue_seq_search: problem submitting Pfam-B search' )
#         if $c->debug;
# 
#       return 0;
#     } 
# 
#     $c->log->debug( 'Search::Sequence::queue_seq_search: successfully queued Pfam-B search' )
#       if $c->debug;
# 
#     $queued++;
#   }
#   
#   #----------------------------------------
# 
#   # make sure we have at least one job...
#   unless ( $queued ) {
#     $c->stash->{searchError} = 'You must run at least one type of search.';
#     
#     $c->log->warn( 'Search::Sequence::queue_seq_search: no searches submitted' )
#       if $c->debug;
#       
#     return 0;
#   }
#   
#   return 1;
# }

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

=head2 translate_dna : Private

Translates the DNA sequence in C<$c->stash->{seq_file}> into protein using the 
external binary C<translate>.

=cut

sub translate_dna : Private {
  my ( $this, $c ) = @_;

  # we only need a temporary file
  my $seq_file = File::Temp->new( UNLINK => 1 );
  $c->stash->{seq_file} = $seq_file;

  # turn sequence into a FASTA file...
  print $seq_file ">user_seq\n";
  print $seq_file $c->stash->{data}->{seq};
  
  # output filename
  $c->stash->{translated_fasta} = $c->stash->{seq_file}->filename . '.translated';

  # "translate" command line
  my @translate_params = ( $this->{translateBinary}, '-qao', $c->stash->{translated_fasta}, $c->stash->{seq_file} );

  $c->log->debug( 'Search::Sequence::translate_dna: translate command: ', join ' ', @translate_params )
    if $c->debug;

  my $run = start \@translate_params, '<pipe', \*IN, '>pipe', \*OUT, '2>pipe', \*ERR;
  unless ( $run ) {
    $c->stash->{seqSearchError} = 'We could not translate your DNA sequence.';

    $c->log->debug( "Search::Sequence::translate_dna: 'translate' failed on run; error status: $?" )
      if $c->debug;

    return 0;
  }
   
  close IN;
  close OUT;

  my $err;
  while (<ERR>) {
    $err .= $_;
  }
  close ERR;

  my $finished = finish $run;
  unless ( $finished ) {
    $c->stash->{seqSearchError} = 'There was a problem translating your DNA sequence.';

    $c->log->debug( "Search::Sequence::translate_dna: 'translate' failed on finish: $err" )
      if $c->debug;

    return 0;
  }

  # translated successfully; return true
  return 1;
}

#-------------------------------------------------------------------------------

=head2 split_sequence_file : Private

Splits the protein sequences that were translated from a DNA sequence. Reads
the single FASTA file and uses the binary "csplit" to generate six individual
files for the six frames.

=cut

sub split_sequence_file : Private {
  my ( $this, $c ) = @_;

  # split translated protein file into six separate files. We should be able to run 
  # this using IPC::Run but I can't get the escaping syntax right for the pattern, so 
  # we'll take the easy route and use "system". I hit the same escaping problems with
  # system if we use the list rather than scalar approach, hence...
  my $csplit_cmd = join ' ', (
    $this->{csplitBinary},
    '-s -z -b%1d', '-f', $c->stash->{translated_fasta},
    $c->stash->{translated_fasta}, q('/^>/' {*})
  );

  $c->log->debug( "Search::Sequence::split_sequence_file: csplit command: $csplit_cmd" )
    if $c->debug;

  my $split = system $csplit_cmd;
  if ( $split ) {
    $c->stash->{seqSearchError} = 'We could not split your protein sequence into separate frames';

    $c->log->debug( "Search::Sequence::split_sequence_file: 'csplit' failed on run; error status: $?" )
      if $c->debug;

    return 0;
  }
   
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
