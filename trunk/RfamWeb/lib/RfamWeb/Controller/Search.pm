
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
use URI::Escape;
use Search::QueryParser;
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
#- taxonomy search actions -----------------------------------------------------
#-------------------------------------------------------------------------------

=head2 taxonomy : Chained('search') PathPart('taxonomy') Args(0)

Stub to start the chain for taxonomy-related searches.

=cut

sub taxonomy : Chained( 'search' )
               PathPart( 'taxonomy' )
               CaptureArgs( 0 ) { }

#---------------------------------------

=head2 tax_query : Chained('taxonomy') PathPart('') Args(0)

Accepts the input from the taxonomy search form and returns the results page.
The results page doesn't actually contain results but it does fire off an 
AJAX request to get results. This action handles both the taxonomy boolean
search and the unique domains search.

=cut

sub tax_query : Chained( 'taxonomy' )
                PathPart( '' )
                Args( 0 ) {
  my ( $this, $c ) = @_;
  
  # detaint the input
  my $query = $c->forward('detaintQueryString', [ $c->req->param('q') ] );

  # there's something wrong with the basic string
  unless ( $query ) {
    $c->log->debug( 'Search::tax_query: problem with queryString' )
      if $c->debug;
    $c->stash->{taxSearchError} = $c->stash->{queryStringError};
    return;
  }
  
  # stash what we have so far
  $c->stash->{q} = $query;
  
  # call parseTerms, to parse the query string and return it with the species 
  # names quoted. That method also puts two arrays into the stash: @terms 
  # stores the raw terms, @sentence stores the terms plus the boolean operators 
  # and braces

  # We need to do this to make sure that we have only a single term in the 
  # query, and so that we can return the error message from this action rather
  # than the one that's triggered by the AJAX call. Because we don't care about
  # the quoted query string, we don't need to keep hold of the return value of
  # the "parseTerms" action this time
  $c->forward('parseTerms', [ $query ] );

  # make sure we got *some* terms in the stash
  unless ( defined $c->stash->{terms} and
           scalar @{ $c->stash->{terms} } ) {
    $c->log->debug( 'Search::tax_query: no terms found' )
      if $c->debug;
    $c->stash->{taxSearchError} =
      'We did not find any species names in the string that you entered: "' . 
      $query . '"';
    return;
  }

  # see if we should be looking for families that are unique to a single species
  if ( $c->req->param('unique') ) {

    $c->log->debug( 'Search::tax_query: checking for a single term' )
      if $c->debug;

    # we should have only one species name in the query
    unless ( scalar @{ $c->stash->{terms} } == 1 ) {
      $c->stash->{taxSearchError} = 'You can only find unique families for a single species.'
        . ' Please enter only one species name and try again.';
      return;
    }
    
    $c->log->debug( 'Search::tax_query: handing off to unique search template' )
      if $c->debug;
    $c->stash->{template} = 'pages/search/taxonomy/uniqueResults.tt';  

  }
  else {

    $c->log->debug( 'Search::tax_query: handing off to taxonomy search template' )
      if $c->debug;
    $c->stash->{template} = 'pages/search/taxonomy/taxonomyResults.tt';
  }
}

#-------------------------------------------------------------------------------

=head2 taxonomyQuery : Path

Performs a taxonomy search and retrieves Pfam-A families selected by the
user-supplied boolean query strings, such as

  "Caenorhabditis elegans AND NOT Homo sapiens"

Renders a template that generates a page fragment, which is intended to be
inserted into a page via an AJAX call.

=cut

sub tax_query_results : Chained( 'taxonomy' )
                        PathPart( 'results' )
                        Args( 0 ) {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Search::Taxonomy::taxonomyQuery: starting a taxonomy search' )
    if $c->debug;

  # detaint the input
  my $query = $c->forward('detaintQueryString', [ $c->req->param('q') ] );

  unless( $query ) {
    $c->log->debug( 'Search::Taxonomy::taxonomyQuery: problem with queryString' )
      if $c->debug;
    $c->stash->{taxSearchError} = $c->stash->{queryStringError};
    return;
  }
  
  # drop the original, but detainted, query string into the hash for the 
  # template to use later
  $c->stash->{queryString} = $query;
  
  #----------------------------------------
  
  # call parseTerms, to parse the query string and return it with the species 
  # names quoted. That method also puts two arrays into the stash: @terms 
  # stores the raw terms, @sentence stores the terms plus the boolean operators 
  # and braces
  
  my $quotedQuery = $c->forward('parseTerms', [ $query ] );

  #----------------------------------------
  
  # we got a valid string, but does it parse ?
  $c->log->debug( "Search::Taxonomy::taxonomyQuery: got quoted query string: |$quotedQuery|" )
    if $c->debug;

  # see if we can retrieve this result from cache. Need to get rid of the 
  # stranger characters in the quoted string, like quotes, for a start...
  my $cacheKey = 'taxonomySearch' . $quotedQuery;
  $cacheKey =~ s/[\W\s]/_/g;
  my $families = $c->cache->get( $cacheKey );
  
  if( defined $families ) {
    $c->log->debug( 'Search::Taxonomy::taxonomyQuery: retrieved families from cache' )
      if $c->debug;
  } else {
    $c->log->debug( 'Search::Taxonomy::taxonomyQuery: failed to retrieve families from cache; going to DB' )
      if $c->debug;
   
    my $qp = new Search::QueryParser;
    my $pq = $qp->parse( $quotedQuery );
    unless( $pq ) {
      $c->stash->{taxSearchError} = 'There was a problem with your query. '
        . 'Please check that you entered a valid boolean query and try again.';
      return;
    }
      
    $c->log->debug( 'Search::Taxonomy::taxonomyQuery: parsed query: ', dump $pq )
      if $c->debug;
  
    # walk down the tree and accumulate the list of Pfam-As for the specified
    # species
    $families = $c->forward('descend', [ $pq, '' ] );
  
    # cache the result
    $c->cache->set( $cacheKey, $families ) unless $ENV{NO_CACHE};
  }
  
  $c->stash->{families} = $families;
  $c->stash->{template} = 'pages/search/taxonomy/taxSearchResults.tt';
}

#-------------------------------------------------------------------------------

=head2 uniqueQuery : Local

Retrieves the list of Pfam-A accessions for families that are unique to the
specified taxonomic level (species, e.g. "C. elegans", or level, 
e.g. "Metazoa").

Renders a template that generates a page fragment, which is intended to be
inserted into a page via an AJAX call.

=cut

sub unique_taxonomy : Chained( 'search' )
                      PathPart( 'unique' )
                      CaptureArgs( 0 ) { }

sub unq_query : Chained( 'unique_taxonomy' )
                PathPart( 'results' )
                Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Search::unq_query: starting a unique family search' )
    if $c->debug;

  # set up the template early. The same template will handle both results and
  # error messages
  $c->stash->{template} = 'pages/search/taxonomy/uniqueSearchResults.tt';

  #----------------------------------------

  # detaint the input
  my $query = $c->forward( 'detaintQueryString', [ $c->req->param('q') ] );

  unless ( $query ) {
    $c->log->debug( 'Search::unq_query: problem with queryString' )
      if $c->debug;
    $c->stash->{taxSearchError} = $c->stash->{queryStringError};
    return;
  }
  
  # drop the original, but detainted, query string into the hash for the 
  # template to use later
  $c->stash->{queryString} = $query;
  
  #----------------------------------------
  
  # call parseTerms, to parse the query string and return it with the species 
  # names quoted. That method also puts two arrays into the stash: @terms 
  # stores the raw terms, @sentence stores the terms plus the boolean operators 
  # and braces
  
  my $quotedQuery = $c->forward('parseTerms', [ $query ] );

  # we should have only one species name in the query
  if ( scalar @{ $c->stash->{terms} } > 1 ) {
    $c->log->debug( 'Search::unq_query: multiple terms specified; detaching' );
    $c->stash->{uniqueSearchError} = 
      'You can only find unique families for a single species. ' .
      'Please enter just one species name and try again.';
    return;
  }
  
  $c->log->debug( "Search::unq_query: finding unique matches for |$quotedQuery|" )
    if $c->debug;

  #----------------------------------------

  # see if we can retrieve this result from cache. Need to get rid of the 
  # stranger characters in the quoted string, like quotes, for a start...
  my $cacheKey = 'uniqueFamilies' . $quotedQuery;
  $cacheKey =~ s/[\W\s]/_/g;
  my $unique = $c->cache->get( $cacheKey );
  
  if ( defined $unique ) {
    $c->log->debug( 'Search::unq_query: retrieved unique families from cache' )
      if $c->debug;
  } 
  else { 
    $c->log->debug( 'Search::unq_query: failed to retrieve unique families from cache; going to DB' )
      if $c->debug;

    # get the hash containing the information we need about each family
    my $familyInfo = $c->forward('getFamilyInfo');

    my $allCount  = $c->forward('getAllFamilyCount');
    my $termCount = $c->forward('getFamilyCount', [ $quotedQuery ] );

    # some error checking
    
    # see if there was a problem down the line with getting the lft and rgt 
    # values from the species tree table
    if ( $c->stash->{rangeError} ) {
      $c->log->debug( 'Search::unq_query: problem when getting range' )
        if $c->debug;
      $c->stash->{uniqueSearchError} = 
        'There was a problem with your query. ' .
        'We could not find the species that you specified.';
      return;
    }

    # see if we got back some term counts 
    unless ( defined $termCount and 
            scalar( keys %$termCount ) ) {
      $c->log->debug( 'Search::unq_query: no term counts retrieved' )
        if $c->debug;
      $c->stash->{uniqueSearchError} = 
        'There was a problem with your query. ' .
        'We did not find any species matching your query';
      return;
    }

    $unique = {};
    while ( my ( $k, $v ) = each %$termCount ) {
      # now see if the count is the same; if it is then it must be unique to 
      # the term
      $unique->{$k} = $familyInfo->{$k}
        if $allCount->{$k} == $v;
    }

    # cache the result
    $c->cache->set( $cacheKey, $unique ) unless $ENV{NO_CACHE};
  }
  
  # stash the results and we're done. Let the template render the results
  $c->stash->{families} = $unique;
}

#-------------------------------------------------------------------------------

=head2 suggest : Local

Returns a list of possible completions for species names as an HTML list (ul).

This action is intended to be called only by an AJAX call from the taxonomy 
search form. The resulting list is presented as auto-completion lines under the 
form field, letting the user know immediately that they can't spell 
Caenorhabditis elegans after all...

=cut

sub suggest : Chained( 'search' )
              PathPart( 'suggest' )
              Args( 0 ) {
  my( $this, $c ) = @_;

  # detaint the input
  my $query = $c->forward('detaintQueryString', [ $c->req->param('q') ] );

  # split up the search string into individual 'words'. We don't care about
  # the return value, which gives the search string with the species names
  # quoted
  $c->forward( 'parseTerms', [ $query ] );

  #----------------------------------------

  # perform a database look-up for the LAST species name in the search string, 
  # if there is one at this point

  my( $rawSpecies, @speciesSuggestions );
  my $lastTerm = $c->stash->{terms}->[-1] || '';
  
  # trap terms that are junk and only suggest for real ones
  if( defined $lastTerm and       # must be defined, for a start
      $lastTerm and               # must be true, i.e. not whitespace or 0
      length( $lastTerm ) > 2 ) { # must be 3 or more characters in length 
    
    $rawSpecies = $c->stash->{terms}->[-1];
  
    $c->log->debug( "Search::Taxonomy::suggest: raw species name: |$rawSpecies|" )
      if $c->debug;

    # collect the list of matching species names    
    my @species = $c->model('RfamDB::TaxonomyWebsearch')
                    ->search_like( { species => "$rawSpecies%" } );
    foreach ( @species ) {
      push @speciesSuggestions, $_->species;
    }
                 
    # collect the list of matching levels, which are distinct from the actual
    # species name    
    my @levels  = $c->model('RfamDB::TaxonomyWebsearch')
                    ->search_like( { level => "$rawSpecies%" } );
    foreach ( @levels ) {
      push @speciesSuggestions, $_->level;
    }
  
    $c->log->debug( 'Search::Taxonomy::suggest: found a total of |'
                    . scalar @speciesSuggestions . "| suggestions for |$rawSpecies|" )
      if $c->debug;
  
  } else {
  
    # the text that we have in the suggestion field doesn't parse to give one
    # or more meaningful terms; bail out now
    $c->res->body( '<ul><li><span class="informal">No suggestions...</li></ul>' );
    return;
  }
  
  #----------------------------------------

  # rebuild the search string using the suggestions, of which we have at least
  # one by this point

  # limit the number of suggestions that we return; set in the config
  my $limit = $this->{numSuggestions} || 10;
  my $i = 0;

  my @searchSuggestions;
  my $suggestionLine = '';
  my $limited = 0;

  WORD:
  foreach my $word ( @{ $c->stash->{sentence} } ) {
    $c->log->debug( "Search::Taxonomy::suggest: suggesting for word |$word|" );

    # if this word is not the same as the last search term (which will be the
    # case if it's AND, OR, NOT or a brace), include it as is
    unless( $word eq $rawSpecies ) {
      $suggestionLine .= $word . ' ';
      next WORD;
    }

    # only add suggestions for the last search term
    unless( $word eq $c->stash->{sentence}->[-1] ) { 
      $suggestionLine .= $word . ' ';
      next WORD;
    }
    
    # build the list of suggestions based on the suggestions for the current
    # word
    SUGGESTION: 
    foreach my $suggestion ( sort @speciesSuggestions ) {
      push @searchSuggestions, $suggestionLine . $suggestion;
      if( $i >= $limit - 1 ) {
        $limited = 1;
        last WORD;
      }
      $i++;
    }

    $c->log->debug( "Search::Taxonomy::suggest: we now have |$i| suggestion lines ("
                    . scalar @searchSuggestions . ' according to array)' )
      if $c->debug;
  }

  my $responseString = '<ul>';
  foreach my $i ( 0 .. $#searchSuggestions ) {
    my $suggestion = $searchSuggestions[$i];

    $c->log->debug( "Search::Taxonomy::suggest: search suggestion: |$suggestion|" )
      if $c->debug;

    if( $limited and $i == $#searchSuggestions ) {
      $c->log->debug( 'Search::Taxonomy::suggest: suggestion list limited' )
        if $c->debug;
      $responseString .= qq(<li>$suggestion<span class="informal"><br />(Showing only first $limit suggestions)</span></li>); 
    } else {
      $responseString .= "<li>$suggestion</li>";
    }
  }
  
  $responseString .= '</ul>';
  
  #----------------------------------------

  $c->res->body( $responseString );
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
  unless ( $seq =~ m/^[ACGUTSWMKRYBDHVN\-]+$/ ) {
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
#- taxonomy-related private actions --------------------------------------------
#-------------------------------------------------------------------------------

=head2 detaintQueryString : Private

Checks the input string and returns the detainted version or, if there was a 
problem with the string, returns "0" (actually, calls via c<forward> always
return "0" by default) and puts the error message into the stash as 
"queryStringError".

=cut

sub detaintQueryString : Private {
  my( $this, $c, $queryString ) = @_;
  
  # make sure we got *something*
  unless( defined $queryString ) {
    $c->stash->{queryStringError} = 'There was a problem with your query. '
      . 'You did not supply a query string.';
    return 0;
  }

  # make sure it was plain text
  unless( $queryString =~ m/^([\w\s():\-%]+)$/ ) {
    $c->log->debug( "Search::Taxonomy::detaintQueryString: rejecting query string: |$queryString|" )
      if $c->debug;
    $c->stash->{queryStringError} = 'There was a problem with your query. '
      . 'Please check that there are no illegal characters in your query string and try again.';
    return 0;
  }
  my $encodedQueryString = $1;
  
  # decode the string, since it could have been encoded by the javascript 
  # function "encodeURI"
  my $decodedQueryString = uri_unescape( $encodedQueryString );

  # make sure we ended up with something after decoding it... We don't allow
  # "%" now, since that should have been there only to allow URI encoding
  unless( $decodedQueryString =~ m/^([\w\s():\-]+)$/ ) {
    $c->log->debug( "Search::Taxonomy::detaintQueryString: rejecting detainted query string: |$decodedQueryString|" )
      if $c->debug;
    $c->stash->{queryStringError} = 'There was a problem checking your query. '
      . 'Please check that there are no illegal characters in your query string and try again.';
    return 0;
  }
    
  $c->log->debug( "Search::Taxonomy::detaintQueryString: passed query string: |$decodedQueryString|" )
    if $c->debug;
    
  return $decodedQueryString;
}

#-------------------------------------------------------------------------------

=head2 parseTerms : Private

Parses the query string into two arrays, @terms and @sentence, containing the 
species names and all of the tokens within the query respectively. The two 
arrays are dropped into the stash. Returns the query string with the species 
names quoted.

=cut

sub parseTerms : Private {
  my( $this, $c, $query ) = @_;

  # later we'll want to chomp to remove trailing spaces... 
  local $/ = ' ';
  
  # put spaces around braces to make sure we see them in the list of words when 
  # we split on spaces
  $query =~ s|([\(\)])| $1 |g;

  # strip leading and trailing spaces 
  $query =~ s/^\s*(.*?)\s*$/$1/;
  $c->log->debug( "Search::Taxonomy::parseTerms: starting with search term: |$query|" )
    if $c->debug;
  
  # break up the search term into "words"
  my @words = split /\s+/, $query;
  
  #----------------------------------------
  
  # next, filter the terms to collect the actual species names separately from
  # the boolean terms and braces. We'll also need to keep track of the order
  # of everything though, so that we can reconstruct the whole string, corrected
  # by the searches

  my( @terms, @sentence );
  my $term = '';
  foreach my $index ( 0 .. $#words ) {
    my $word = $words[$index];

    # filter out the debris of merging AND and NOT
    next if not defined $word;

    # and actually merge AND and NOT...
    if( $word =~ /^AND$/i ) {
      
      # check if the next word in the sentence is NOT and, if it is, merge
      # it with the current AND before deleting the NOT from the sentence
      # altogether
      if( $words[$index + 1] and 
          $words[$index + 1] eq 'NOT' ) {
        delete $words[$index + 1];
        chomp $term;
        push @sentence, $term, 'AND NOT';
        push @terms, $term;
        $term = '';

      } else {
        chomp $term;
        push @sentence, $term, 'AND';
        push @terms, $term;
        $term = '';
      }

    } elsif( $word =~ m/^(OR|NOT|\(|\))$/i ) {

      # just stuff this term (OR, NOT or braces) into the sentence, but ignore
      # terms that are just composed of spaces
      unless( $term =~ /^\s*$/ ) {
        chomp $term;
        push @sentence, $term;
        push @terms, $term;
      }
      push @sentence, uc $word;
      $term = '';

    } else {
      # the word that we have is a search term; add it to the growing search 
      # term string
      $term .= $word . ' ';
    }

  }

  # finally, if we had a non-whitespace term at the end of the search string, 
  # push in it into the list
  unless( $term =~ /^\s*$/ ) {
    chomp $term;
    push @sentence, $term;
    push @terms, $term;
  }

  #----------------------------------------

  # now we should have two arrays, one with the species names that we'll need
  # to search (@term), one with the whole search term broken up into 'words'
  # (@sentence)
  
  # before we push these into the stash though, we'll check if we actually
  # retrieved some terms, otherwise the query isn't meaningful
  if( scalar @terms ) {
    $c->log->debug( 'Search::Taxonomy::parseTerms: retrieved some terms from the query' )
      if $c->debug;
    $c->stash->{terms}    = \@terms;
    $c->stash->{sentence} = \@sentence;    
  } else {
    $c->log->warn( 'Search::Taxonomy::parseTerms: no search terms retrieved' )
      if $c->debug;
  }
  
  # finally, put double-quotes around each of the search terms in the input
  # string and return that
  foreach my $term ( @{ $c->stash->{terms} } ) {
    $query =~ s/($term)/"$1"/ig;
  }
  
  return $query;
}

#-------------------------------------------------------------------------------

=head2 getFamilyCount : Private

Retrieves the list of families in a given species. Returns a reference to a 
hash containing the Rfam accession and the count of the number of families.

=cut

sub getFamilyCount : Private {
  my ( $this, $c, $term ) = @_;  

  # see if we can retrieve the families from cache
  my $cacheKey = 'familyCount' . $term;
  $cacheKey =~ s/[\W\s]/_/g;
  $c->log->debug( "Search::Taxonomy::getFamilyCount: cacheKey: |$cacheKey|" )
    if $c->debug;
  my $termCount = $c->cache->get( $cacheKey );

  if ( defined $termCount ) {
    $c->log->debug( 'Search::Taxonomy::getFamilyCount: retrieved family counts from cache' )
      if $c->debug;
  } 
  else {
    $c->log->debug( 'Search::Taxonomy::getFamilyCount: failed to retrieve family counts from cache; going to DB' )
      if $c->debug;

    my $range = $c->forward('getRange', [ $term ] );
    
    if ( not $range ) {
      $c->log->debug( 'Search::Taxonomy::getFamilyCount: range error' );
      $c->stash->{rangeError} = "Could not find $term";
      return;
    }

    $c->log->debug( 'Search::Taxonomy::getFamilyCount: getting count for '
                    . "|$term|, |$range->[0]|, |$range->[1]|" )
      if $c->debug;
  
    my @rs = $c->model('RfamDB::TaxonomyWebsearch')
               ->search( { lft => { '>=' => $range->[0] },
                           rgt => { '<=' => $range->[1] } },
                         { join     => [ 'rfam_ncbi' ],
                           select   => [ 'rfam_ncbi.rfam_acc', 
                                         { count => 'rfam_ncbi.rfam_acc' } ],
                           as       => [ 'rfam_acc', 'count' ],
                           group_by => [ 'rfam_ncbi.rfam_acc' ],
                         } );

    foreach ( @rs ) {
      next unless( defined $_->get_column('rfam_acc') and
                   defined $_->get_column('count') );
      $termCount->{ $_->get_column('rfam_acc') } = $_->get_column('count');
    }
  
    $c->log->debug( 'Search::Taxonomy::getFamilyCount: got |'
                    . scalar( keys %$termCount ) . '| family counts' )
      if $c->debug;

    # cache the result
    $c->cache->set( $cacheKey, $termCount ) unless $ENV{NO_CACHE};
  }
  
  return $termCount;
}

#-------------------------------------------------------------------------------

=head2 getAllFamilyCount : Private

Retrieves, for each Rfam family, the count of the number of times that family
occurs for each species. Returns a reference to a hash with Rfam accession 
and count.

=cut

sub getAllFamilyCount : Private {
  my ( $this, $c ) = @_;  

  my $cacheKey = 'allFamilyCount';
  $c->log->debug( "Search::Taxonomy::getAllFamilyCount: cacheKey: |$cacheKey|" )
    if $c->debug;
  my $res      = $c->cache->get( $cacheKey );

  if(  defined $res ) {
    $c->log->debug( 'Search::Taxonomy::getAllFamilyCount: retrieved family counts from cache' )
      if $c->debug;
  } 
  else {
    $c->log->debug( 'Search::Taxonomy::getAllFamilyCount: failed to retrieve family counts from cache; going to DB' )
      if $c->debug;

    my @rs = $c->model('RfamDB::RfamNcbi')
               ->search( {},
                         { select   => [ 'rfam_acc', 
                                         { count => 'auto_rfam' } ],
                           as       => [ 'rfam_acc', 'count' ],
                           group_by => [ 'me.auto_rfam' ],
                         } );
  
    # hash the results
    foreach ( @rs ) {
      next unless( defined $_->get_column('rfam_acc') and
                   defined $_->get_column('count') );
      $res->{ $_->get_column('rfam_acc') } = $_->get_column('count');
    }
    
    # and cache them
    $c->cache->set( $cacheKey, $res ) unless $ENV{NO_CACHE};
  }

  return $res;
}

#-------------------------------------------------------------------------------

=head2 getFamiliesForTerm : Private

Returns the families that are found for the single specified taxonomic term. 
This is used only by the "descend" method, to build the list of Pfam-A families 
for a set of species.

The first job is to find the range, the lft and rgt values, for the given
species in the table that stores the species tree. Given that range we can
find the sub-tree under that node, from which we get the families for
those species.

=cut

sub getFamiliesForTerm : Private {
  my( $this, $c, $term ) = @_;
  
  # see if we can retrieve the families for this species from cache
  my $cacheKey = 'familiesForTerm' . $term;
  $cacheKey =~ s/[\W\s]/_/g;  
  $c->log->debug( "Search::Taxonomy::getFamilies: cacheKey: |$cacheKey|" )
    if $c->debug;
  my $res      = $c->cache->get( $cacheKey );
  
  if( defined $res ) {
    $c->log->debug( 'Search::Taxonomy::getFamiliesForTerm: retrieved families from cache' )
      if $c->debug;
  } else { 

    # find the left and right values for this specific species name
    my $range = $c->forward('getRange', [ $term ] );
  
    # "getRange" returns 0 if it couldn't find a range for the given term, or
    # a hash ref if it found the lft/rgt values
  
    # return "0" by default, as this is what $c->forward will enforce anyway
    return 0 unless ref($range) eq 'ARRAY';

    # get the hash containing the information we need about each family
    my $familyInfo = $c->forward('getFamilyInfo');
  
    $c->log->debug( 'Search::Taxonomy::getFamiliesForTerm: failed to retrieve families from cache; going to DB' )
      if $c->debug;

    my @rs = $c->model('RfamDB::RfamNcbi')
               ->search( { 'tax.lft'    => { '>=' => $range->[0] },
                           'tax.rgt'    => { '<=' => $range->[1] } },
                         { join     => [ 'tax' ],
                           prefetch => [ 'tax' ] }
                       );
  
    # map the Pfam-A accession to a hash with other information for the family
    my %res = map{ $_->rfam_acc => $familyInfo->{$_->rfam_acc} } @rs;

    $c->log->debug( 'Search::Taxonomy::getFamiliesForSpecies: found |'
                    . scalar( keys %res ) . '| families' )
      if $c->debug;
      
    $res = \%res;
      
    # cache the result
    $c->cache->set( $cacheKey, $res ) unless $ENV{NO_CACHE};
  }
  
  return $res;
}

#-------------------------------------------------------------------------------

=head2 getFamilyInfo : Private

Retrieves items of information for all Pfam families and stores them in a hash.
Tries to retrieve the hash from cache before hitting the database. Currently the
hash is keyed on Pfam-A accession and contains references to hashes with the
accession, ID and description of the family.

This hash is used to add a little more information to the results of the 
taxonomy searches.

=cut

sub getFamilyInfo : Private {
  my( $this, $c ) = @_;

  my $cacheKey = 'familyInfo';
  my $familyInfo = $c->cache->get( $cacheKey );

  if( defined $familyInfo ) {
    $c->log->debug( 'Search::Taxonomy::getFamilyInfo: retrieved family info from cache' );
  } else {
    $c->log->debug( 'Search::Taxonomy::getFamilyInfo: failed to retrieve family info from cache; going to DB' );

    my @rs = $c->model('RfamDB::Rfam')
               ->search( {},
                         { select => [ qw( rfam_acc rfam_id description ) ] }
                       );

    my %familyInfo = map { $_->rfam_acc => { rfam_acc   => $_->rfam_acc,
                                             rfam_id    => $_->rfam_id,
                                             description => $_->description } } @rs;

    $c->log->debug( 'Search::Taxonomy::getFamilyInfo: found info for |'
                    . scalar( keys %familyInfo ). '| families' );

    $familyInfo = \%familyInfo;
    $c->cache->set( $cacheKey, $familyInfo ) unless $ENV{NO_CACHE};
  }

  return $familyInfo;
}

#-------------------------------------------------------------------------------

=head2 descend : Private

Walks down the tree generated by parsing the taxonomy query and runs searches 
for each of the species terms found. Collects the resulting species 
name-to-NCBI code mapping in a hash and returns the reference. 

=cut

sub descend : Private {
  my( $this, $c, $query, $operator ) = @_;

  my $collectedFamilies = {};

  # walk over these specific keys. Need to have this as a literal list in order
  # to fix the order of precedence
  foreach my $k ( '+', '-', '', 'value'){
    next unless $query->{$k};
    
    $c->log->debug( "Search::Taxonomy::descend: key: |$k|" ) if $c->debug;
    
    # if there's an array of values...
    my $v = $query->{$k};
    if( ref($v) eq 'ARRAY' ) {

      $c->log->debug( 'Search::Taxonomy::descend: got an array' ) if $c->debug;

      # walk over the array of values
      foreach my $e ( @$v ) {

        # descend further down the tree
        my $f = $c->forward('descend', [ $e, $k ] );
        $f = undef if $f eq 0;

        # there's another level below this one
        if( keys %$collectedFamilies ) {
          if( $k eq '+' ) {
            $collectedFamilies = findCommon( $collectedFamilies, $f ); # AND
          } elsif( $k eq '-' ) {
            $collectedFamilies = uniquify(   $collectedFamilies, $f ); # NOT
          } else {
            $collectedFamilies = merge(      $collectedFamilies, $f ); # OR
          }          
        } else {
          $collectedFamilies = $f;
        }
      } # end of "foreach value"

    } elsif( $k eq 'value' ) {
      # there's a single value

      if( ref($v) eq 'HASH' ) {
        # still at a branch; recurse further
        $collectedFamilies = $c->forward('descend', [ $v, $operator ] );
        
      } else {
        # leaf node; $v is now a species term
        $collectedFamilies = $c->forward('getFamiliesForTerm', [ $v ] );
      } 
    } else {
      $c->log->warn( "Search::Taxonomy::descend: didn't do anything with |$k|$v|" );
    }
  }

  return $collectedFamilies if ref($collectedFamilies) eq 'HASH';
}

#-------------------------------------------------------------------------------

=head2 getRange : Private

Looks up the left and right values for a given species or taxonomic level.
If it finds both left and right values, they are returned as a reference to 
an array, "0" otherwise.

=cut

sub getRange : Private {
  my( $this, $c, $term ) = @_;
  
  # we want to get the left and right ranges for the term from the taxomony 
  # table
  
  # we need to remove double quotes added round the term by QueryParser
  $term =~ s/\"//g;

  $c->log->debug( "Search::Taxonomy::get_range: looking up term: |$term|" )
    if $c->debug;

  my $rs = $c->model('RfamDB::TaxonomyWebsearch')
             ->find( { species => $term } );   
  
  # $c->log->debug( "Search::Taxonomy::getRange: looking up term: |$term|" )
  #   if $c->debug;

  # return "0" by default, as this is what $c->forward will enforce anyway
  my $rv = 0;
  if( defined $rs and 
      $rs->lft and 
      $rs->rgt ) {
    $c->log->debug( 'Search::Taxonomy::getRange: got range for term: '
                    . '|' . $rs->lft . '|' . $rs->rgt . '|' )
      if $c->debug;   
    $rv = [ $rs->lft, $rs->rgt ];  
  }

  # return 0 if we don't get a range
  return $rv;
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

#-------------------------------------------------------------------------------

=head2 findCommon

Returns a reference to a hash with keys that are common to both input hashes. 
Note that the values in the hash are always from the first hash.

=cut

sub findCommon {
  my( $hashRef1, $hashRef2 ) = @_;

  my %common;
  foreach ( keys %$hashRef1 ) {
    $common{$_} = $hashRef1->{$_}
      if exists $hashRef2->{$_};
  }
  return \%common;
}

#-------------------------------------------------------------------------------

=head2 merge

Returns a reference to a hash containing the key/value pairs from the two input
hashes. The hashes are merged in the order hash1, hash2, so that duplicate keys
in hash2 will overwrite those from hash1. Values for duplicate keys are taken
from hash2.

=cut

sub merge {
  my( $hashRef1, $hashRef2 ) = @_;
  my %merged;
  foreach my $hashRef ( $hashRef1, $hashRef2 ) {
    while( my( $k, $v) = each %$hashRef ) {
      $merged{$k} = $v;
    }
  } 
  return \%merged
}

#-------------------------------------------------------------------------------

=head2 uniquify

Returns a reference to a hash containing those keys from the first input hash
that are not found in the second input hash. Values are taken from hash1.

=cut

sub uniquify {
  my( $hashRef1, $hashRef2 ) = @_;
  my %this_not_that;
  foreach ( keys %$hashRef1 ) {
    $this_not_that{$_} = $hashRef1->{$_}
      unless exists $hashRef2->{$_};
  }
  return \%this_not_that;
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
