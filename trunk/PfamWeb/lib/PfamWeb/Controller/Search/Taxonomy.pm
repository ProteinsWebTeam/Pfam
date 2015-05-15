
# Taxonomy.pm
# jt6 20070918 WTSI
#
# $Id: Taxonomy.pm,v 1.12 2009-10-28 11:57:19 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Taxonomy - controller for taxonomy searches

=cut

package PfamWeb::Controller::Search::Taxonomy;

=head1 DESCRIPTION

A search controller for performing taxonomy searches

$Id: Taxonomy.pm,v 1.12 2009-10-28 11:57:19 jt6 Exp $

=cut

use strict;
use warnings;

use URI::Escape;
use Search::QueryParser;
use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 taxonomy : Path

Accepts the input from the taxonomy search form and returns the results page.
The results page doesn't actually contain results but it does fire off an 
AJAX request to get results. This action handles both the taxonomy boolean
search and the unique domains search.

=cut

sub taxonomy : Path( '/search/taxonomy' ) {
  my ( $this, $c ) = @_;
  
  # detaint the input
  my $query = $c->forward('detaint_query_string', [ $c->req->param('q') ] );

  # there's something wrong with the basic string
  unless ( $query ) {
    $c->log->debug( 'Search::Taxonomy::taxonomy: problem with queryString' )
      if $c->debug;

    $c->stash->{taxSearchError} = $c->stash->{queryStringError};

    return;
  }
  
  # stash what we have so far
  $c->stash->{q} = $query;
  
  # call parse_terms, to parse the query string and return it with the species 
  # names quoted. That method also puts two arrays into the stash: @terms 
  # stores the raw terms, @sentence stores the terms plus the boolean operators 
  # and braces

  # We need to do this to make sure that we have only a single term in the 
  # query, and so that we can return the error message from this action rather
  # than the one that's triggered by the AJAX call. Because we don't care about
  # the quoted query string, we don't need to keep hold of the return value of
  # the "parse_terms" action this time
  $c->forward('parse_terms', [ $query ] );

  # make sure we got *some* terms in the stash
  unless ( defined $c->stash->{terms} and
           scalar @{ $c->stash->{terms} } ) {
    $c->log->debug( 'Search::Taxonomy::taxonomy: no terms found' )
      if $c->debug;

    $c->stash->{taxSearchError} =
      'We did not find any species names in the string that you entered: "' . 
      $query . '"';

    return;
  }

  # see if we should be looking for families that are unique to a single species
  if ( $c->req->param('unique') ) {

    $c->log->debug( 'Search::Taxonomy::taxonomy: checking for a single term' )
      if $c->debug;

    # we should have only one species name in the query
    unless ( scalar @{ $c->stash->{terms} } == 1 ) {
      $c->stash->{taxSearchError} = 'You can only find unique families for a single species.'
        . ' Please enter only one species name and try again.';
      return;
    }
    
    $c->log->debug( 'Search::Taxonomy::taxonomy: handing off to unique search template' )
      if $c->debug;
    $c->stash->{template} = 'pages/search/taxonomy/uniqueResults.tt';  

  }
  else {

    $c->log->debug( 'Search::Taxonomy::taxonomy: handing off to taxonomy search template' )
      if $c->debug;
    $c->stash->{template} = 'pages/search/taxonomy/taxonomyResults.tt';
  }
}

#-------------------------------------------------------------------------------

=head2 taxonomy_query : Path

Performs a taxonomy search and retrieves Pfam-A families selected by the
user-supplied boolean query strings, such as

  "Caenorhabditis elegans AND NOT Homo sapiens"

Renders a template that generates a page fragment, which is intended to be
inserted into a page via an AJAX call.

=cut

sub taxonomy_query : Path( '/search/taxonomy/results' ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Search::Taxonomy::taxonomy_query: starting a taxonomy search' )
    if $c->debug;

  # detaint the input
  my $query = $c->forward('detaint_query_string', [ $c->req->param('q') ] );

  unless ( $query ) {
    $c->log->debug( 'Search::Taxonomy::taxonomy_query: problem with queryString' )
      if $c->debug;

    $c->stash->{taxSearchError} = $c->stash->{queryStringError};

    return;
  }
  
  # drop the original, but detainted, query string into the hash for the 
  # template to use later
  $c->stash->{queryString} = $query;
  
  #----------------------------------------
  
  # call parse_terms, to parse the query string and return it with the species 
  # names quoted. That method also puts two arrays into the stash: @terms 
  # stores the raw terms, @sentence stores the terms plus the boolean operators 
  # and braces
  
  my $quotedQuery = $c->forward('parse_terms', [ $query ] );

  #----------------------------------------
  
  # we got a valid string, but does it parse ?
  $c->log->debug( "Search::Taxonomy::taxonomy_query: got quoted query string: |$quotedQuery|" )
    if $c->debug;

  # see if we can retrieve this result from cache. Need to get rid of the 
  # stranger characters in the quoted string, like quotes, for a start...
  my $cacheKey = 'taxonomySearch' . $quotedQuery;
  $cacheKey =~ s/[\W\s]/_/g;
  my $families = $c->cache->get( $cacheKey );
  
  if ( defined $families ) {
    $c->log->debug( 'Search::Taxonomy::taxonomy_query: retrieved families from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Search::Taxonomy::taxonomy_query: failed to retrieve families from cache; going to DB' )
      if $c->debug;
   
    my $qp = new Search::QueryParser;
    my $pq = $qp->parse( $quotedQuery );
    unless ( $pq ) {
      $c->stash->{taxSearchError} = 'There was a problem with your query. '
        . 'Please check that you entered a valid boolean query and try again.';
      return;
    }
      
    $c->log->debug( 'Search::Taxonomy::taxonomy_query: parsed query: ', dump $pq )
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

sub uniqueQuery : Path( '/search/unique/results' ) {
  my( $this, $c ) = @_;

  $c->log->debug( 'Search::Taxonomy::uniqueQuery: starting a unique family search' )
    if $c->debug;

  # set up the template early. The same template will handle both results and
  # error messages
  $c->stash->{template} = 'pages/search/taxonomy/uniqueSearchResults.tt';

  #----------------------------------------

  # detaint the input
  my $query = $c->forward('detaint_query_string', [ $c->req->param('q') ] );

  unless( $query ) {
    $c->log->debug( 'Search::Taxonomy::uniqueQuery: problem with queryString' )
      if $c->debug;
    $c->stash->{taxSearchError} = $c->stash->{queryStringError};
    return;
  }
  
  # drop the original, but detainted, query string into the hash for the 
  # template to use later
  $c->stash->{queryString} = $query;
  
  #----------------------------------------
  
  # call parse_terms, to parse the query string and return it with the species 
  # names quoted. That method also puts two arrays into the stash: @terms 
  # stores the raw terms, @sentence stores the terms plus the boolean operators 
  # and braces
  
  my $quotedQuery = $c->forward('parse_terms', [ $query ] );

  # we should have only one species name in the query
  if( scalar @{ $c->stash->{terms} } > 1 ) {
    $c->log->debug( 'Search::Taxonomy::uniqueQuery: multiple terms specified; detaching' );
    $c->stash->{uniqueSearchError} = 
      'You can only find unique families for a single species. ' .
      'Please enter just one species name and try again.';
    return;
  }
  
  $c->log->debug( "Search::Taxonomy::uniqueQuery: finding unique matches for |$quotedQuery|" )
    if $c->debug;

  #----------------------------------------

  # see if we can retrieve this result from cache. Need to get rid of the 
  # stranger characters in the quoted string, like quotes, for a start...
  my $cacheKey = 'uniqueFamilies' . $quotedQuery;
  $cacheKey =~ s/[\W\s]/_/g;
  my $unique = $c->cache->get( $cacheKey );
  
  if( defined $unique ) {
    $c->log->debug( 'Search::Taxonomy::uniqueQuery: retrieved unique families from cache' )
      if $c->debug;
  } else { 
    $c->log->debug( 'Search::Taxonomy::uniqueQuery: failed to retrieve unique families from cache; going to DB' )
      if $c->debug;

    # get the hash containing the information we need about each family
    my $familyInfo = $c->forward('get_family_info');

    my $allCount  = $c->forward('getAllFamilyCount');
    my $termCount = $c->forward('getFamilyCount', [ $quotedQuery ] );

    # some error checking
    
    # see if there was a problem down the line with getting the lft and rgt 
    # values from the species tree table
    if( $c->stash->{rangeError} ) {
      $c->log->debug( 'Search::Taxonomy::uniqueQuery: problem when getting range' )
        if $c->debug;
      $c->stash->{uniqueSearchError} = 
        'There was a problem with your query. ' .
        'We could not find the species that you specified.';
      return;
    }

    # see if we got back some term counts 
    unless( defined $termCount and 
            scalar( keys %$termCount ) ) {
      $c->log->debug( 'Search::Taxonomy::uniqueQuery: no term counts retrieved' )
        if $c->debug;
      $c->stash->{uniqueSearchError} = 
        'There was a problem with your query. ' .
        'We did not find any species matching your query';
      return;
    }

    $unique = {};
    while( my( $k, $v ) = each %$termCount ) {
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

sub suggest : Local {
  my( $this, $c ) = @_;

  # detaint the input
  my $query = $c->forward('detaint_query_string', [ $c->req->param('q') ] );

  # split up the search string into individual 'words'. We don't care about
  # the return value, which gives the search string with the species names
  # quoted
  $c->forward( 'parse_terms', [ $query ] );

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
    my @species = $c->model('PfamDB::Taxonomy')
                    ->search_like( { species => "$rawSpecies%" } );
    foreach ( @species ) {
      push @speciesSuggestions, $_->species;
    }
                 
    # collect the list of matching levels, which are distinct from the actual
    # species name    
    my @levels  = $c->model('PfamDB::Taxonomy')
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

=head2 detaint_query_string : Private

Checks the input string and returns the detainted version or, if there was a 
problem with the string, returns "0" (actually, calls via c<forward> always
return "0" by default) and puts the error message into the stash as 
"queryStringError".

=cut

sub detaint_query_string : Private {
  my ( $this, $c, $query_string ) = @_;
  
  # make sure we got *something*
  unless ( defined $query_string ) {
    $c->stash->{query_stringError} = 'There was a problem with your query. '
                                    . 'You did not supply a query string.';
    return 0;
  }

  # make sure it was plain text
  unless ( $query_string =~ m/^([\w\s():\-%\.]+)$/ ) {
    $c->log->debug( "Search::Taxonomy::detaint_query_string: rejecting query string: |$query_string|" )
      if $c->debug;
    $c->stash->{query_stringError} = 'There was a problem with your query. '
      . 'Please check that there are no illegal characters in your query string and try again.';
    return 0;
  }
  my $encoded_query_string = $1;
  
  # decode the string, since it could have been encoded by the javascript 
  # function "encodeURI"
  my $decoded_query_string = uri_unescape( $encoded_query_string );

  # make sure we ended up with something after decoding it... We don't allow
  # "%" now, since that should have been there only to allow URI encoding
  unless( $decoded_query_string =~ m/^([\w\s():\-\.]+)$/ ) {
    $c->log->debug( "Search::Taxonomy::detaint_query_string: rejecting detainted query string: |$decoded_query_string|" )
      if $c->debug;
    $c->stash->{query_stringError} = 'There was a problem checking your query. '
      . 'Please check that there are no illegal characters in your query string and try again.';
    return 0;
  }
    
  $c->log->debug( "Search::Taxonomy::detaint_query_string: passed query string: |$decoded_query_string|" )
    if $c->debug;
    
  return $decoded_query_string;
}

#-------------------------------------------------------------------------------

=head2 parse_terms : Private

Parses the query string into two arrays, @terms and @sentence, containing the 
species names and all of the tokens within the query respectively. The two 
arrays are dropped into the stash. Returns the query string with the species 
names quoted.

=cut

sub parse_terms : Private {
  my ( $this, $c, $query ) = @_;

  # later we'll want to chomp to remove trailing spaces... 
  local $/ = ' ';
  
  # put spaces around braces to make sure we see them in the list of words when 
  # we split on spaces
  $query =~ s|([\(\)])| $1 |g;

  # strip leading and trailing spaces 
  $query =~ s/^\s*(.*?)\s*$/$1/;
  $c->log->debug( "Search::Taxonomy::parse_terms: starting with search term: |$query|" )
    if $c->debug;
  
  # break up the search term into "words"
  my @words = split /\s+/, $query;
  
  #----------------------------------------
  
  # next, filter the terms to collect the actual species names separately from
  # the boolean terms and braces. We'll also need to keep track of the order
  # of everything though, so that we can reconstruct the whole string, corrected
  # by the searches

  my ( @terms, @sentence );
  my $term = '';
  foreach my $index ( 0 .. $#words ) {
    my $word = $words[$index];

    # filter out the debris of merging AND and NOT
    next if not defined $word;

    # and actually merge AND and NOT...
    if ( $word =~ /^AND$/i ) {
      
      # check if the next word in the sentence is NOT and, if it is, merge
      # it with the current AND before deleting the NOT from the sentence
      # altogether
      if ( $words[$index + 1] and 
          $words[$index + 1] eq 'NOT' ) {
        delete $words[$index + 1];
        chomp $term;
        push @sentence, $term, 'AND NOT';
        push @terms, $term;
        $term = '';

      }
      else {
        chomp $term;
        push @sentence, $term, 'AND';
        push @terms, $term;
        $term = '';
      }

    }
    elsif ( $word =~ m/^(OR|NOT|\(|\))$/i ) {

      # just stuff this term (OR, NOT or braces) into the sentence, but ignore
      # terms that are just composed of spaces
      unless ( $term =~ /^\s*$/ ) {
        chomp $term;
        push @sentence, $term;
        push @terms, $term;
      }
      push @sentence, uc $word;
      $term = '';

    }
    else {
      # the word that we have is a search term; add it to the growing search 
      # term string
      $term .= $word . ' ';
    }

  }

  # finally, if we had a non-whitespace term at the end of the search string, 
  # push in it into the list
  unless ( $term =~ /^\s*$/ ) {
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
  if ( scalar @terms ) {
    $c->log->debug( 'Search::Taxonomy::parse_terms: retrieved some terms from the query' )
      if $c->debug;
    $c->stash->{terms}    = \@terms;
    $c->stash->{sentence} = \@sentence;    
  }
  else {
    $c->log->warn( 'Search::Taxonomy::parse_terms: no search terms retrieved' )
      if $c->debug;
  }
  
  # finally, put double-quotes around each of the search terms in the input
  # string and return that
  foreach my $term ( @{ $c->stash->{terms} } ) {
    next unless $term;
    $query =~ s/($term)/"$1"/ig;
  }
  
  return $query;
}

#-------------------------------------------------------------------------------

=head2 getFamilyCount : Private

Retrieves the list of families in a given species. Returns a reference to a 
hash containing the Pfam-A accession and the count of the number of families.

=cut

sub getFamilyCount : Private {
  my( $this, $c, $term ) = @_;  

  # see if we can retrieve the families from cache
  my $cacheKey = 'familyCount' . $term;
  $cacheKey =~ s/[\W\s]/_/g;
  $c->log->debug( "Search::Taxonomy::getFamilyCount: cacheKey: |$cacheKey|" )
    if $c->debug;
  my $termCount = $c->cache->get( $cacheKey );

  if( defined $termCount ) {
    $c->log->debug( 'Search::Taxonomy::getFamilyCount: retrieved family counts from cache' )
      if $c->debug;
  } else {
    $c->log->debug( 'Search::Taxonomy::getFamilyCount: failed to retrieve family counts from cache; going to DB' )
      if $c->debug;

    my $range = $c->forward('get_range', [ $term ] );
    
    if( not $range ) {
      $c->log->debug( 'Search::Taxonomy::getFamilyCount: range error' );
      $c->stash->{rangeError} = "Could not find $term";
      return;
    }

    $c->log->debug( 'Search::Taxonomy::getFamilyCount: getting count for '
                    . "|$term|, |$range->[0]|, |$range->[1]|" )
      if $c->debug;
  
    my @rs = $c->model('PfamDB::Taxonomy')
               ->search( { lft => { '>=' => $range->[0] },
                           rgt => { '<=' => $range->[1] } },
                         { join     => [ 'pfama_ncbi' ],
                           select   => [ 'pfama_ncbi.pfama_acc', 
                                         { count => 'pfama_ncbi.pfama_acc' } ],
                           as       => [ 'pfama_acc', 'count' ],
                           group_by => [ 'pfama_ncbi.pfama_acc' ],
                         } );

    foreach ( @rs ) {
      next unless( defined $_->get_column('pfama_acc') and
                   defined $_->get_column('count') );
      $termCount->{ $_->get_column('pfama_acc') } = $_->get_column('count');
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

Retrieves, for each Pfam-A family, the count of the number of times that family
occurs for each species. Returns a reference to a hash with Pfam-A accession 
and count.

=cut

sub getAllFamilyCount : Private {
  my( $this, $c ) = @_;  

  my $cacheKey = 'allFamilyCount';
  $c->log->debug( "Search::Taxonomy::getAllFamilyCount: cacheKey: |$cacheKey|" )
    if $c->debug;
  my $res      = $c->cache->get( $cacheKey );

  if( defined $res ) {
    $c->log->debug( 'Search::Taxonomy::getAllFamilyCount: retrieved family counts from cache' )
      if $c->debug;
  } else {
    $c->log->debug( 'Search::Taxonomy::getAllFamilyCount: failed to retrieve family counts from cache; going to DB' )
      if $c->debug;

    my @rs = $c->model('PfamDB::PfamaNcbi')
               ->search( {},
                         { select   => [ 'pfama_acc', 
                                         { count => 'auto_pfama' } ],
                           as       => [ 'pfama_acc', 'count' ],
                           group_by => [ 'me.auto_pfama' ],
                         } );
  
    # hash the results
    foreach ( @rs ) {
      next unless( defined $_->get_column('pfama_acc') and
                   defined $_->get_column('count') );
      $res->{ $_->get_column('pfama_acc') } = $_->get_column('count');
    }
    
    # and cache them
    $c->cache->set( $cacheKey, $res ) unless $ENV{NO_CACHE};
  }

  return $res;
}

#-------------------------------------------------------------------------------

=head2 get_families_for_term : Private

Returns the families that are found for the single specified taxonomic term. 
This is used only by the "descend" method, to build the list of Pfam-A families 
for a set of species.

The first job is to find the range, the lft and rgt values, for the given
species in the table that stores the species tree. Given that range we can
find the sub-tree under that node, from which we get the families for
those species.

=cut

sub get_families_for_term : Private {
  my ( $this, $c, $term ) = @_;
  
  # see if we can retrieve the families for this species from cache
  my $cacheKey = 'familiesForTerm' . $term;
  $cacheKey =~ s/[\W\s]/_/g;  
  $c->log->debug( "Search::Taxonomy::get_families_for_term: cacheKey: |$cacheKey|" )
    if $c->debug;
  my $res = $c->cache->get( $cacheKey );
  
  if ( defined $res ) {
    $c->log->debug( 'Search::Taxonomy::get_families_for_term: retrieved families from cache' )
      if $c->debug;
  }
  else { 

    # find the left and right values for this specific species name
    my $range = $c->forward('get_range', [ $term ] );
  
    # "get_range" returns 0 if it couldn't find a range for the given term, or
    # a hash ref if it found the lft/rgt values
  
    # return "0" by default, as this is what $c->forward will enforce anyway
    return 0 unless ref($range) eq 'ARRAY';

    # get the hash containing the information we need about each family
    my $familyInfo = $c->forward('get_family_info');
  
    $c->log->debug( 'Search::Taxonomy::get_families_for_term: failed to retrieve families from cache; going to DB' )
      if $c->debug;

    my @rs = $c->model('PfamDB::PfamaNcbi')
               ->search( { 'taxonomy.lft' => { '>=' => $range->[0] },
                           'taxonomy.rgt' => { '<=' => $range->[1] } },
                         { select   => [ 'pfama_acc' ],
                           as       => [ 'pfama_acc' ],
                           join     => [ qw(taxonomy) ] }
                       );
  
    $c->log->debug( 'Search::Taxonomy::get_families_for_term: found |'
                    . scalar @rs . '| rows for tax query' )
      if $c->debug;

    # map the Pfam-A accession to a hash with other information for the family
    my %res = map{ $_->get_column('pfama_acc') => $familyInfo->{$_->get_column('pfama_acc')} } @rs;

    $c->log->debug( 'Search::Taxonomy::get_families_for_term: found |'
                    . scalar( keys %res ) . '| families' )
      if $c->debug;
      
    $res = \%res;
      
    # cache the result
    $c->cache->set( $cacheKey, $res ) unless $ENV{NO_CACHE};
  }
  
  return $res;
}

#-------------------------------------------------------------------------------

=head2 get_family_info : Private

Retrieves items of information for all Pfam families and stores them in a hash.
Tries to retrieve the hash from cache before hitting the database. Currently the
hash is keyed on Pfam-A accession and contains references to hashes with the
accession, ID and description of the family.

This hash is used to add a little more information to the results of the 
taxonomy searches.

=cut

sub get_family_info : Private {
  my ( $this, $c ) = @_;

  my $cacheKey = 'family_info';
  my $family_info = $c->cache->get( $cacheKey );

  if ( defined $family_info ) {
    $c->log->debug( 'Search::Taxonomy::get_family_info: retrieved family info from cache' );
  } 
  else {
    $c->log->debug( 'Search::Taxonomy::get_family_info: failed to retrieve family info from cache; going to DB' );

    my @rs = $c->model('PfamDB::Pfama')
               ->search( {},
                         { select => [ qw( pfama_acc pfama_id description ) ] } );

    my %family_info = map { $_->pfama_acc => { pfama_acc   => $_->pfama_acc,
                                               pfama_id    => $_->pfama_id,
                                               description => $_->description } } @rs;

    $c->log->debug( 'Search::Taxonomy::get_family_info: found info for |'
                    . scalar( keys %family_info ). '| families' );

    $family_info = \%family_info;
    $c->cache->set( $cacheKey, $family_info ) unless $ENV{NO_CACHE};
  }

  return $family_info;
}

#-------------------------------------------------------------------------------

=head2 descend : Private

Walks down the tree generated by parsing the taxonomy query and runs searches 
for each of the species terms found. Collects the resulting species 
name-to-NCBI code mapping in a hash and returns the reference. 

=cut

sub descend : Private {
  my ( $this, $c, $query, $operator ) = @_;

  my $collectedFamilies = {};

  # walk over these specific keys. Need to have this as a literal list in order
  # to fix the order of precedence
  foreach my $k ( '+', '-', '', 'value'){
    next unless $query->{$k};
    
    $c->log->debug( "Search::Taxonomy::descend: key: |$k|" ) if $c->debug;
    
    # if there's an array of values...
    my $v = $query->{$k};
    if ( ref($v) eq 'ARRAY' ) {

      $c->log->debug( 'Search::Taxonomy::descend: got an array' ) if $c->debug;

      # walk over the array of values
      foreach my $e ( @$v ) {

        # descend further down the tree
        my $f = $c->forward('descend', [ $e, $k ] );
        $f = undef if $f eq 0;

        # there's another level below this one
        if ( keys %$collectedFamilies ) {
          if ( $k eq '+' ) {
            $collectedFamilies = findCommon( $collectedFamilies, $f ); # AND
          }
          elsif( $k eq '-' ) {
            $collectedFamilies = uniquify(   $collectedFamilies, $f ); # NOT
          }
          else {
            $collectedFamilies = merge(      $collectedFamilies, $f ); # OR
          }          
        }
        else {
          $collectedFamilies = $f;
        }
      } # end of "foreach value"

    }
    elsif ( $k eq 'value' ) {
      # there's a single value

      if ( ref($v) eq 'HASH' ) {
        # still at a branch; recurse further
        $collectedFamilies = $c->forward('descend', [ $v, $operator ] );
        
      } 
      else {
        # leaf node; $v is now a species term
        $collectedFamilies = $c->forward('get_families_for_term', [ $v ] );
      } 
    }
    else {
      $c->log->warn( "Search::Taxonomy::descend: didn't do anything with |$k|$v|" );
    }
  }

  return $collectedFamilies if ref($collectedFamilies) eq 'HASH';
}

#-------------------------------------------------------------------------------

=head2 get_range : Private

Looks up the left and right values for a given species or taxonomic level.
If it finds both left and right values, they are returned as a reference to 
an array, "0" otherwise.

=cut

sub get_range : Private {
  my( $this, $c, $term ) = @_;
  
  # we want to get the left and right ranges for the term from the taxomony 
  # table
  
  # we need to remove double quotes added round the term by QueryParser
  $term =~ s/\"//g;
  
  $c->log->debug( "Search::Taxonomy::get_range: looking up term: |$term|" )
    if $c->debug;

  my $rs = $c->model('PfamDB::Taxonomy')
          ->find( { species => $term } );  
  
  # return "0" by default, as this is what $c->forward will enforce anyway
  my $rv = 0;
  if( defined $rs and 
      $rs->lft and 
      $rs->rgt ) {
    $c->log->debug( 'Search::Taxonomy::get_range: got range for term: '
                    . '|' . $rs->lft . '|' . $rs->rgt . '|' )
      if $c->debug;   
    $rv = [ $rs->lft, $rs->rgt ];  
  }

  # return 0 if we don't get a range
  return $rv;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
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
