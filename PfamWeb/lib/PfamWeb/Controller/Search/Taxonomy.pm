
# Taxonomy.pm
# jt6 20070918 WTSI
#
# $Id: Taxonomy.pm,v 1.4 2007-10-02 15:21:20 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Taxonomy - controller for taxonomy searches

=cut

package PfamWeb::Controller::Search::Taxonomy;

=head1 DESCRIPTION

A search controller for performing taxonomy searches

$Id: Taxonomy.pm,v 1.4 2007-10-02 15:21:20 jt6 Exp $

=cut

use strict;
use warnings;

use Search::QueryParser;
use Data::Dump qw( dump );

use base 'PfamWeb::Controller::Search';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 process : Path

Perform a taxonomy search

=cut

sub process : Path {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Search::Taxonomy::process: starting a taxonomy search' )
    if $c->debug;

  # make sure we got *something*
  unless( defined $c->req->param('q') ) {
    $c->stash->{taxSearchError} = 'You did not supply a query string.';
    return;
  }

  # make sure it was plain text
  # But how do we deal with a case like "Escherichia coli O157:H7"? 
  unless( $c->req->param('q') =~ m/^([\S().\s]+)$/ ) {
    $c->stash->{taxSearchError} = 'You did not supply a valid query string.';
    return;
  }
  my $query = $1;
  
  if( $query =~ /^unique (.*)$/i ){
    my $term = $1;
    $c->stash->{term} = $term;
    $c->log->debug( "Search::Taxonomy::process: Going to find unique matches for string: |$term|" )
      if $c->debug;
    my $allCountRef  = $c->forward('getAllFamilyCount');   
    my $termCountRef = $c->forward('getFamiliesForTerm');    
    
    my %uniqueFams;
    while( my( $k, $v ) = each %$termCountRef ) {
      # now see if the count is the same; if it is then it must be unique to 
      # the term
      if( $allCountRef->{$k} == $v ) {
         $uniqueFams{$k}++;
      }
    }

    # now stuff the list of unique families to the stash
    $c->stash->{uniqueFamsToTerm} = \%uniqueFams

  } else {
    # we got a valid string, but does it parse ?
    $c->log->debug( "Search::Taxonomy::process: found query string: |$query|" )
      if $c->debug;

    my $qp = new Search::QueryParser;
    my $pq = $qp->parse( $query );
    unless($pq){
      $c->stash->{taxSearchError} = 'Error parsing query string.';
      return;
    }
    
    $c->log->debug( 'Search::Taxonomy::process: parsed query: ', dump $pq )
      if $c->debug;
    $c->stash->{parsedQuery} = $pq;

    # walk down the tree
    my $famsRef = $c->forward('descend');

    $c->stash->{famsForTerm} = $famsRef;
    $c->log->debug( 'Search::Taxonomy::process: got the following families ',
                    dump $famsRef )
      if $c->debug;
  } 
  
  $c->stash->{template} = 'pages/search/taxonomy/results.tt';
}

#-------------------------------------------------------------------------------

=head2 suggest : Local

Returns a list of possible completions for species names as an HTML list.

This action is intended to be called only by an AJAX call from the taxonomy 
search form. The resulting list is presented as auto-completion lines under the 
form field, letting the user know immediately that they can't spell 
Caenorhabditis elegans after all...

=cut

sub suggest : Local {
  my( $this, $c ) = @_;

  # first, split up the search string into individual 'words'
  $c->forward( 'parseTerms' );

  #----------------------------------------

  # perform a database look-up for the LAST species name in the search string, 
  # if there is one at this point

  my( $rawSpecies, @speciesSuggestions );
  
  if( defined $c->stash->{terms}->[-1] ) {
    
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
  }
  
  #----------------------------------------

  # rebuild the search string using the suggestions

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
      if( $i >= $limit ) {
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

=head2 parseTerms : Private

Description...

=cut

sub parseTerms : Private {
  my( $this, $c ) = @_;

  # later we'll want to chomp to remove trailing spaces... 
  local( $/ ) = ' ';
  
  # detaint the parameter
  my( $q ) = $c->req->param('q') =~ m/^([\w\s\(\)\.]+)$/;

  # put spaces around braces to make sure we see them in the list of words when 
  # we split on spaces
  $q =~ s|([\(\)])| $1 |g;

  # strip leading and trailing spaces 
  $q =~ s/^\s*(.*?)\s*$/$1/;
  
  # break up the search term into "words"
  my @words = split /\s+/, $q;

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
        push @sentence, $term, 'AND NOT';
        $term = '';

      } else {
        chomp $term;
        push @sentence, $term, 'AND';
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

  # now we should have two arrays, one with the species names that we'll need
  # to search (@term), one with the whole search term broken up into 'words'
  # (@sentence)
  $c->stash->{terms}    = \@terms;
  $c->stash->{sentence} = \@sentence;
}

#-------------------------------------------------------------------------------

=head2 getAllFamilyCount : Private

Description...

=cut

sub getAllFamilyCount : Private {
  my( $this, $c ) = @_;  

  my @rs;
  if( $c->stash->{term} ) {
    
    my $range = $c->forward('getRange');
    
    $c->log->debug( 'Search::Taxonomy::getAllFamilyCount: getting count for |'
                    . $c->stash->{term} . "|, |$range->[0]|, |$range->[1]|" )
      if $c->debug;

    @rs = $c->model('PfamDB::Taxonomy')
            ->search( { lft => { '>=' => $range->[0] },
                        rgt => { '<=' => $range->[1] } },
                      { join     => [ 'pfamAncbi' ],
                        select   => [ 'pfamAncbi.pfamA_acc', 
                                      { count => 'pfamAncbi.auto_pfamA' } ],
                        as       => [ 'pfamA_acc', 'count' ],
                        group_by => [ 'pfamAncbi.auto_pfamA' ],
                      } );
  } else {

    $c->log->debug( 'Search::Taxonomy::getAllFamilyCount: no search term provided' )
      if $c->debug;

    @rs = $c->model('PfamDB::PfamA_ncbi')
            ->search( {},
                      { select   => [ 'pfamA_acc', 
                                      { count => 'auto_pfamA' } ],
                        as       => [ 'pfamA_acc', 'count' ],
                        group_by => [ 'me.auto_pfamA' ],
                      } );
  } 

  my %res = map{ $_->get_column('pfamA_acc') => $_->get_column('count') } @rs;

  $c->log->debug( 'Search::Taxonomy::getAllFamilyCount: got |'
                  . scalar( keys %res ) . '| family counts' )
    if $c->debug;

  return \%res;
}

#-------------------------------------------------------------------------------

=head2 getFamiliesForTerm : Private

Description...

=cut

sub getFamiliesForTerm : Private {
  my( $this, $c ) = @_;

  my $rv;
  
  if( $c->stash->{term} ) {
    
    my $range = $c->forward('getRange');

    $c->log->debug( "Search::Taxnomoy::getFamiliesForTerm: got lft: |$range->[0]|; "
                    . "got rgt: |$range->[1]|")
      if $c->debug;

    my @rs = $c->model('PfamDB::PfamA_ncbi')
               ->search( { 'tax.lft'    => { '>=' => $range->[0] },
                           'tax.rgt'    => { '<=' => $range->[1] } },
                         { join     => [ 'tax' ],
                           prefetch => [ 'tax' ] }
                       ); 

    my %res = map{ $_->pfamA_acc => 1 } @rs; 
    $rv = \%res;
  }
  
  return $rv;
}

#-------------------------------------------------------------------------------

=head2 descend : Private

Description...

=cut

sub descend : Private {
  my( $this, $c ) = @_;
  
  # take copies of everything, as the recursive nature of this code 
  # means that this data will be lost otherwise.
  my $query     = $c->stash->{parsedQuery};
  my $operator  = $c->stash->{operator} || '';
  my $famAllRef = $c->{allFams};

  # walk over these specific keys. Need to have this as a literal list in order
  # to fix the order of precedence
  foreach my $k ( '+', '-', '', 'value'){
    next unless $query->{$k};
    
    $c->log->debug( "Search::Taxonomy::descend: key: |$k|" ) if $c->debug;
    
    # there's an array of values
    my $v = $query->{$k};
    if( ref($v) eq 'ARRAY' ){

      $c->log->debug( 'Search::Taxonomy::descend: got an array' ) if $c->debug;
      $operator = $k;

      # walk over the values
      foreach my $e ( @$v ){
        $c->stash->{operator}    = $operator;
        $c->stash->{parsedQuery} = $e;

        # descend further down the tree
        my $f = $c->forward('descend');

        # there's another level below this one
        if( keys %$famAllRef ) {
          if( $operator eq '+' ) {
            $famAllRef = common( $famAllRef, $f ); # AND
          } elsif( $operator eq '-' ) {
            $famAllRef = unique( $famAllRef, $f ); # NOT
          } else {
            $famAllRef = merge( $famAllRef, $f );  # OR
          }          
        } else {
          $famAllRef = $f;
        }
      } # end of "foreach value"

    } elsif( $k eq 'value' ) {
      # there's a single value

      if( ref($v) eq 'HASH' ) {
        # reset the operators and query and descend down from here
        $c->stash->{operator}    = $operator;
        $c->stash->{parsedQuery} = $v;
        $c->forward('descend');
      } else {
        $c->stash->{term} = $v;
        $famAllRef = $c->forward('getFamiliesForTerm');
      } 
    } else {
      $c->log->warn( "Search::Taxonomy::descend: didn't do anything with |$k|$v|" );
    }
  }

  return $famAllRef if ref($famAllRef) eq 'HASH';
}

#-------------------------------------------------------------------------------

=head2 getRange: Private

Looks up the left and right values for a given species or taxonomic level.
If it finds both left and right values, they are returned as a reference to 
an array, undef otherwise.

=cut

sub getRange : Private {
  my( $this, $c ) = @_;
  
  # we want to get the left and right ranges for the term from the taxomony 
  # table
  
  # we need to remove double quotes added round the term by QueryParser
  $c->stash->{term} =~ s/\"//g;
  
  $c->log->debug( 'Search::Taxonomy::getRange: looking up term: |'
                  . $c->stash->{term} . '|' )
    if $c->debug;

  my $rs;
  if( $c->stash->{term} =~ m/\S+\s+\S+/ ){
    # looks like a species name
    $c->log->debug( 'Search::Taxonomy::getRange: looks like a species term')
      if $c->debug;
         
    $rs = $c->model('PfamDB::Taxonomy')
            ->find( { species => $c->stash->{term} } );  

  } elsif( $c->stash->{term} =~/\S+/ ) {
    # looks like a taxonomic level other than species.
    $c->log->debug( 'Search::Taxonomy::getRange: looks like a level term' )
      if $c->debug;

    $rs = $c->model('PfamDB::Taxonomy')
            ->find( { level => $c->stash->{term} } );
  }
  
  my $rv;
  if( $rs->lft and $rs->rgt ) {
    $c->log->debug( 'Search::Taxonomy::getRange: got range for term' );   
    $rv = [ $rs->lft, $rs->rgt ];  
  } else {
    $c->{taxSearchError} = 
      'Could not find ' . $c->stash->{term} . " in the database\n";
  }

  return $rv;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 common

Returns a reference to a hash with keys that are common to both input hashes. 
Note that the values in the hash are always 1.

=cut

sub common {
  my( $hashRef1, $hashRef2 ) = @_;

  my %common;
  foreach ( keys %$hashRef1 ) {
    $common{$_} = 1
      if exists $hashRef2->{$_};
  }
  return \%common;
}

#-------------------------------------------------------------------------------

=head2 merge

Returns a reference to a hash containing the key/value pairs from the two input
hashes. The hashes are merged in the order hash1, hash2, so that duplicate keys
in hash2 will overwrite those from hash1.

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

=head2 unique

Returns a reference to a hash containing those keys from the first input hash
that are not found in the second input hash.

=cut

sub unique {
  my( $hashRef1, $hashRef2 ) = @_;
  my %this_not_that;
  foreach ( keys %$hashRef1 ) {
    $this_not_that{$_} = 1
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

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;