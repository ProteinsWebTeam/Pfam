
# Taxonomy.pm
# jt6 20070918 WTSI
#
# $Id: Taxonomy.pm,v 1.3 2007-10-01 15:56:06 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Search::Taxonomy - controller for taxonomy searches

=cut

package PfamWeb::Controller::Search::Taxonomy;

=head1 DESCRIPTION

A search controller for performing taxonomy searches

$Id: Taxonomy.pm,v 1.3 2007-10-01 15:56:06 jt6 Exp $

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
  
  $c->log->debug( 'Search::Taxonomy::process: starting a taxonomy search' );

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
    $c->log->debug( "Search::Taxonomy::process: Going to find unique matches for string: |$term|" );
    my $allCountRef  = $c->forward('getAllFamilyCount');   
    my $termCountRef = $c->forward('getAllFamilyCount');
    
    
    my %uniqueFams;
    while (my ( $k, $v ) = each( %$termCountRef )){
      #Now see if the count is the same, if it is then it must be unique to the term. 
      if( $$allCountRef{$k} == $v ) {
         $uniqueFams{$k}++;
      }
    }
    #Now stuff the list of unique families to the stash 
    $c->stash->{uniqueFamsToTerm} = \%uniqueFams
  }else{
    $c->log->debug( "Search::Taxonomy::process: found query string: |$query|" );
    # we got a valid string, but does it parse ?
    my $qp = new Search::QueryParser;
    my $pq = $qp->parse( $1 );
    unless($pq){
       $c->stash->{taxSearchError} = 'Error parsing query string.';
       return;
    }  
    $c->log->debug( 'Search::Taxonomy::process: parsed query: ', dump $pq );
    $c->{parsedQuery} = $pq;
    my $famsRef = $c->forward('descend');
    $c->stash->{famsForTerm} = $famsRef;
    $c->log->debug( 'Search::Taxonomy::process: got the following families ', dump $famsRef);
  } 
  
  #Set the templter
  $c->stash->{template} = 'pages/search/taxonomy/results.tt';
}

#-------------------------------------------------------------------------------

=head2 action : Attribute

Suggest something why don't you...

=cut

sub suggest : Local {
  my( $this, $c ) = @_;

  local( $/ ) = ' ';
  
  # detaint the parameter
  my( $q ) = $c->req->param('q') =~ m/^([\w\s\(\)]+)$/;
  $c->log->debug( "Search::Taxonomy::suggest: got raw parameter; q = |$q|" );

  # put spaces around braces to make sure we see them in the list of words when 
  # we split on spaces
  $q =~ s|([\(\)])| $1 |g;
  $c->log->debug( "Search::Taxonomy::suggest: braces padded: |$q|" );

  # strip leading and trailing spaces 
  $q =~ s/^\s*(.*?)\s*$/$1/;
  $c->log->debug( "Search::Taxonomy::suggest: leading/trailing spaces stripped: |$q|" );
  
  # break up the search term into "words"
  my @words = split /\s+/, $q;
  foreach ( @words ) {
    $c->log->debug( "Search::Taxonomy::suggest: split word: |$_|" );
  }

  my( @terms, @sentence );
  my $term = '';
  foreach my $index ( 0 .. $#words ) {
    my $word = $words[$index];

    # filter out the debris of merging AND and NOT
    if( not defined $word ) {
      $c->log->debug( 'Search::Taxonomy::suggest: skipping null word' );
      next;
    }
    
    $c->log->debug( "Search::Taxonomy::suggest: raw word: |$word|" );

    if( $word =~ /^AND$/i ) {
      $c->log->debug( 'Search::Taxonomy::suggest: found AND' );
      
      if( $words[$index + 1] eq 'NOT' ) {
        $c->log->debug( 'Search::Taxonomy::suggest:   next term is NOT' );
        delete $words[$index + 1];
        push @sentence, $term, 'AND NOT';
        $term = '';

      } else {
        chomp $term;
        push @sentence, $term, 'AND';
        $c->log->debug( "Search::Taxonomy::suggest: pushed term |$term|" );
        $term = '';
      }

    } elsif( $word =~ m/^(OR|NOT|\(|\))$/i ) {

      $c->log->debug( "Search::Taxonomy::suggest: found OR, NOT or braces ('$word')" );
      unless( $term =~ /^\s*$/ ) {
        chomp $term;
        push @sentence, $term;
        push @terms, $term;
        $c->log->debug( "Search::Taxonomy::suggest: pushed term |$term|" );
      }
      push @sentence, $word;
      $term = '';

    } else {
      
      $term .= $word . ' ';
      $c->log->debug( "Search::Taxonomy::suggest: term is now |$term|" );
    }

  }
  # push in the last term in the list, which will be otherwise omitted    
  chomp $term;
  push @sentence, $term;
  push @terms, $term;
  $c->log->debug( "Search::Taxonomy::suggest: pushed term |$term|" );

  my $responseString = '<ul>';
  foreach ( @terms ) {
    $c->log->debug( "Search::Taxonomy::suggest: term: |$_|" );
    $responseString .= "<li>$_</li>";
  }
  $responseString .= '</ul>';

  foreach ( @sentence ) {
    $c->log->debug( "Search::Taxonomy::suggest: sentence: |$_|" );
  }

  $c->log->debug( "Search::Taxonomy::suggest: built response string: |$responseString|" );

  $c->res->body( $responseString );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

# none

sub getAllFamilyCount : Private {
  my( $this, $c ) = @_;  
    
  my (@rs, %res);
  if($c->{term}){
    my ($l, $r) = $c->forward('_getRange');
    $c->log->debug( "Getting count for ".$c->{term}.", $l, $r");
    @rs = $c->model("PfamDB::Taxonomy")
                    ->search({  "lft"    => { ">=" => $l },
                                "rgt"    => { "<=" => $r }},
                              { join     => [ qw(pfamAncbi) ],
                                select    => [ qw( pfamAncbi.pfamA_acc
                                                   ), 
                                                 { count => 'pfamAncbi.auto_pfamA' } ],
                                as        => [ qw( pfamA_acc 
                                                   count ) ],
                                group_by => [ qw( pfamAncbi.auto_pfamA ) ],
                                                   });
  }else{
    @rs = $c->model("PfamDB::PfamA_ncbi")
                    ->search( {},
                              {
                                select    => [ qw( pfamA_acc ), 
                                                 { count => 'auto_pfamA' } ],
                                as        => [ qw( pfamA_acc count ) ],
                                group_by => [ qw( me.auto_pfamA ) ],
                                                   });
      print "Getting count for all\n";
  } 
  %res = map{$_->get_column("pfamA_acc") => $_->get_column("count")} @rs;
  print $c->log->debug("Search::Taxonomy::getAllFamiliesCoun  Got: ".scalar(keys(%res))." family counts");
  return \%res;
}

sub getFamiliesForTerm : Private {
  my( $this, $c ) = @_;
  
  if($c->{term}){
    
    
    my $lrRef = $c->forward('_getRange');
    my $lft = $$lrRef[0];
    my $rgt = $$lrRef[1];
   #  my ($lft, $rgt) = $c->forward('_getRange');
    $c->log->debug("Got lft:|".$lft."|, Got rgt:|".$rgt."|");
   my @rs = $c->model("PfamDB::PfamA_ncbi")
                  ->search({  "tax.lft"    => { '>=' => $lft },
                              "tax.rgt"    => { '<=' => $rgt } },
                            { join     => [ qw(tax) ],
                              prefetch => [ qw(tax)] }); 
  my %res = map{$_->pfamA_acc => 1} @rs; 
  return \%res;
  
  }
  
  
}

sub descend : Private {
  my( $this, $c ) = @_;
  
  #Take copies of everything, as the recursive nature of this code 
  #means that this data will be lost otherwise.
  my $query = $c->{parsedQuery};
  my $operator = $c->{operator} if($c->{operator});
  my $famAllRef = $c->{allFams};
  
  foreach my $k ("+", "-", "", "value"){
    next unless($$query{$k});
    $c->log->debug("Search::Taxonomy::descend key = $k");
    my $v = $$query{$k};   
    if(ref($v) eq "ARRAY" ){
        $c->log->debug("*** array ***");
        $operator = $k;
        
        foreach my $e (@$v){
           $c->{operator} = $operator;
           $c->{parsedQuery} = $e;
           my $f = $c->forward('descend');
           if(keys %$famAllRef){
              if($operator eq "+"){
                 $famAllRef = &common($famAllRef, $f);
              }elsif($operator eq "-"){
                #NOT
                $famAllRef = &unique($famAllRef, $f);
              }else{
                #Match OR
                $famAllRef = &merge($famAllRef, $f);
              } 
           }else{
              $famAllRef = $f; 
           }
           
        }
        next;
        #return $famAllRef;
    }elsif(($k) eq "value"){
      if( ref($v) eq "HASH"){
        $c->{operator} = $operator;
        $c->{parsedQuery} = $v;
        my $f = $c->forward('descend');
      }else{
        #print "Terminal hash?\n"; 
        #print "|$operator|";
        #print dump( $v ), "\n";
        $c->{term} = $v;
        $famAllRef = $c->forward('getFamiliesForTerm');
      } 
    }else{
      #print "Did not do anything with $k, $v\n";
    }
  }
  
  return ($famAllRef) if(ref($famAllRef) eq "HASH");
}

sub _getRange : Private {
  my( $this, $c ) = @_;
  #We want to get the left and right ranges for the term from the taxomony table.
  my $rs;
  
  #We need to remove double quotes added round the term by QueryPaser
  $c->{term} =~ s/\"//g;
  $c->log->debug("Search::Taxonomy::_getRange looking up term|".$c->{term}."|");
  if($c->{term} =~/\S+\s+\S+/){
    #Looks like a spcies name
    $c->log->debug("Search::Taxonomy::_getRange, looks like a species term");   
    $rs = $c->model("PfamDB::Taxonomy")
                    ->find({ "species" => $c->{term}});  
  }elsif($c->{term} =~/\S+/){
    #Looks like a taxonomic level other than species.
    $c->log->debug("Search::Taxonomy::_getRange, looks like a level term");      
    $rs = $c->model("PfamDB::Taxonomy")
                  ->find({ "level" => $c->{term}});
  }
  
  if($rs->lft and $rs->rgt){
    $c->log->debug("Search::Taxonomy::_getRange, got range for term!");   
    return ([ $rs->lft, $rs->rgt ]);  
  }else{
    $c->{taxSearchError} = "Could not find ".$c->{term}." in the database\n";
    return;
  }
}

sub common {
  my ($hashRef1, $hashRef2) = @_;

  print dump( $hashRef1, $hashRef2)."\n";
  my %common;
  foreach (keys %{$hashRef1}) {
        $common{$_} = 1 if exists $$hashRef2{$_};
  }
  # %common now contains keys 
  return \%common;
}

sub merge {
  my ($hashRef1, $hashRef2) = @_;
  my %merged;
  foreach my $hashRef ( $hashRef1, $hashRef2 ) {
    while (my($k, $v) = each %$hashRef) {
        $merged{$k} = $v;
    }
  } 
  return \%merged
}


sub unique {
 my ($hashRef1, $hashRef2) = @_;
 my %this_not_that;
  foreach (keys %$hashRef1) {
    $this_not_that{$_} = 1 unless exists $$hashRef2{$_};
  }
  return \%this_not_that;
}


=head1 METHODS

=head2 process : Path

Perform a taxonomy search

=cut






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