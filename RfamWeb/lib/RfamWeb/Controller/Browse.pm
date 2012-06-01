
# Browse.pm
# jt6 20080314 WTSI
#
# $Id: Browse.pm,v 1.3 2009-01-06 11:51:13 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Browse - controller to build the "browse" pages

=cut

package RfamWeb::Controller::Browse;

=head1 DESCRIPTION

Retrieves the data for the various "browse" pages.

Generates a B<full page>.

$Id: Browse.pm,v 1.3 2009-01-06 11:51:13 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

BEGIN {
  extends 'Catalyst::Controller';
}

use Data::Dump qw( dump );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Caches the page and notifies the navbar of the location.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;

  # stash the parameters, after checking that they're valid
  ( $c->stash->{$_} ) = $c->req->param($_) || '' =~ m/^(\w)$/
    for qw( to from top20 numbers );

  # set the page to be cached for one week
  $c->cache_page( 604800 );

  # tell the navbar where we are
  $c->stash->{nav} = 'browse';
}

#-------------------------------------------------------------------------------

=head2 browse : Global

Show an index page for the various "browse" pages.

=cut

sub browse : Global {
  my ( $this, $c ) = @_;

  # copy the kingdoms list into the stash so that we can use them to build the
  $c->stash->{kingdoms} = [ sort keys %{ $this->{kingdoms} } ]; 

  $c->forward( 'build_active_letters' );

  $c->stash->{template} ||= 'pages/browse/index.tt';
}

#-------------------------------------------------------------------------------
#- clans -----------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 browse_clans : Chained PathPart CaptureArgs

Retrieves the list of clans from the DB and stashes them for the template.

=cut

sub browse_clans : Chained( '/' )
                   PathPart( 'clans' )
                   CaptureArgs( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Browse::browse_clans: building a list of clans' )
    if $c->debug;
    
  $c->stash->{template} = 'pages/browse/clans.tt';
}

#-------------------------------------------------------------------------------

=head2 browse_clans_list : Chained PathPart Args

Retrieves the full list of clans from the DB and stashes them for the 
template.

=cut

sub browse_clans_list : Chained( 'browse_clans' )
                        PathPart( '' )
                        Args( 0 ) {
  my ( $this, $c ) = @_;

  # we need the "active_letters" data structure, so that we can decide
  # which families have structures
  $c->forward( 'build_active_letters' );

  $c->log->debug( 'Browse::browse_clans_list: building full list of clans' )
    if $c->debug;

  my @res = $c->model('RfamDB::Clans')
              ->search( {},
                        { prefetch  => [ 'clan_memberships' ],
                          '+select' => [ { count => 'clan_memberships.auto_rfam' } ],
                          '+as'     => [ 'num_families' ],
                          group_by  => [ 'clan_memberships.auto_clan' ],
                          order_by  => [ 'clan_id ASC' ] } );

  $c->log->debug( 'Browse::browse_clans_list: found ' . scalar @res
                  . ' clans' ) if $c->debug;

  # stash the results for the template
  $c->stash->{clans} = \@res if scalar @res;
}

#-------------------------------------------------------------------------------
#- genomes ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 browse_genomes : Chained PathPart CaptureArgs

Retrieves the list of genomes from the DB and stashes them for the template.

=cut

sub browse_genomes : Chained( '/' )
                     PathPart( 'genomes' )
                     CaptureArgs( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Browse::browse_genomes: building a list of genomes' )
    if $c->debug;
    
  $c->stash->{template} = 'pages/browse/genomes.tt';
}

#-------------------------------------------------------------------------------

=head2 browse_genomes_list : Chained PathPart Args

Retrieves the full list of genomes from the DB and stashes them for the 
template.

=cut

sub browse_genomes_list : Chained( 'browse_genomes' )
                          PathPart( '' )
                          Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Browse::browse_genome_list: building full list of genomes' )
    if $c->debug;

  my @res = $c->model('RfamDB::GenomeSummary')
              ->search( {},
                        { order_by => 'species ASC' } );

  $c->log->debug( 'Browse::browse_genome_list: found ' . scalar @res
                  . ' genomes' ) if $c->debug;

  # stash the results for the template
  $c->stash->{genomes} = \@res if scalar @res;
}

#-------------------------------------------------------------------------------

=head2 browse_genomes_by_kingdom : Chained PathPart Args

Retrieves the list of genomes from a particular kingdom and stashes them for 
the template.

=cut

sub browse_genomes_by_kingdom : Chained( 'browse_genomes' )
                                PathPart( '' )
                                Args( 1 ) {
  my ( $this, $c, $tainted_kingdom ) = @_;

  my ( $kingdom ) = $tainted_kingdom =~ m/^([\w\-\.\"\']+)/;
  unless ( defined $kingdom ) {
    $c->log->debug( 'Genome::browse_by_kingdom: no kingdom name found' )
      if $c->debug;  

    $c->stash->{errorMsg} = 'You must give a valid kingdom name';
    
    return;
  }

  $c->log->debug( "Genome::browse_by_kingdom: looking for kingdom |$kingdom|" )
    if $c->debug;  

  unless ( defined $this->{kingdoms}->{$kingdom} ) {
    $c->log->debug( 'Genome::browse_by_kingdom: unknown kingdom name found' )
      if $c->debug;  

    $c->stash->{errorMsg} = 'Unknown kingdom';
    
    return;
  }

  $c->log->debug( "Genome::browse_by_kingdom: building list of genomes for '$kingdom'" )
    if $c->debug;

  $c->stash->{kingdom} = $kingdom;

  my @res = $c->model('RfamDB::GenomeSummary')
              ->search( { kingdom => $kingdom },
                        { order_by => 'species ASC' } );

  $c->log->debug( 'Genome::browse_by_kingdom: found ' . scalar @res
                  . " genomes for '$kingdom'" ) if $c->debug;

  # stash the results for the template
  $c->stash->{genomes} = \@res if scalar @res;
}

#-------------------------------------------------------------------------------
#- families --------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 browse_families : Chained PathPart CaptureArgs

Start of a chain for building the "browse families" pages.

=cut

sub browse_families : Chained( '/' )
                      PathPart( 'families' )
                      CaptureArgs( 0 ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Browse::browse_families: building a list of families' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 browse_all_families : Chained PathPart Args

Build a page showing the list of all Rfam families. End of a dispatch chain.

=cut

sub browse_all_families : Chained( 'browse_families' )
                          PathPart( '' )
                          Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Browse::browse_all_families: showing all families' )
      if $c->debug;

  # we need the "active_letters" data structure
  $c->forward( 'build_active_letters' );
  
  my @rs = $c->model('RfamDB::Rfam')
             ->search( { 'alignments_and_trees.type' => 'seed' },
                       { prefetch => 'alignments_and_trees',
                         order_by => 'rfam_id' }
                     )->all;

  $c->log->debug( 'Browse::browse_all_families: got |' . scalar @rs
                  . '| families' ) if $c->debug;

  $c->stash->{all}      = 1;
  $c->stash->{families} = \@rs;
  $c->stash->{template} = 'pages/browse/all_families.tt';
}

#-------------------------------------------------------------------------------

=head2 browse_letter : Chained PathPart Args

Build a page showing the list of families starting with a particular 
letter/number. End of a dispatch chain.

=cut

sub browse_families_by_letter : Chained( 'browse_families' )
                                PathPart( '' )
                                Args( 1 ) {
  my ( $this, $c, $tainted_letter ) = @_;

  # we need the "active_letters" data structure
  $c->forward( 'build_active_letters' );
  
  if ( defined $tainted_letter and
       $tainted_letter eq 'with_structure' ) {
    $c->log->debug( 'Browse::browse_families_by_letter: building a list of families with structures' )
      if $c->debug;
  
    my @rs = $c->model( 'RfamDB::PdbRfamReg' )
               ->search( { 'alignments_and_trees.type' => 'seed' },
                         { prefetch => { 'auto_rfam' => 'alignments_and_trees' },
                           '+select'=> [ { count => 'auto_rfam.auto_rfam' } ],
                           '+as'    => [ 'num_structures' ],
                           group_by => [ 'auto_rfam.rfam_id' ],
                           order_by => 'auto_rfam.rfam_id' } );
    
    $c->stash->{families} = \@rs;
    $c->stash->{template} = 'pages/browse/structures.tt';
  }
  elsif ( defined $tainted_letter and 
          $tainted_letter eq 'top20' ) {
    $c->log->debug( 'Browse::browse_families_by_letter: showing top 20 largest families' )
      if $c->debug;

    my @rs = $c->model('RfamDB::Rfam')
               ->search( { 'alignments_and_trees.type' => 'seed' },
                         { prefetch => 'alignments_and_trees',
                           rows     => 20,
                           page     => 1,
                           order_by => 'num_full DESC' }
                         )->all;
  
    $c->log->debug( 'Browse::browse_families_by_letter: got |' . scalar @rs
                    . '| top20 families' ) if $c->debug;
  
    $c->stash->{top20}    = 1;
    $c->stash->{families} = \@rs;
    $c->stash->{template} = 'pages/browse/families.tt';
  }
  else {
    # if there's no letter we shouldn't be here at all. The dispatcher should
    # have already fired 'browse_all_families' for us
    $c->forward( 'browse_all_families' );
  }
}

#-------------------------------------------------------------------------------

=head2 browse_families_range : Chained PathPart Args

Build a page showing the list of families starting with a particular 
letter/number. End of a dispatch chain.

=cut

sub browse_families_range : Chained( 'browse_families' ) 
                            PathPart( '' )
                            Args( 2 ) {
  my ( $this, $c, $tainted_a, $tainted_b ) = @_;

  my ( $a ) = $tainted_a =~ m/^(\w)$/;
  my ( $b ) = $tainted_b =~ m/^(\w)$/;
  
  my ( $from, $to ) = sort ( $a, $b );
  
  $c->log->debug( "Browse::browse_families_range: from / to: |$from|$to|" ) if $c->debug;

  # according to the DBIC docs, it's bad to use an SQL function on the left-
  # hand side of a comparison like this (here we're doing a SUBSTRING), because
  # it means that the DB engine has to scan the whole table. Because the Rfam 
  # table is likely to contain only on the order of thousands of rows (unless 
  # something drastic happens), we should be okay. 

  my @families = $c->model('RfamDB::Rfam')
                   ->search( { 'SUBSTRING(rfam_id,1,1)'  => { 'IN', [ $from .. $to ] },
                               'alignments_and_trees.type' => 'seed' },
                             { prefetch => [ 'alignments_and_trees' ],
                               order_by => 'rfam_id' } );        

  $c->log->debug( 'Browse::browse_families_range: found |' . scalar @families
                  . '| families in total' ) if $c->debug;

  $c->stash->{from}     = $from;
  $c->stash->{to}       = $to;
  $c->stash->{families} = \@families;
  $c->stash->{template} = 'pages/browse/families.tt';
}

#-------------------------------------------------------------------------------
#- wikipedia articles  ---------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 browse_articles : Chained PathPart CaptureArgs

Start of a chain for building the "browse articles" pages.

=cut

sub browse_articles : Chained( '/' )
                      PathPart( 'articles' )
                      CaptureArgs( 0 ) { }

#-------------------------------------------------------------------------------

=head2 browse_all_articles : Chained PathPart Args

Build a page showing the list of all wikipedia articles. End of a dispatch 
chain.

=cut

sub browse_all_articles : Chained( 'browse_articles' )
                          PathPart( '' )
                          Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Browse::browse_articles: building a list of articles' )
    if $c->debug;

  # decide if we're outputting text or HTML
  if ( $c->req->param('output' ) ) {

    if ( $c->req->param('output') eq 'text' ) {
      $c->log->debug( 'Browse::browse_articles: outputting a text list' )
        if $c->debug;
      $c->res->content_type( 'text/plain' );
      $c->res->header( 'Content-disposition' => "attachment; filename=rfam_wikipedia_articles.txt" );
      $c->stash->{template} = 'pages/browse/all_articles_text.tt';
    }
    elsif ( $c->req->param('output') eq 'list' ) {
      $c->log->debug( 'Browse::browse_articles: outputting a tabular list' )
        if $c->debug;
      $c->stash->{template} = 'pages/browse/all_articles_columns.tt';
    }

  }
  else {
    $c->stash->{template} = 'pages/browse/all_articles.tt';
  }

  my $article_mapping_and_letters = $c->forward('build_articles_list');

  $c->stash->{articles}       = $article_mapping_and_letters->{mapping};
  $c->stash->{active_letters} = $article_mapping_and_letters->{active_letters};
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 build_active_letters : Private

Builds a data structure that can be used for building the lists of first-letters
in the various browse pages.

=cut

sub build_active_letters : Private {
  my ( $this, $c ) = @_;
  
  my $cache_key = 'browse_active_letters_hash';
  my $active_letters = $c->cache->get( $cache_key );
  if ( defined $active_letters ) {
    $c->log->debug( 'Browse::build_active_letters: retrieved active letters list from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Browse::build_active_letters: failed to retrieve active letters list from cache; going to DB' )
      if $c->debug;

    #----------------------------------------
    
    # get a list of all families and join to pdb_rfam_reg, so that we can find
    # those families for which there's a 3-D structure
    my @families = $c->model( 'RfamDB::Rfam' )
                     ->search( {},
                               { prefetch => [ 'pdb_rfam_regs' ] } );
                              
    my $first_letter;
    foreach my $family ( @families ) {
      $first_letter = uc( substr( $family->rfam_id, 0, 1 ) );
      $first_letter = '0 - 9' if $first_letter =~ m/^\d+$/;
      $active_letters->{families}->{$first_letter} = 1;
      $active_letters->{families_with_structures}->{$first_letter} = 1
        if $family->pdb_rfam_regs->count;
    }

    #----------------------------------------

    # clans
    my @clans = $c->model('RfamDB::Clans')
                  ->search();

    foreach my $clan ( @clans ) {
      $first_letter = uc( substr( $clan->clan_id, 0, 1 ) );
      $first_letter = '0 - 9' if $first_letter =~ m/^\d+$/;
      $active_letters->{clans}->{$first_letter} = 1;
    }

    #----------------------------------------

    # genomes
    my @genomes = $c->model('RfamDB::GenomeSummary')
                    ->search( {}, {} );

    foreach my $genome ( @genomes ) {
      $first_letter = uc( substr( $genome->species, 0, 1 ) );
      $first_letter = '0 - 9' if $first_letter =~ m/^\d+$/;
      $active_letters->{genomes}->{$first_letter} = 1;
    }

    #----------------------------------------

    # articles
    my $articles_cache_key = 'article_mapping';
    my $article_mapping_and_letters = $c->cache->get( $articles_cache_key );
    if ( defined $article_mapping_and_letters ) {
      $c->log->debug( 'Browse::build_active_letters: retrieved article mapping from cache' )
        if $c->debug;
    }
    else {
      $c->log->debug( 'Browse::build_active_letters: failed to retrieve article mapping from cache; going to DB' )
        if $c->debug;
      $article_mapping_and_letters = $c->forward('build_articles_list');
      $c->cache->set( $articles_cache_key, $article_mapping_and_letters ) unless $ENV{NO_CACHE};
    }

    $active_letters->{articles} = $article_mapping_and_letters->{active_letters};
    $c->stash->{articles}       = $article_mapping_and_letters->{mapping};

    #----------------------------------------
    
    $c->cache->set( $cache_key, $active_letters ) unless $ENV{NO_CACHE};
  }

  $c->stash->{active_letters} = $active_letters;
}

#-------------------------------------------------------------------------------

=head2 build_articles_list : Private

Builds a data structure containing the mapping between families and articles, 
as well as the list of active letters for the articles list.

=cut

sub build_articles_list : Private {
  my ( $this, $c ) = @_;

  my @rs = $c->model('WebUser::ArticleMapping')
             ->search( { accession                    => { like => 'RF%' },
                         'wikitext.approved_revision' => { '>'  => 0 } },
                       { join => [ 'wikitext' ] } );

  $c->log->debug( 'Browse::build_articles_list: got |' . scalar @rs . '| rows' )
    if $c->debug;

  my $first_letter;
  my $mapping = {};
  my $active_letters = {};
  
  foreach my $row ( @rs ) {
    push @{ $mapping->{$row->title} }, $row->accession;
    $first_letter = uc( substr( $row->title, 0, 1 ) );
    $first_letter = '0 - 9' if $first_letter =~ m/^\d+$/;
    $active_letters->{$first_letter} = 1;
  }

  return { mapping        => $mapping,
           active_letters => $active_letters };
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
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
