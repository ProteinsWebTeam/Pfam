
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

use strict;
use warnings;

use Data::Dump qw( dump );

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Caches the page and notifies the navbar of the location.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

  # stash the parameters, after checking that they're valid
  ( $c->stash->{$_} ) = $c->req->param($_) || '' =~ m/^(\w)$/
    for qw( to from top20 numbers );

  # set the page to be cached for one week
  #$c->cache_page( 604800 );

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

  my $cache_key = 'active_letters_hash';
  my $active_letters = $c->cache->get( $cache_key );
  if ( defined $active_letters ) {
    $c->log->debug( 'Browse::browse: retrieved active letters list from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Browse::browse: failed to retrieve active letters list from cache; going to DB' )
      if $c->debug;

    # get a list of all genomes, then hash them on kingdom and species name first
    # letter
    my @res = $c->model('RfamDB::GenomeSummary')
                ->search( {},
                          { order_by => 'species ASC' } );

    $active_letters = { all => {} };
    foreach my $kingdom ( keys %{ $this->{kingdoms} } ) {
      $active_letters->{$kingdom} = {};
    }

    foreach my $genome_row ( @res ) {
      my $first_letter = uc( substr( $genome_row->species, 0, 1 ) );
      $active_letters->{$genome_row->kingdom}->{$first_letter} = 1;
      $active_letters->{all}->{$first_letter} = 1;
    }

    $c->cache->set( $cache_key, $active_letters );
  }

  $c->stash->{active_letters} = $active_letters;

  #$c->log->debug( "Browse::browse: active_letters: " . dump( $active_letters ) )
  #  if $c->debug;

  $c->stash->{template} ||= 'pages/browse/index.tt';
}

#-------------------------------------------------------------------------------
#- genomes ---------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 browse_genomes : Path

Retrieves the list of genomes from the DB and stashes them for the template.

=cut

sub browse_genomes : Chained( '/' )
                     PathPart( 'genome' )
                     CaptureArgs( 0 ) {
  my( $this, $c ) = @_;

  $c->log->debug( 'Genome::browse_genomes: building a list of genomes' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 browse_genomes : Path

Retrieves the full list of genomes from the DB and stashes them for the 
template.

=cut

sub browse_list : Chained( 'browse_genomes' )
                  PathPart( 'browse' )
                  Args( 0 ) {
  my( $this, $c ) = @_;

  $c->log->debug( 'Genome::browse_list: building full list of genomes' )
    if $c->debug;

  my @res = $c->model('RfamDB::GenomeSummary')
              ->search( {},
                        { order_by => 'species ASC' } );

  $c->log->debug( 'Genome::browse_list: found ' . scalar @res
                  . ' genomes' ) if $c->debug;

  # stash the results for the template
  $c->stash->{genomes} = \@res if scalar @res;

  # render the page
  $c->stash->{template} = 'pages/browse/genomes.tt';
}

#-------------------------------------------------------------------------------

=head2 browse_by_kingdom : Path

Retrieves the list of genomes from a particular kingdom and stashes them for 
the template.

=cut

sub browse_by_kingdom : Chained( 'browse_genomes' )
                        PathPart( 'browse' )
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

  # render the page
  $c->stash->{template} = 'pages/browse/genomes.tt';
}

#-------------------------------------------------------------------------------
#- families --------------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 browse_families : Chained PathPart CaptureArgs

Start of a chain for building the "browse families" pages.

=cut

sub browse_families : Chained( '/' )
                      PathPart( 'family' )
                      CaptureArgs( 0 ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Browse::browse_families: building a list of families' )
    if $c->debug;

                        
  $c->stash->{template} = 'pages/browse/families.tt';
}

#-------------------------------------------------------------------------------

=head2 browse_letter : Chained PathPart Args

Build a page showing the list of families starting with a particular 
letter/number. End of a dispatch chain.

=cut

sub browse_letter : Chained( 'browse_families' )
                    PathPart( 'browse' )
                    Args( 1 ) {
  my ( $this, $c, $tainted_letter ) = @_;
  
  if ( $tainted_letter eq 'top20' ) {
    $c->log->debug( 'Browse::browse_letter: showing top 20 largest families' )
      if $c->debug;

    my @rs = $c->model('RfamDB::Rfam')
               ->search( { },
                         { rows     => 20,
                           page     => 1,
                           order_by => 'num_full DESC' }
                         )->all;
  
    $c->log->debug( 'Browse::browse_letter: got |' . scalar @rs
                    . '| top20 families' ) if $c->debug;
  
    $c->stash->{top20}    = 1;
    $c->stash->{families} = \@rs;
  }
}

#-------------------------------------------------------------------------------

=head2 browse_range : Chained PathPart Args

Build a page showing the list of families starting with a particular 
letter/number. End of a dispatch chain.

=cut

sub browse_range : Chained( 'browse_families' ) 
                   PathPart( 'browse' )
                   Args( 2 ) {
  my ( $this, $c, $tainted_a, $tainted_b ) = @_;

  my ( $a ) = $tainted_a =~ m/^(\w)$/;
  my ( $b ) = $tainted_b =~ m/^(\w)$/;
  
  my ( $from, $to ) = sort ( $a, $b );
  
  $c->log->debug( "Browse::browse_range: from / to: |$from|$to|" ) if $c->debug;

  # according to the DBIC docs, it's bad to use an SQL function on the left-
  # hand side of a comparison like this (here we're doing a SUBSTRING), because
  # it means that the DB engine has to scan the whole table. Because the Rfam 
  # table is likely to contain only on the order of thousands of rows (unless 
  # something drastic happens), we should be okay. 

  my @families = $c->model('RfamDB::Rfam')
                   ->search( { 'SUBSTRING(rfam_id,1,1)'  => { 'IN', [ $from .. $to ] } },
                             { order_by => 'rfam_id' } );                   

  $c->log->debug( 'Browse::browse_range: found |' . scalar @families
                  . '| families in total' ) if $c->debug;

  $c->stash->{from}     = $from;
  $c->stash->{to}       = $to;
  $c->stash->{families} = \@families;
  $c->stash->{template} = 'pages/browse/families.tt';
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
