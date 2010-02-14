
# Browse.pm
# jt6 20070704 WTSI
#
# $Id: Browse.pm,v 1.12 2009-10-27 13:40:05 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Browse - controller to build the "browse" pages

=cut

package PfamWeb::Controller::Browse;

=head1 DESCRIPTION

Retrieves the data for the various "browse" pages.

Generates a B<full page>.

$Id: Browse.pm,v 1.12 2009-10-27 13:40:05 jt6 Exp $

=cut

use strict;
use warnings;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

Caches the page and notifies the navbar of the location.

=cut

sub begin : Private {
  my( $this, $c ) = @_;

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

  $c->stash->{template} = 'pages/browse/index.tt';
}

#-------------------------------------------------------------------------------

=head2 browse_families : Path

Retrieves data for the specified set of families. We check the value of the 
"browse" parameter for some special values, as well as initial letters:

=over

=item numbers

show the families whose ID starts with a number

=item top twenty

show the top twenty largest families (in terms of sequences in the full 
alignment)

=item new (default)

show families that are new in the current Pfam release

=item [A-Za-z]

show families beginning with the specified letter

=back

=cut

sub browse_families : Path( '/family/browse' ) {
  my ( $this, $c ) = @_;

  my @res;

  if ( lc $c->req->param('browse') eq 'numbers' ) {
    $c->log->debug( 'Browse::browse_families: browsing "numbers"...' )
      if $c->debug;
    $c->stash->{char} = 'numbers';

    # run the query to get back all families starting with a number
    @res = $c->model('PfamDB::Pfama')
             ->search( { pfama_id => { 'REGEXP', '^[0-9]' } },
                       { order_by => 'pfama_id ASC' } );
  }
  elsif ( lc $c->req->param('browse') =~ m/top.*?twenty/ ) {
    $c->log->debug( 'Browse::browse_families: browsing "top twenty"...' );
    $c->stash->{char} = 'top twenty';

    # retrieve the top twenty largest families, ordered by number of
    # sequences in the family
    @res = $c->model('PfamDB::Pfama')
             ->search( { },
                       { rows     => 20,
                         page     => 1,
                         order_by => 'num_full DESC' } )
             ->all;
  }
  else {

    $c->log->debug( 'Browse::browse_families: not a number, not "top twenty"...' );

    # see if we should load the page for families starting with a given letter
    my $char;
    ( $char ) = $c->req->param('browse') =~ /^(\w{1})$/;

    if ( defined $char ) {
      $c->log->debug( "Browse::browse_families: browsing for a character: |$char|" );
      $c->stash->{char} = uc $char;
  
      # run the query to get back all families starting with the
      # specified letter, ordered by ID
      @res = $c->model('PfamDB::Pfama')
               ->search( { pfama_id => { 'LIKE', qq($char%) } },
                         { order_by => 'pfama_id' } );
    }
    else {

      # either "new" specified, or no starting letter specified, so default 
      # to new families anyway
      $c->log->debug( 'Browse::browse_families: browsing new entries' );
      $c->stash->{char} = 'new';
  
      @res = $c->model('PfamDB::Pfama')
               ->search( { change_status => 'NEW' },
                         { order_by => 'pfama_id ASC' } );
    }
  }

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

  # set the template and let the default end action from Section
  # render it for us
  if ( $c->req->param('list') ) {
    # we want to return just the list of Pfam IDs, as a snippet of HTML.
    # This is used in the domain query search form
    $c->stash->{template} = 'pages/browse/ids.tt';
  }
  else {
    # just render as a regular 'browse' page
    $c->stash->{template} = 'pages/browse/families.tt';
  }
}

#-------------------------------------------------------------------------------

=head2 browse_clans : Path

Retrieves data for the clans browse page.

=cut

sub browse_clans : Path( '/clan/browse' ) {
  my ( $this, $c ) = @_;

  my @res = $c->model('PfamDB::Clans')
              ->search( {},
                			  { order_by => 'clan_id ASC' } );

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

  # render the page
	$c->stash->{template} = 'pages/browse/clans.tt';
}

#-------------------------------------------------------------------------------

=head2 browse_proteomes : Path

Retrieves the list of proteomes from the DB and stashes them for the template.

=cut

sub browse_proteomes : Path( '/proteome/browse' ) {
  my( $this, $c ) = @_;

  my @res = $c->model('PfamDB::CompleteProteomes')
              ->search( {},
                        { prefetch => [ qw( ncbi_taxid ) ],
                          order_by => 'me.species ASC' } );

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

  # render the page
  $c->stash->{template} = 'pages/browse/proteomes.tt';
}

#-------------------------------------------------------------------------------

=head2 browseMetaseq : Path

Retrieves the list of metaseq datasets from the DB and stashes them for the 
template.

=cut

sub browseMetaseq : Path( '/metaseq/browse' ) {
  my( $this, $c ) = @_;

  my @res = $c->model('PfamDB::MetagenomicsRefs')
              ->search( {},
                        { select => [ { distinct => [ 'source' ] }, 'long_source' ],
                          as     => [ qw( src long_src ) ],
                          order_by => 'source ASC' } );

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

  # render the page
  $c->stash->{template} = 'pages/browse/metaseq.tt';
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
