
# Browse.pm
# jt6 20070704 WTSI
#
# $Id: Browse.pm,v 1.4 2009-11-17 10:22:47 pg6 Exp $

=head1 NAME

iPfamWeb::Controller::Browse - controller to build the "browse" pages

=cut

package iPfamWeb::Controller::Browse;

=head1 DESCRIPTION

Retrieves the data for the various "browse" pages.

Generates a B<full page>.

$Id: Browse.pm,v 1.4 2009-11-17 10:22:47 pg6 Exp $

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

=head2 browseFamilies : Path

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

sub browseFamilies : Path( '/family/browse' ) {
  my ( $this, $c ) = @_;

  my @res;

  # numbers
  if ( lc $c->req->param('browse') eq 'numbers' ) {
    $c->log->debug( 'Browse::browseFamilies: browsing "numbers"...' )
      if $c->debug;
    $c->stash->{char} = 'numbers';

    # run the query to get back all families starting with a number
    @res = $c->model('iPfamDB::Pfama')
             ->search( { pfama_id => { 'REGEXP', '^[0-9]' } },
                       { order_by => 'pfama_id ASC' } );
  }

  # a letter, presumably
  else {

    $c->log->debug( 'Browse::browseFamilies: not a number, not "top twenty"...' )
      if $c->debug;

    # see if we should load the page for families starting with a given letter
    my $char;
    
    if( defined $c->req->param( 'browse' ) ){
      ( $char ) = $c->req->param('browse') =~ /^(\w)$/ ;
    }
    
    if ( defined $char ) {
      $c->log->debug( "Browse::browseFamilies: browsing for a character: |$char|" )
        if $c->debug;
      $c->stash->{char} = uc $char;
  
      # run the query to get back all families starting with the
      # specified letter, ordered by ID
      @res = $c->model('iPfamDB::Pfama')
               ->search( { 
                           -and => [
                             pfama_id => { 'LIKE', qq($char%) },
                             -or => [
                               domCount => { '>', 0 },
                               ligCount => { '>', 0 },
                               naCount  => { '>', 0 },
                             ],
                           ],
                         },
                         { order_by => 'pfama_id' } );
    }
    else {

      # either "new" specified, or no starting letter specified, so default 
      # to new families anyway
      $c->log->debug( 'Browse::browseFamilies: browsing new entries' )
        if $c->debug;
      $c->stash->{char} = 'new';
  
      @res = $c->model('iPfamDB::Pfama')
               ->search( {},
                         { order_by => 'pfama_id ASC' } );
    }

  }

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

  # set the template and let the default end action from Section
  # render it for us
  if ( defined $c->req->param('list') and $c->req->param('list') ) {
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

sub browseLigands : Path( '/ligand/browse' ) {
  my ( $this, $c ) = @_;

  my @res;

  # numbers
  if ( lc $c->req->param('browse') eq 'numbers' ) {
    $c->log->debug( 'Browse::browseLigands: browsing "numbers"...' )
      if $c->debug;
    $c->stash->{char} = 'numbers';

    # run the query to get back all families starting with a number
    @res = $c->model('iPfamDB::LigandSummary')
             ->search( { three_letter_code => { 'REGEXP', '^[0-9]' },
                         domCount          => { '>', 0 } },
                       { order_by => 'three_letter_code' } );
  }

  # a letter, presumably
  else {

    $c->log->debug( 'Browse::browseLigands: not a number, not "top twenty"...' )
      if $c->debug;

    # see if we should load the page for families starting with a given letter
    my $char;
    if( defined $c->req->param( 'browse' ) ){
      ( $char ) = $c->req->param('browse') =~ /^(\w)$/;
    }

    if ( defined $char ) {
      $c->log->debug( "Browse::browseLigands: browsing for a character: |$char|" )
        if $c->debug;
      $c->stash->{char} = uc $char;
  
      # run the query to get back all families starting with the
      # specified letter, ordered by ID
      @res = $c->model('iPfamDB::LigandSummary')
               ->search( { three_letter_code => { 'LIKE', qq($char%) },
                           domCount          => { '>', 0 } },
                         { order_by => 'three_letter_code' } );
    }else{
      $c->log->debug( "Browse::browseLigands: load all the ligands present in the database");
      
      @res = $c->model( 'iPfamDB::LigandSummary' )
               ->search( { domCount          => { '>', 0 } },
                         { order_by =>  'three_letter_code ASC' }
               );
    }

  }

  # stash the results for the template
  $c->stash->{browse} = \@res if scalar @res;

  # set the template and let the default end action from Section
  # render it for us
  if ( defined $c->req->param('list') and $c->req->param('list') ) {
    # we want to return just the list of Pfam IDs, as a snippet of HTML.
    # This is used in the domain query search form
    $c->stash->{template} = 'pages/browse/ids.tt';
  }
  else {
    # just render as a regular 'browse' page
    $c->stash->{template} = 'pages/browse/ligands.tt';
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
