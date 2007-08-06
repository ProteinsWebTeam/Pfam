
# Clan.pm
# jt6 20060411 WTSI
#
# Controller to build the main Pfam clans page.
#
# $Id: Relationship.pm,v 1.1 2007-08-06 16:11:23 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Clan - controller for clan-related
sections of the site

=cut

package PfamWeb::Controller::Clan::Relationship;

=head1 DESCRIPTION

This is intended to be the base class for everything related to clans
across the site. The L<begin|/"begin : Private"> method will try to
extract a clan ID or accession from the captured URL and then try to
load a Clan object from the model into the stash.

Generates a B<tabbed page>.

$Id: Relationship.pm,v 1.1 2007-08-06 16:11:23 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;

use base 'PfamWeb::Controller::Clan';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

We get here after the begin method from the parent class has handled
the extraction of the clan accession from the URL, so we can jump
straight in and retrieve the image map and relationship diagram image.

=cut

sub auto : Private {
  my( $this, $c ) = @_;

  $c->forward( 'getDiagram' );

  return ( defined $c->stash->{map} and
           defined $c->stash->{image} );
}

#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 image : Local

Serves the image file for the clan relationship diagram.

=cut

sub image : Local {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Clan::Relationship::image: serving clan relationship image' );
  
  if( defined $c->stash->{image} ) {
    $c->res->content_type( 'image/gif' );
    $c->res->body( $c->stash->{image} );
  } else {
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->uri_for( '/static/images/blank.gif' ) );
  }

}

#-------------------------------------------------------------------------------

=head2 map : Local

Serves the image map for the clan relationship diagram.

=cut

sub map : Local {
  my( $this, $c ) = @_;
  
  $c->log->debug( 'Clan::Relationship::image: serving clan relationship image map' );
  
  if( defined $c->stash->{map} ) {
    $c->res->body( $c->stash->{map} );
  } else {
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->uri_for( '/static/images/blank.gif' ) );
  }

}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getDiagram : Private

Retrieves the two components of the clan relationship diagram from the DB, 
namely the image showing the relationship and the HTML snippet with the 
image map.

=cut

sub getDiagram : Private {
  my( $this, $c ) = @_;

  my $cacheKeyRoot = 'clanRelationship' . $c->stash->{acc};

  my $image = $c->cache->get( $cacheKeyRoot . 'image' );
  my $map   = $c->cache->get( $cacheKeyRoot . 'map' );

  if( defined $image and defined $map ) { 
    $c->log->debug( 'Clan::Relationship::getDiagram: extracted image and map from cache' );
  } else {
    $c->log->debug( 'Clan::Relationship::getDiagram: failed to extract both image '
                    . 'and map from cache; going to DB' );

    my $row = $c->model('PfamDB::ClanRelationship')
                ->find( $c->stash->{clan}->auto_clan );
  
    # check we actually retrieved a row
    unless( defined $row->relationship ) {
      $c->stash->{errorMsg} = 'We could not retrieve the relationship data for '
                              . $c->stash->{acc};
      return;
    }
  
    # we'll need both the image and the image map HTML uncompressed
    $image = Compress::Zlib::memGunzip( $row->relationship );
    unless( defined $image ) {  
      $c->stash->{errorMsg} = 'We could not extract the relationship image for '
                              . $c->stash->{acc};
      return;
    }
  
    $map = Compress::Zlib::memGunzip( $row->image_map );
    unless( defined $map ) {  
      $c->stash->{errorMsg} = 'We could not extract the relationship image map for '
                              . $c->stash->{acc};
      return;
    }

    $c->cache->set( $cacheKeyRoot . 'image', $image );
    $c->cache->set( $cacheKeyRoot . 'map',   $map );

  }

  $c->stash->{image} = $image;
  $c->stash->{map}   = $map;
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
