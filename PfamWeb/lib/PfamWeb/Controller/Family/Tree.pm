
# Tree.pm
# jt6 20060511 WTSI
#
# $Id: Tree.pm,v 1.11 2007-07-27 10:57:27 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Tree;

Controller to build the tree view of the family.

=cut

package PfamWeb::Controller::Family::Tree;

=head1 DESCRIPTION

Uses treefam drawing code to generate images of the tree for
a given family.

$Id: Tree.pm,v 1.11 2007-07-27 10:57:27 jt6 Exp $

=cut

use strict;
use warnings;

use treefam::nhx_plot;

use base 'PfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

We get here after the begin method from the parent class has handled
the extraction of the family accession from the URL, so we can jump
straight in and generate the tree.

Tries to retrieve it from cache before actually generating it though.

=cut

sub auto : Private {
  my( $this, $c ) = @_;

  # find out what type of tree to draw, seed or full
  if( defined $c->req->param('alnType') ) {
    $c->stash->{alnType} = ( $c->req->param( 'alnType' ) eq 'seed' ) ? 'seed' : 'full';
    $c->log->debug( 'Family::Tree::auto: setting alnType to |' . 
                    $c->stash->{alnType} . '|' );
  } else {
    $c->stash->{alnType} = 'seed';
    $c->log->debug( 'Family::Tree::auto: no alnType parameter; defaulting to seed ' );
  }  
  
  # get hold of the tree data, either from cache or (currently) file
  $c->forward( 'getTree' );
}

#-------------------------------------------------------------------------------

=head2 showTree : Path

Plots the tree that we loaded in "auto" and hands off to a template that
builds HTML for the image and associated image map.

=cut

sub showTree : Path {
  my( $this, $c ) = @_;

  # populate the tree nodes with the areas for the image map
  $c->stash->{tree}->plot_core;

  # set up the TT view
  $c->stash->{template} = 'components/blocks/family/treeMap.tt';

  # cache the page (fragment) for one week
  $c->cache_page( 604800 );
}

#-------------------------------------------------------------------------------

=head2 image : Local

If we successfully generated a tree image, returns it directly as
an "image/gif". Otherwise returns a blank image.

=cut

sub image : Local {
  my( $this, $c ) = @_;

  if( defined $c->stash->{tree} ) {
    $c->res->content_type( 'image/gif' );
    $c->res->body( $c->stash->{tree}->plot_core( 1 )->gif );
  } else {
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->uri_for( '/static/images/blank.gif' ) );
  }
  
  # cache the image for one week
  $c->cache_page( 604800 ); 
}

#-------------------------------------------------------------------------------

=head2 end : Private

Override the end method from the parent class and use RenderView to take care
of displaying things.

=cut

sub end : ActionClass( 'RenderView' ) {}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 image : Local

If we successfully generated a tree image, returns it directly as
an "image/gif". Otherwise returns a blank image.

=cut

sub getTree : Private {
  my( $this, $c) = @_;

  my $acc  = $c->stash->{acc};
  my $type = $c->stash->{alnType};
  
  # build a cache key...
  my $cacheKey = "tree$acc$type";

  # see if it retrieves anything
  my $tree = $c->cache->get( $cacheKey ); 

  if( $tree ) {
    $c->log->debug( 'Family::Tree::getTree: successfully retrieved tree from cache' );
  } else {
    $c->log->debug( 'Family::Tree::getTree: no cached tree; generating from file' );

    # get a new tree object...
    $tree = treefam::nhx_plot->new( -width => 600,
                                    -skip  => 14 );
                                    
    # decide if we want the full or the seed alignment
    # TODO trees need to be in the database
    my $treeDataFile = $this->{treeFileDir} . "/$type/$acc.tree";
    $c->log->debug( "Family::Tree::getTree: loading tree file |$treeDataFile|" );

    # open the data file
    open TREE, $treeDataFile
      or $c->log->error( "Family::Tree::getTree: WARNING: couldn't open tree file for $acc: $!" ) and return;
  
    eval {
      $tree->parse( join '', <TREE> );
    };
    close TREE;  
    if( $@ ) {
      $c->log->error( "Family::Tree::getTree: ERROR: failed to parse tree for $acc: $@" );
      return 0;
    }

    # cache the tree that we just generated
    $c->cache->set( $cacheKey, $tree )
      if defined $tree;
  }

  $c->stash->{tree} = $tree;
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
