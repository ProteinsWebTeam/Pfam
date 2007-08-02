
# Tree.pm
# jt6 20060511 WTSI
#
# $Id: Tree.pm,v 1.12 2007-08-02 15:24:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Tree;

Controller to build the tree view of the family.

=cut

package PfamWeb::Controller::Family::Tree;

=head1 DESCRIPTION

Uses treefam drawing code to generate images of the tree for
a given family.

$Id: Tree.pm,v 1.12 2007-08-02 15:24:28 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;

use treefam::nhx_plot;

use base 'PfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

We get here after the begin method from the parent class has handled
the extraction of the family accession from the URL, so we can jump
straight in and retrieve the tree data for the required family.

Breaks out of the processing chain if the tree can't be generated for
whatever reason.

=cut

sub auto : Private {
  my( $this, $c ) = @_;

  # stash of the tree data
  $c->forward( 'getTree' );

  return defined $c->stash->{tree};
}

#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
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

}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 getTree : Private

Retrieves and stashed the raw tree data. Tries first to extract the data
from cache and if that fails, retrieves it from the database.

=cut

sub getTree : Private {
  my( $this, $c) = @_;

  # see if we can extract the pre-built tree object from cache
  my $cacheKey = 'tree' . $c->stash->{acc} . $c->stash->{alnType};
  my $tree     = $c->cache->get( $cacheKey );
  
  if( defined $tree ) {
    $c->log->debug( 'Family::Tree::getTree: extracted tree from cache' );  
  } else {
    $c->log->debug( 'Family::Tree::getTree: failed to extract tree from cache; going to DB' );  

    # get a new tree object...
    $tree = treefam::nhx_plot->new( -width => 600,
                                    -skip  => 14 );

    # retrieve the tree from the DB
    my $rs = $c->model('PfamDB::AlignmentsAndTrees')
               ->search( { auto_pfamA => $c->stash->{pfam}->auto_pfamA,
                           type       => $c->stash->{alnType} } );
    my $row = $rs->first;

    unless( defined $row->tree ) {
      $c->stash->{errorMsg} = 'We could not retrieve the tree data for '
                              . $c->stash->{acc};
      return;
    }

    # make sure we can uncompress it
    my $treeData = Compress::Zlib::memGunzip( $row->tree );
    unless( defined $treeData ) {
      $c->stash->{errorMsg} = 'We could not extract the tree data for '
                              . $c->stash->{acc};
      return;
    }
  
    # parse the data
    eval {
      $tree->parse( $treeData );
    };
    if( $@ ) {
      $c->log->error( "Family::Tree::getTree: ERROR: failed to parse tree: $@" );
      $c->stash->{errorMsg} = 'There was a problem with the tree data for '
                              . $c->stash->{acc};
      return;
    }

    # and now cache the populated tree object
    $c->cache->set( $cacheKey, $tree );
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
