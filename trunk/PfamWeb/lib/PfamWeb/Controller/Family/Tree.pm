
# Tree.pm
# jt6 20060511 WTSI
#
# $Id: Tree.pm,v 1.9 2007-06-15 16:16:35 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Tree;

Controller to build the tree view of the family.

=cut

package PfamWeb::Controller::Family::Tree;

=head1 DESCRIPTION

Uses treefam drawing code to generate images of the tree for
a given family.

$Id: Tree.pm,v 1.9 2007-06-15 16:16:35 jt6 Exp $

=cut

use strict;
use warnings;

use treefam::nhx_plot;

use base "PfamWeb::Controller::Family";

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

  # the accession should have been dropped into the stash by the begin
  # method
  my $acc = $c->stash->{pfam}->pfamA_acc;

  # find out what type of tree to draw, seed or full, being careful
  # not to take what the user supplies directly...
  my $type = (defined $c->req->param('type') and $c->req->param('type') eq 'full')
               ? "full" : "seed";
  # get a new tree object...
  my $tree = treefam::nhx_plot->new( -width => 600,
                  									 -skip  => 14 );

	# decide if we want the full or the seed alignment
	my $treeDataFile = $this->{treeFileDir} . "/$type/$acc.tree";
	$c->log->debug( "Tree::generateTree: loading tree file \"$treeDataFile\"" );

  	# open the data file
	open TREE, $treeDataFile
	  or $c->log->error( "Tree::generateTree: WARNING: couldn't open tree file for $acc: $!" ) and return;

	eval {
	  $tree->parse( join '', <TREE> );
	};
	close TREE;  
	if( $@ ) {
	  $c->log->error( "Tree::generateTree: ERROR: failed to parse tree for $acc: $@" );
	  return 0;
	}

  # stash the stuff we have now
  $c->stash->{type} = $type;
  $c->stash->{tree} = $tree;

}

#sub auto : Private {
#  my( $this, $c ) = @_;
#
#  # the accession should have been dropped into the stash by the begin
#  # method
#  my $acc = $c->stash->{pfam}->pfamA_acc;
#
#  # find out what type of tree to draw, seed or full, being careful
#  # not to take what the user supplies directly...
#  my $type = (defined $c->req->param('type') and $c->req->param('type') eq 'full')
#               ? "full" : "seed";
#
#  # before generating it, see if we can retrieve it from the cache
#  my $cacheKey = "tree$acc$type";
#  my $tree = $c->cache->get( $cacheKey ); 
#
#  if( $tree ) {
#  	$c->log->debug( "Tree::auto: successfully retrieved tree from cache" );
#  } else {
#  	$c->log->debug( "Tree::auto: no cached tree; generating from file" );
#
#  	# get a new tree object...
#  	$tree = treefam::nhx_plot->new( -width => 600,
#                  									-skip  => 14 );
#  	# decide if we want the full or the seed alignment
#  	my $treeDataFile = $this->{treeFileDir} . "/$type/$acc.tree";
#  	$c->log->debug( "Tree::generateTree: loading tree file \"$treeDataFile\"" );
#
#  	# open the data file
#  	open TREE, $treeDataFile
#  	  or $c->log->error( "Tree::generateTree: WARNING: couldn't open tree file for $acc: $!" ) and return;
#	
#  	eval {
#  	  $tree->parse( join '', <TREE> );
#  	};
#  	close TREE;  
#  	if( $@ ) {
#  	  $c->log->error( "Tree::generateTree: ERROR: failed to parse tree for $acc: $@" );
#  	  return 0;
#  	}
#
#    # cache the tree that we just generated
#  	$c->cache->set( $cacheKey, $tree, '1209600' )
#    	if defined $tree;
#  }
#
#  # stash the stuff we have now
#  $c->stash->{type} = $type;
#  $c->stash->{tree} = $tree;
#
#}

#-------------------------------------------------------------------------------

=head2 generateImage : Path

If we successfully generated a tree image, returns it directly as
an "image/gif". Otherwise returns a blank image.

=cut

sub generateImage : Path( "/gettreeimage" ) {
  my( $this, $c ) = @_;

  if( defined $c->stash->{tree} ) {
  	$c->res->content_type( "image/gif" );
  	$c->res->write( $c->stash->{tree}->plot_core( 1 )->gif );
  } else {
  	$c->res->redirect( $c->uri_for( "/images/blank.gif" ) );
  }
}

#-------------------------------------------------------------------------------

=head2 generateMap : Path

Returns the image map that goes with a tree image.

=cut

sub generateMap : Path( "/gettreemap" ) {
  my( $this, $c ) = @_;

  # populate the tree nodes with the areas for the image map
  $c->stash->{tree}->plot_core;

  # set up the TT view
  $c->stash->{template} = "components/blocks/family/treeMap.tt";

  # and use it
  $c->forward( "PfamWeb::View::TT" );

}

#-------------------------------------------------------------------------------

=head2 end : Private

Just overrides the parent end, since we're returning the tree image directly.

=cut

sub end : Private {

  # do nothing...

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
