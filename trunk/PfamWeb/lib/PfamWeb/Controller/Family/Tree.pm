
# Tree.pm
# jt6 20060511 WTSI
#
# Controller to build the tree view of the family
#
# $Id: Tree.pm,v 1.2 2006-08-14 10:43:45 jt6 Exp $

package PfamWeb::Controller::Family::Tree;

use strict;
use warnings;

use treefam::nhx_plot;

use Data::Dumper;

use base "PfamWeb::Controller::Family";

#-------------------------------------------------------------------------------
# we get here after the begin method from the parent class has handled
# the extraction of the family accession from the URL, so we can jump
# straight in and generate the tree

sub auto : Private {
  my( $this, $c ) = @_;

  # the accession should have been dropped into the stash by the begin
  # method
  my $acc = $c->stash->{pfam}->pfamA_acc;

  # find out what type of tree to draw, seed or full, being careful
  # not to take what the user supplies directly...
  my $type = (defined $c->req->param("type") and $c->req->param("type") eq "full")
	? "full" : "seed";

  # before generating it, see if we can retrieve it from the cache
  my $tree;
  unless( $tree = $c->cache->get( "tree" . $acc . $type ) ) {

	$c->log->debug( "Tree::auto: no cached tree; generating from file" );

	# get a new tree object...
	$tree = treefam::nhx_plot->new( -width => 600,
									-skip  => 14 );

	# decide if we want the full or the seed alignment
	my $treeDataFile = $this->{treeFileDir};
	if( $c->req->param( "type" ) eq "full" ) {
	  $treeDataFile .= "/full/$acc.tree";
	} else {
	  $treeDataFile .= "/seed/$acc.tree";
	}
	$c->log->debug( "Tree::generateTree: loading tree file \"$treeDataFile\"" );

	# open the data file
	open TREE, $treeDataFile
	  or $c->log->error( "Tree::generateTree: WARNING: couldn't open tree file for $acc: $!" ) and return;
	
	eval {
	  $tree->parse( join "", <TREE> );
	};
	close TREE;

	if( $@ ) {
	  $c->log->erro( "Tree::generateTree: ERROR: failed to parse tree for $acc: $@" );
	  return 0;
	}

	$c->cache->set( "tree". $acc . $type, $tree ) if defined $tree;
  }

  $c->stash->{tree} = $tree;

}

#-------------------------------------------------------------------------------

sub generateImage : Path( "/gettreeimage" ) {
  my( $this, $c ) = @_;

  if( defined $c->stash->{tree} ) {
	$c->res->content_type( "image/gif" );
	$c->res->write( $c->stash->{tree}->plot_core( 1 )->gif );
  } else {
	$c->res->redirect( "static/images/blank.gif" );
  }
}

#-------------------------------------------------------------------------------

sub generateMap : Path( "/gettreemap" ) {
  my( $this, $c ) = @_;

  # populate the tree nodes with the areas for the image map
  $c->stash->{tree}->plot_core;

  # set up the TT view
  $c->stash->{template} = "components/blocks/family/treeMap.tt";

  # and use it
  $c->forward( "PfamWeb::View::TTBlock" );

}

#-------------------------------------------------------------------------------

sub end : Private {

  # do nothing... just override the parent end

}

#-------------------------------------------------------------------------------

1;
