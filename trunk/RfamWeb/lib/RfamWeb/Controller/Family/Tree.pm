
# Tree.pm
# jt6 20060511 WTSI
#
# $Id: Tree.pm,v 1.3 2009-01-06 11:52:51 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Family::Tree;

Controller to build the tree view of the family.

=cut

package RfamWeb::Controller::Family::Tree;

=head1 DESCRIPTION

Uses treefam drawing code to generate images of the tree for
a given family.

$Id: Tree.pm,v 1.3 2009-01-06 11:52:51 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;
use Data::Dump qw( dump );

use treefam::nhx_plot;

use base 'RfamWeb::Controller::Family';

#-------------------------------------------------------------------------------
#- exposed actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 METHODS

=head2 tree : Path

Plots the tree that we loaded in "auto" and hands off to a template that
builds HTML for the image and associated image map.

=cut

sub tree : Path {
  my ( $this, $c ) = @_;
  
  # stash the tree object
  $c->forward( 'get_tree' );

  # populate the tree nodes with the areas for the image map
  $c->stash->{tree}->plot_core 
    if defined $c->stash->{tree};

  # set up the TT view
  $c->stash->{template} = 'components/blocks/family/treeMap.tt';

  # cache the page (fragment) for one week
  #$c->cache_page( 604800 );

  $c->log->debug( 'Family::Tree::tree: rendering treeMap.tt' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 image : Local

If we successfully generated a tree image, returns it directly as
an "image/gif". Otherwise returns a blank image.

=cut

sub image : Local {
  my ( $this, $c ) = @_;

  # stash the tree object
  $c->forward( 'get_tree' );

  if ( defined $c->stash->{tree} ) {
    $c->res->content_type( 'image/gif' );
    $c->res->body( $c->stash->{tree}->plot_core( 1 )->gif );
  }
  else {
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
  }

  $c->log->debug( 'Family::Tree::image: returning raw tree image' )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 download : Local

Serves the raw tree data as a downloadable file.

=cut

sub download : Local {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::Tree::download: dumping tree data to the response' )
    if $c->debug;

  # stash the raw tree data
  $c->forward( 'get_tree_data' );

  return unless defined $c->stash->{treeData};

  my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType} . '.nhx';
  $c->log->debug( 'Family::Tree::download: tree data: |' . $c->stash->{treeData} . '|' )
    if $c->debug;

  $c->log->debug( "Family::Tree::download: tree filename: |$filename|" )
    if $c->debug;

  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $c->stash->{treeData} );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 action : Attribute

Description...

=cut

sub auto : Private {
  my ( $this, $c ) = @_;
  
  $c->stash->{label} = 'acc';
  if ( defined $c->req->param('label') and
       $c->req->param('label') eq 'species' ) {
    $c->log->debug( 'Tree::image: labelling tree with species' ) if $c->debug;
    $c->stash->{label} = 'species';
  }
  
  1;

}

#-------------------------------------------------------------------------------

=head2 get_tree : Private

Builds the TreeFam tree object for the specified family and alignment type 
(seed or full). We first check the cache for the pre-built tree object and 
then fall back to the database if it's not already available in the cache.

=cut

sub get_tree : Private {
  my ( $this, $c) = @_;

  # retrieve the tree from the DB
  $c->forward( 'get_tree_data' );

  unless ( defined $c->stash->{treeData} ) {
    $c->stash->{errorMsg} = 'We could not extract the ' . $c->stash->{alnType}
                            . 'tree for ' . $c->stash->{acc};
    return;
  }
  
  # get the tree with accessions as labels and ask if for the maximum length of
  # the labels
  $c->log->debug( 'Family::Tree::get_tree: building tree labelled with accessions' )
    if $c->debug;
  my $acc_labelled_tree = $c->forward( 'build_labelled_tree', [ 0 ] );
  my $acc_max_width = $acc_labelled_tree->calculate_max_label_width();

    # get the tree again, this time with species as labels
  $c->log->debug( 'Family::Tree::get_tree: building tree labelled with species names' )
    if $c->debug;
  my $species_labelled_tree = $c->forward( 'build_labelled_tree', [ 1 ] );
  my $species_max_width = $species_labelled_tree->calculate_max_label_width();
    
  # work out which set of labels are longer, accession or species
  my $max_width;
  if ( $acc_max_width > $species_max_width ) {
    $c->log->debug( 'Family::Tree::get_tree: using width calculated from accessions' )
      if $c->debug;
    $max_width = $acc_max_width;
  }
  else {
    $c->log->debug( 'Family::Tree::get_tree: using width calculated from species names' )
      if $c->debug;
    $max_width = $species_max_width;
  }

  # make sure we catch parse failures from the forwards
  return if $c->stash->{errorMsg};

  # set the maximum label width explicitly on both trees, so that they'll 
  # generate images with the same branch lengths
  $acc_labelled_tree->max_label_width( $max_width );
  $species_labelled_tree->max_label_width( $max_width );

  # now, we really only need one of these for this hit, so stash just the 
  # requested tree object
  if ( $c->stash->{label} eq 'acc' ) {
    $c->log->debug( 'Family::Tree::get_tree: returning tree labelled with accessions' )
      if $c->debug;
    $c->stash->{tree} = $acc_labelled_tree;
  }
  else {
    $c->log->debug( 'Family::Tree::get_tree: returning tree labelled with species names' )
      if $c->debug;
    $c->stash->{tree} = $species_labelled_tree;
  }
}

#-------------------------------------------------------------------------------

=head2 build_labelled_tree : Private

Builds a tree object with leaf nodes labelled either with species names or
sequence accessions, depending on the value of the first argument (0 or 1
respectively).

=cut

sub build_labelled_tree : Private {
  my ( $this, $c, $use_species_labels ) = @_;
  
  my $cache_key = 'tree_' 
                  . $c->stash->{acc} 
                  . $c->stash->{alnType}
                  . $use_species_labels;
  my $tree = $c->cache->get( $cache_key );
  
  if ( defined $tree ) {
    $c->log->debug( 'Family::Tree::build_labelled_tree: extracted tree object from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Tree::build_labelled_tree: failed to extract tree object from cache; going to DB' )
      if $c->debug;
  
    # get a new tree object...
    $tree = treefam::nhx_plot->new( -width => 600,
                                    -skip  => 14 );
  
    # parse the data
    eval {
      $tree->parse( $c->stash->{treeData} );
    };
    if ( $@ ) {
      $c->log->error( "Family::Tree::build_labelled_tree: ERROR: failed to parse tree: $@" );
      $c->stash->{errorMsg} = 'There was a problem with the tree data for '
                              . $c->stash->{acc};
      return;
    }
  
    # re-label the tree
    foreach my $node ( $tree->node_array ) {
  
      # ignore everything but leaf nodes
      next if $node->{C};
  
      # store the original label in a new slot, "L"
      $node->{L} = $node->{N};
  
      # rebuild the label, converting underscores to spaces as we pass
      if ( $node->{N} =~ m/^(\d+\.\d+)_(\w+\.?\d*)\/(\d+)\-(\d+)\_(.*)$/ ) {
        $node->{N} = $use_species_labels ? $5 : "$2/$3-$4";
        $node->{N} =~ s/_/ /g;
      }
      else {
        $c->log->debug( 'Family::Tree::build_labelled_tree: couldn\'t match node name: |' . $node->{N} . '|')
          if $c->debug;
      }
    }

    # cache the tree
    $c->cache->set( $cache_key, $tree ) unless $ENV{NO_CACHE};
  }

  return $tree;
}

#-------------------------------------------------------------------------------

=head2 getTreeData : Private

Retrieves the raw tree data. We first check the cache and then fall back to the 
database.

=cut

sub get_tree_data : Private {
  my ( $this, $c) = @_;

  # see if we can extract the pre-built tree object from cache
  my $cacheKey = 'treeData' 
                 . $c->stash->{acc}
                 . $c->stash->{alnType};
  my $treeData = $c->cache->get( $cacheKey );
  
  if ( defined $treeData ) {
    $c->log->debug( 'Family::Tree::get_tree_data: extracted tree data from cache' )
      if $c->debug;  
  }
  else {
    $c->log->debug( 'Family::Tree::get_tree_data: failed to extract tree data from cache; going to DB' )
      if $c->debug;  

    # retrieve the tree from the DB
    my $rs = $c->model('RfamDB::AlignmentsAndTrees')
               ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                           type      => $c->stash->{alnType} } );
    my $row = $rs->first;

    if ( defined $row and 
         defined $row->tree ) {
      $c->log->debug( 'Family::Tree::get_tree_data: retrieved tree data from DB' )
        if $c->debug;
    }
    else {
      $c->log->debug( 'Family::Tree::get_tree_data: no rows from DB query' )
        if $c->debug;
      $c->stash->{errorMsg} = 'We could not retrieve the tree data for '
                              . $c->stash->{acc};
      return;
    }

    # make sure we can uncompress it
    $treeData = Compress::Zlib::memGunzip( $row->tree );
    if ( defined $treeData ) {
      $c->log->debug( 'Family::Tree::get_tree_data: successfully gunzipped tree data' )
        if $c->debug;
    }
    else {
      $c->log->debug( 'Family::Tree::get_tree_data: tree data not gzipped...' )
        if $c->debug;
      $treeData = $row->tree;
    }

    unless ( defined $treeData ) {
      $c->log->debug( 'Family::Tree::get_tree_data: failed to retrieve tree data' )
        if $c->debug;
      $c->stash->{errorMsg} = 'We could not extract the tree data for '
                              . $c->stash->{acc};
      return;
    }

    # and now cache the populated tree data
    $c->cache->set( $cacheKey, $treeData ) unless $ENV{NO_CACHE};
  }

  # stash the uncompressed tree
  $c->stash->{treeData} = $treeData;
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
