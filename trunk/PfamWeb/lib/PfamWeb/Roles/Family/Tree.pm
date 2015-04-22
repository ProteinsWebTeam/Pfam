# Tree.pm
# jt6 20120920 WTSI
#
# $Id$

=head1 NAME

PfamWeb::Roles::Family::Tree - role to add tree-specific methods to the family
controller

=cut

package PfamWeb::Roles::Family::Tree;

=head1 DESCRIPTION

This role adds tree-specific methods to the <LFamily> controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use treefam::nhx_plot;

#-------------------------------------------------------------------------------

=head1 TREE ACTIONS

=head2 tree : Chained

Stub to provide somewhere to hang the other methods.

=cut

sub tree : Chained( 'family' )
           PathPart( 'tree' )
           CaptureArgs( 0 ) {
  my ( $this, $c, $aln_type ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  $c->stash->{alnType} = 'seed';
}

#-------------------------------------------------------------------------------

=head2 tree_html : Chained

Plots the tree and hands off to a template that builds HTML for the image and
associated image map.

=cut

sub tree_html : Chained( 'tree' )
                PathPart( 'html' )
                Args( 0 ) {
  my ( $this, $c ) = @_;

  # stash the tree object
  $c->forward( 'get_tree' );
  
  # bail unless we actually got a tree
  unless ( defined $c->stash->{tree} ) {
    $c->res->status( 204 );
    return;
  }

  # populate the tree nodes with the areas for the image map
  $c->stash->{tree}->plot_core;

  # set up the TT view
  $c->stash->{template} = 'components/blocks/family/treeMap.tt';

  # cache the page (fragment) for one week
  $c->cache_page( 604800 );
}

#---------------------------------------

=head2 old_tree : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_tree : Path( '/family/tree' ) {
  my ( $this, $c, $action ) = @_;

  my $aln_type = 'seed';

  if ( defined $c->req->param('alnType') and
       exists $this->allowed_alignment_types->{ $c->req->param('alnType') } ) {
    $aln_type = $c->req->param('alnType');
  }

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  if ( ( $action || '' ) eq 'image' ) {
    $c->log->debug( 'Family::Tree::old_tree: redirecting to "image"' )
      if $c->debug;
    $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, "tree/$aln_type/image", $c->req->params ) );
  }
  elsif ( ( $action || '' ) eq 'download' ) {
    $c->log->debug( 'Family::Tree::old_tree: redirecting to "download"' )
      if $c->debug;
    $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, "tree/$aln_type/download", $c->req->params ) );
  }
	else {
    $c->log->debug( 'Family::Tree::old_tree: redirecting to "tree"' )
      if $c->debug;
    $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, "tree/$aln_type", $c->req->params ) );
  }
}

#-------------------------------------------------------------------------------

=head2 gone_tree_$type : Chained

These stubs just return an error message saying that we no longer have this
type of tree.

=cut

sub gone_tree_full : Chained( 'tree' )
                     PathPart( 'full' )
                     Args {
  my ( $this, $c ) = @_;

  $c->res->status( 410 ); # Gone
  $c->res->body( 'We no longer provide phylogenetic trees for full alignments.' );
}

sub gone_tree_meta : Chained( 'tree' )
                     PathPart( 'meta' )
                     Args {
  my ( $this, $c ) = @_;

  $c->res->status( 410 ); # Gone
  $c->res->body( 'We no longer provide phylogenetic trees for metagenomics alignments.' );
}

sub gone_tree_ncbi : Chained( 'tree' )
                     PathPart( 'ncbi' )
                     Args {
  my ( $this, $c ) = @_;

  $c->res->status( 410 ); # Gone
  $c->res->body( 'We no longer provide phylogenetic trees for NCBI alignments.' );
}

sub gone_tree_seed : Chained( 'tree' )
                     PathPart( 'seed' )
                     Args {
  my ( $this, $c, @args ) = @_;

  $c->log->debug( 'Family::Tree::gone_tree_seed: redirecting from seed alignment request' )
    if $c->debug;
  $c->res->redirect( $c->uri_for( '/family', $c->stash->{acc}, 'tree', @args ) );
}

#-------------------------------------------------------------------------------

=head2 image : Local

If we successfully generated a tree image, returns it directly as
an "image/gif". Otherwise returns a blank image.

=cut

sub image : Chained( 'tree' )
            PathPart( 'image' )
            Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  # stash the tree object
  $c->forward( 'get_tree' );

  if ( defined $c->stash->{tree} ) {
    $c->res->content_type( 'image/gif' );
    $c->res->body( $c->stash->{tree}->plot_core( 1 )->gif );
  }
    else {
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) ) if $c->debug;
  }

}

#-------------------------------------------------------------------------------

=head2 download : Chained

Serves the raw tree data as a downloadable file.

=cut

sub download : Chained( 'tree' )
               PathPart( 'download' )
               Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  $c->log->debug( 'Family::Tree::download: dumping tree data to the response' )
    if $c->debug;

  # stash the raw tree data
  $c->forward( 'get_tree_data' );

  return unless defined $c->stash->{treeData};

  my $filename = $c->stash->{acc} . '.nhx';

  $c->log->debug( "Family::Tree::download: tree filename: |$filename|" )
    if $c->debug;

  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $c->stash->{treeData} );
}

#-------------------------------------------------------------------------------
#- tree-related private actions ------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_tree : Private

Builds the TreeFam tree object for the specified family and alignment type 
(seed or full). We first check the cache for the pre-built tree object and 
then fall back to the database if it's not already available in the cache.

=cut

sub get_tree : Private {
  my ( $this, $c ) = @_;

  # see if we can extract the pre-built tree object from cache
  my $cacheKey = 'tree' . $c->stash->{acc} . $c->stash->{alnType};
  my $tree     = $c->cache->get( $cacheKey );
  
  if ( defined $tree ) {
    $c->log->debug( 'Family::Tree::get_tree: extracted tree from cache' )
      if $c->debug;  
  }
  else {
    $c->log->debug( 'Family::Tree::get_tree: failed to extract tree from cache; going to DB' )
      if $c->debug;  

    # get a new tree object...
    $tree = treefam::nhx_plot->new( -width => 600,
                                    -skip  => 14 );

    # retrieve the tree from the DB
    $c->forward( 'get_tree_data' );
    return unless defined $c->stash->{treeData};
  
    # parse the data
    eval {
      $tree->parse( $c->stash->{treeData} );
    };
    if( $@ ) {
      $c->log->error( 'Family::Tree::get_tree: ERROR: failed to parse ' 
                      . $c->stash->{alnType} . ' tree for ' 
                      . $c->stash->{acc} . ": $@" );
      return;
    }

    # and now cache the populated tree object
    $c->cache->set( $cacheKey, $tree ) unless $ENV{NO_CACHE};
  }
  
  $c->stash->{tree} = $tree;
}

#-------------------------------------------------------------------------------

=head2 get_tree_data : Private

Retrieves the raw tree data. We first check the cache and then fall back to the 
database.

=cut

sub get_tree_data : Private {
  my ( $this, $c ) = @_;

  # see if we can extract the pre-built tree object from cache
  my $cacheKey = 'treeData' . $c->stash->{acc} . $c->stash->{alnType};
  my $treeData = $c->cache->get( $cacheKey );
  
  if( defined $treeData ) {
    $c->log->debug( 'Family::Tree::get_tree_data: extracted tree data from cache' )
      if $c->debug;  
  } else {
    $c->log->debug( 'Family::Tree::get_tree_data: failed to extract tree data from cache; going to DB' )
      if $c->debug;  

    # retrieve the tree from the DB
    my $rs = $c->model('PfamDB::AlignmentAndTree')
               ->search( { pfama_acc => $c->stash->{pfam}->pfama_acc,
                           type       => $c->stash->{alnType} } );

    return unless defined $rs;
    
    my $row = $rs->first;
    return unless defined $row;
    
    my $tree = $row->tree;
    return unless defined $tree;

    # make sure we can uncompress it
    $treeData = Compress::Zlib::memGunzip( $tree );
    unless ( defined $treeData ) {
      $c->log->error( 'Family::Tree::get_tree_data: ERROR: failed to uncompress ' 
                      . $c->stash->{alnType} . ' tree data for ' 
                      . $c->stash->{acc} );
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

