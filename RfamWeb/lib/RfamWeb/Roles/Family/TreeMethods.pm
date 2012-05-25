
# TreeMethods.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Family::TreeMethods - role to add tree-related methods to the 
family page controller

=cut

package RfamWeb::Roles::Family::TreeMethods;

=head1 DESCRIPTION

This is a role to add species tree-related methods to the Family controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

use treefam::nhx_plot;

#-------------------------------------------------------------------------------

=head2 tree : Chained('family') PathPart('tree') CaptureArgs(1)

Mid-point in a chain for handling trees for a family. Requires "seed" or "full"
as the first argument.

=cut

sub tree : Chained( 'family' )
           PathPart( 'tree' )
           CaptureArgs( 1 ) {
  my ( $this, $c, $aln_type ) = @_;
  
  $c->stash->{alnType} = ( $aln_type || '' ) eq 'seed' ? 'seed' : 'full'; 

  $c->log->debug( 'Family::tree: looking for tree type ' . $c->stash->{alnType} )
    if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 tree_data : Chained('tree') PathPart('') Args(0)

Returns the raw tree data.

=cut

sub tree_data : Chained( 'tree' )
                PathPart( '' )
                Args( 0 ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family::tree_data: returning ' . $c->stash->{alnType} . ' tree' )
    if $c->debug;

  # stash the raw tree data
  $c->forward( 'get_tree_data' );

  # fail fast if there was a problem retrieving family data
  unless ( $c->stash->{treeData} ) {
    $c->res->status(500); # Internal server error

    $c->stash->{rest}->{error} ||= 'Could not retrieve tree data.';
    $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/html' )
                          ? 'components/blocks/family/error.tt'
                          : 'rest/family/error_xml.tt';

    return;
  }

  $c->log->debug( 'Family::tree_data: tree data: |' . $c->stash->{treeData} . '|' )
    if $c->debug;

  my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType} . '.nhx';
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $c->stash->{treeData} );
}

#---------------------------------------

=head2 old_tree_dl : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_tree_dl : Path( '/family/tree/download' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_tree_dl: redirecting to "tree_data"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed'    ? 'seed'    : 'full';

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 
                                  'tree',  $aln_type ) );
}

#-------------------------------------------------------------------------------

=head2 tree_labels : Chained('tree') PathPart('label') CaptureArgs(1)

Mid-point in a chain for handling trees for a family. Requires one argument,
"species" or "acc", setting the label style to be applied.

=cut

sub tree_labels : Chained( 'tree' )
                  PathPart( 'label' )
                  CaptureArgs( 1 ) {
  my ( $this, $c, $label ) = @_;
  
  $c->stash->{label} = ( $label || '' ) eq 'species' ? 'species' : 'acc';

  $c->log->debug( 'Family::tree_labels: labelling ' . $c->stash->{alnType} 
                  . ' tree with ' . $c->stash->{label} . ' labels' )
    if $c->debug;

  # stash the tree object
  $c->forward( 'get_tree' );
}

#-------------------------------------------------------------------------------

=head2 tree_map : Path

Returns an HTML snippet with a link to the tree image and associated image map
for the specified seed/full alignment with the given label type (acc/species).
Also builds the image and caches it, so that the request for the image won't
have to wait.

If the client is a web-browser, the image map is returned. Otherwise, an error
message will be shown, to the effect that it only makes sense to view this as
HTML.

=cut

sub tree_map : Chained( 'tree_labels' )
               PathPart( 'map' )
               Args( 0 ) 
               ActionClass( 'REST' ) {}

# handles responses for all clients, browsers and otherwise
sub tree_map_GET : Private {
  my ( $this, $c ) = @_;

  # default to the error template
  $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' )
                        ? 'rest/family/error_xml.tt'
                        : 'components/blocks/family/error.tt';

  # fail fast if there was a problem retrieving the tree
  unless ( $c->stash->{tree} ) {
    $c->res->status( 500 ); # Internal server error
    $c->stash->{rest}->{error} ||= 'Could not retrieve tree data.';
    return;
  }

  # it only makes sense to return an image map as HTML
  # unless ( grep m|text/html|, @{ $c->req->accepted_content_types || [] } ) {
  # unless ( $c->req->accepts( 'text/html' ) or
  #          $c->req->accepts( '*/*' ) ) {
  #   $this->status_bad_request( $c, message => 'Image map only available as HTML.' );
  #   return;
  # }

  # cache the page (fragment) for one week
  $c->cache_page( 604800 );

  # populate the tree nodes with the areas for the image map
  $c->stash->{tree}->plot_core 
    if defined $c->stash->{tree};

  # cache the template output for one week
  $c->cache_page( 604800 );

  $c->stash->{template} = 'components/blocks/family/treeMap.tt';
}

#---------------------------------------

=head2 old_tree_map : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_tree_map : Path( '/family/tree' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_tree_map: redirecting to "tree_map"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed'    ? 'seed'    : 'full';
  my $label    = ( $c->req->param('label')   || '' ) eq 'species' ? 'species' : 'acc';

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{label};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 
                                  'tree',  $aln_type, 
                                  'label', $label, 
                                  'map',   $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 tree_image : Chained('tree_labels') PathPart('image') Args(0)

If we successfully generated a tree image, returns it directly as an
"image/gif". If an image could not be generated, either a blank, single-pixel
gif is handed back (if the client looks like a browser), or an error message is
returned.

=cut

sub tree_image : Chained( 'tree_labels' )
                 PathPart( 'image' )
                 Args( 0 ) 
                 ActionClass( 'REST::ForBrowsers' ) {}

# handle response for browsers only
sub tree_image_GET_html : Private {
  my ( $this, $c ) = @_;

  unless ( $c->stash->{tree} ) {
    $c->log->debug( 'Family::tree_image: returning "blank.gif"' )
      if $c->debug;
    # TODO this is bad. We should avoid hard-coding a path to an image here
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
    return;
  }

  $c->log->debug( 'Family::tree_image: returning raw tree image' )
    if $c->debug;

  $c->res->content_type( 'image/gif' );
  $c->res->body( $c->stash->{tree}->plot_core( 1 )->gif );
}

#---------------------------------------

# generate other output formats besides HTML
sub tree_image_GET : Private {
  my ( $this, $c ) = @_;

  unless ( defined $c->stash->{tree} ) {
    $c->res->status( 500 ); # Internal server error

    $c->stash->{rest}->{error} ||= 'There was a problem when plotting the tree.';
    $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' )
                          ? 'rest/family/error_xml.tt'
                          : 'components/blocks/family/error.tt';
    
    return;
  }

  # cache the template output for one week
  $c->cache_page( 604800 );

  $c->res->content_type( 'image/gif' );
  $c->res->body( $c->stash->{tree}->plot_core( 1 )->gif );
}

#---------------------------------------

=head2 old_tree_image : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_tree_image : Path( '/family/tree/image' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_tree_image: redirecting to "tree_image"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed'    ? 'seed'    : 'full';
  my $label    = ( $c->req->param('label')   || '' ) eq 'species' ? 'species' : 'acc';

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{label};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry},
                                  'tree',  $aln_type, 
                                  'label', $label, 
                                  'image', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- tree actions (private) ------------------------------------------------------
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

  if ( ! defined $c->stash->{treeData} or $c->stash->{rest}->{error} ) {
    $c->log->debug( 'Family::get_tree: no tree data or hit a previous error' )
      if $c->debug;

    $c->stash->{rest}->{error} ||= 'We could not extract the ' . $c->stash->{alnType} 
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
    $c->log->debug( 'Family::build_labelled_tree: extracted tree object from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::build_labelled_tree: failed to extract tree object from cache; going to DB' )
      if $c->debug;
  
    # get a new tree object...
    $tree = treefam::nhx_plot->new( -width => 600,
                                    -skip  => 14 );
  
    # parse the data
    eval {
      $tree->parse( $c->stash->{treeData} );
    };
    if ( $@ ) {
      $c->log->debug( "Family::build_labelled_tree: ERROR: failed to parse tree: $@" )
        if $c->debug;

      $c->stash->{rest}->{error} ||= 'There was a problem with the tree data for '
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

=head2 get_tree_data : Private

Retrieves the raw tree data. We first check the cache and then fall back to the 
database.

=cut

sub get_tree_data : Private {
  my ( $this, $c) = @_;

  # see if we can extract the pre-built tree object from cache
  my $cacheKey = 'treeData' 
                 . $c->stash->{acc}
                 . $c->stash->{alnType};
  my $tree_data = $c->cache->get( $cacheKey );
  
  if ( defined $tree_data ) {
    $c->log->debug( 'Family::get_tree_data: extracted tree data from cache' )
      if $c->debug;  
  }
  else {
    $c->log->debug( 'Family::get_tree_data: failed to extract tree data from cache; going to DB' )
      if $c->debug;  

    # retrieve the tree from the DB
    my $rs = $c->stash->{db}->resultset('AlignmentsAndTrees')
               ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                           type      => $c->stash->{alnType} },
                         { columns => [ 'tree' ] } );
    my $row = $rs->first;

    if ( defined $row and 
         defined $row->tree ) {
      $c->log->debug( 'Family::get_tree_data: retrieved tree data from DB' )
        if $c->debug;
    }
    else {
      $c->log->debug( 'Family::get_tree_data: no rows from DB query' )
        if $c->debug;

      $c->stash->{rest}->{error} ||= 'We could not retrieve the tree data for '
                                     . $c->stash->{acc};

      return;
    }

    # make sure we can uncompress it
    $tree_data = Compress::Zlib::memGunzip( $row->tree );
    if ( defined $tree_data ) {
      $c->log->debug( 'Family::get_tree_data: successfully gunzipped tree data' )
        if $c->debug;
    }
    else {
      $c->log->debug( 'Family::get_tree_data: tree data not gzipped...' )
        if $c->debug;
      $tree_data = $row->tree;
    }

    unless ( defined $tree_data ) {
      $c->log->debug( 'Family::get_tree_data: failed to retrieve tree data' )
        if $c->debug;

      $c->stash->{rest}->{error} ||= 'We could not extract the tree data for '
                                     . $c->stash->{acc};

      return;
    }

    # and now cache the populated tree data
    $c->cache->set( $cacheKey, $tree_data ) unless $ENV{NO_CACHE};
  }

  # stash the uncompressed tree
  $c->stash->{treeData} = $tree_data;
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


