
# Family.pm
# jt6 20080306 WTSI
#
# $Id: Family.pm,v 1.6 2009-01-06 11:52:06 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Family - controller to build the main Rfam family page

=cut

package RfamWeb::Controller::Family;

=head1 DESCRIPTION

This is intended to be the base class for everything related to Rfam
families across the site. The L<begin|/"begin : Private"> method tries
to extract a Rfam ID or accession from the captured URL and tries to
load a Rfam object from the model.

Generates a B<tabbed page>.

$Id: Family.pm,v 1.6 2009-01-06 11:52:06 jt6 Exp $

=cut

use strict;
use warnings;

use Compress::Zlib;
use MIME::Base64;
use JSON;
use File::Temp qw( tempfile );
use Data::Dump qw( dump );
use Moose;
use namespace::autoclean;
use treefam::nhx_plot;

BEGIN {
  extends 'RfamWeb::Controller::Section';
  extends 'Catalyst::Controller::REST';
}

# set up the list of content-types that we handle
__PACKAGE__->config(
  'default' => 'text/html',
  'map'     => {
    'text/html' => [ 'View', 'TT' ],
    'text/xml'  => [ 'View', 'TT' ],
    'application/json' => 'JSON',
  }
);

# set the name of the section
__PACKAGE__->config( SECTION => 'family' );

# this is really ugly, but it makes sense to have this image easily to hand.
# Ideally we'd use a __DATA__ stream, but that breaks in mod_perl and we
# probably shouldn't try it in a FastCGI environment either. 
# See: http://modperlbook.org/html/6-6-1-_-_END_-_-and-_-_DATA_-_-Tokens.html
our $no_alignment_image = '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';
    
#-------------------------------------------------------------------------------

sub testaction : Chained( 'family' )
                 PathPart( 'test' )
                 Args( 0 ) 
                 ActionClass( 'REST::ForBrowsers' ) {}

sub testaction_GET {
  my ( $this, $c ) = @_;

  if ( $c->req->param('status') eq 'bad' ) {
    $c->res->status( 500 ); # Internal server error
    $c->stash->{rest} = { error => 'There was a problem.' };
  }
  else {
    $this->status_ok( 
      $c,
      entity => {
        foo => 'bar'
      }
    );
  }
}

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 begin : Private

This is the guts of this controller. Its function is to extract
values from the parameters. Accepts "acc", "id" and "entry", in
lieu of having them as path components.

=cut

sub begin : Private {
  my ( $this, $c ) = @_;
  
  # get a handle on the entry, if supplied as params, and detaint it
  my $tainted_entry = $c->req->param('acc')   ||
                      $c->req->param('id')    ||
                      $c->req->param('entry') ||
                      '';
  
  if ( $tainted_entry ) {
    $c->log->debug( 'Family::begin: got a tainted entry' )
      if $c->debug;
    ( $c->stash->{param_entry} ) = $tainted_entry =~ m/^([\w-]+)$/;
  }
}

#-------------------------------------------------------------------------------

=head2 family : Chained('/') PathPart('family') CaptureArgs(1)

Mid-point of a chain handling family-related data. Retrieves family information
from the DB.

=cut

sub family : Chained( '/' )
             PathPart( 'family' )
             CaptureArgs( 1 ) {

  my ( $this, $c, $entry_arg ) = @_;

  my $tainted_entry = $c->stash->{param_entry} ||
                      $entry_arg               ||
                      '';
  
  $c->log->debug( "Family::family: tainted_entry: |$tainted_entry|" )
    if $c->debug;

  my $entry;
  if ( $tainted_entry ) {
    ( $entry ) = $tainted_entry =~ m/^([\w-]+)$/;
    $c->stash->{errorMsg} = 'Invalid Rfam family accession or ID' 
      unless defined $entry;
  }
  else {
    $c->stash->{errorMsg} = 'No Rfam family accession or ID specified';
  }

  # retrieve data for the family
  $c->forward( 'get_data', [ $entry ] ) if defined $entry;
}

# TODO need to handle this URL, "/family", with no entry given, and have it
# provide a list of all families.

#-------------------------------------------------------------------------------

=head2 family_page : Chained('family') PathPart('') Args(0) ActionClass('REST')

End-point of a chain handling family data. Generates a family page (HTML) 
or an XML output. Implemented using C::C::REST; accepts GET requests only.

=cut

sub family_page : Chained( 'family' )
                  PathPart( '' )
                  Args( 0 )
                  ActionClass( 'REST::ForBrowsers' ) { }

sub family_page_GET : Private {
  my ( $this, $c ) = @_;

  # handle XML

}

sub family_page_GET_html : Private {
  my ( $this, $c ) = @_;

  unless ( $c->stash->{rfam} ) {
    $c->stash->{template} = 'components/blocks/family/error.tt';
    return;
  }

  #---------------------------------------

  # load the data for all regions, provided the number of regions is less than
  # the limit set in the config
  if ( $c->stash->{rfam}->num_full <= $this->{regionsLimits}->{showAll} ) {
    $c->log->debug( 'Family::family_page: num_full <= showAll limit; retrieving regions' )
      if $c->debug;
    $c->stash->{showAll} = 1;
    $c->forward( 'get_regions_data' );
  }
  elsif ( $c->stash->{rfam}->num_full <= $this->{regionsLimits}->{showText} ) {
    $c->log->debug( 'Family::family_page: num_full <= showText limit; retrieving regions later' )
      if $c->debug;
    $c->stash->{showText} = 1;
  }

  # add the clan details, if any
  my $clan = $c->model('RfamDB::Clans')
               ->search( { 'clan_memberships.auto_rfam' => $c->stash->{rfam}->auto_rfam },
                         { prefetch => [ qw(clan_memberships) ] } )
               ->first;
  
  if ( $clan and defined $clan->clan_acc ) {
    $c->log->debug( 'Family::family_page: adding clan info' ) if $c->debug;
    $c->stash->{clan} = $clan;
  }

  $c->cache_page( 43200 ); # cache for 12 hours
  
  #---------------------------------------

  # are we emitting XML or HTML ? We need to retrieve less data for XML...

  # (should be able to use $request->preferred_content_types but it throws
  # an exception in the tests for the controller, so we'll use this 
  # work-around instead.)
  if ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' ) {
    $c->log->debug( 'Family::family_page: emitting XML' ) 
      if $c->debug;

    $c->stash->{template} = 'rest/family/rfam.tt';

    # if there was an error...
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Family::family_page: there was an error: |' .
                      $c->stash->{errorMsg} . '|' ) if $c->debug;
      $c->stash->{template} = 'rest/family/error_xml.tt';
      return;
    }
  }
  else {
    $c->log->debug( 'Family::family_page: emitting HTML' )
      if $c->debug;

    $c->log->debug( 'Family::family_page: adding summary info' ) 
      if $c->debug;
    $c->forward( 'get_summary_data' );

    $c->log->debug( 'Family::family_page: adding wikipedia info' ) 
      if $c->debug;
    $c->forward( 'get_wikipedia' );
  }

  $c->stash->{pageType} ||= $this->{SECTION};
  $c->forward( 'Section', 'end' );
  $c->log->debug( 'section: ' . $this->{SECTION} )
    if $c->debug;
  # $c->stash->{pageType} ||= $this->{SECTION};
  # $c->stash->{template} = 'pages/layout.tt';
}

#---------------------------------------

=head2 old_family : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_family : Path( '/family' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_family: redirecting to "family"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- family page components ------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 id : Chained

Returns the ID for this family as a single, plain text string. Returns 404 if
there's no family to work on.

=cut

sub id : Chained( 'family' )
         PathPart( 'id' )
         Args( 0 ) {
  my ( $this, $c ) = @_;
  
  # cache page for 1 week
  $c->cache_page( 604800 ); 
  
  if ( defined $c->stash->{rfam} ) {    
    $c->res->content_type( 'text/plain' );
    $c->res->body( $c->stash->{rfam}->rfam_id );
  }
  else { 
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#-------------------------------------------------------------------------------

=head2 acc : Chained

Returns the accession for this family as a single, plain text string. Returns 
404 if there's no family to work on.

=cut

sub acc : Chained( 'family' )
          PathPart( 'acc' )
          Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache page for 1 week
  $c->cache_page( 604800 );

  if ( defined $c->stash->{rfam} ) {
    $c->res->content_type( 'text/plain' );
    $c->res->body( $c->stash->{rfam}->rfam_acc );
  }
  else {
    $c->res->status( 404 );
    $c->res->body( 'No such family' );
  }
}

#-------------------------------------------------------------------------------

=head2 varna : Local

This is the way into the VARNA secondary structure viewer applet.

To fix a possible problem with the reference structure annotation in VARNA,
we apply a pattern match to the structure description string, converting 
"A" and "a" to "[" and "]", and "Bb" to "{}".

Hands straight off to a template that generates a "tool" page containing the 
VARNA applet.

=cut

sub varna : Chained( 'family' )
            PathPart( 'varna' )
            Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'components/tools/varna.tt';

  my $json = JSON->new;

  # retrieve the JSON string with the (broken) annotation
  # my $json_string = $c->stash->{rfam}->structure_annotations;

  # get the gzip compressed JSON string for the structure annotation
  my $rs = $c->model('RfamDB::SecondaryStructureImages')
             ->find( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                       type      => 'ss' } );
  return unless ( $rs and $rs->image );

  # try to uncompress it
  my $json_string = Compress::Zlib::memGunzip( $rs->image );
  return unless $json_string;

  # decode it so we can work with it as a regular perl data structure,
  # convert the A/a notation to [/] and similarly for B/b to {/}, then
  # re-encode it and stash it for the template
  my $ss = $json->decode( $json_string );
  $ss->{reference_structure} =~ tr/AaBbC-Zc-z/[]{}../;
  $c->stash->{ss} = $json->encode( $ss );
}

#---------------------------------------

=head2 old_varna : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_varna : Path( '/family/varna' ) {
  my ( $this, $c, $entry_arg ) = @_;

  $c->log->debug( 'Family::old_varna: redirecting to "varna"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $entry_arg, 'varna', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 image : Local

Retrieves and returns an image from the database. Caches the image, unless
C<$ENV{NO_CACHE}> is true. 

=cut

sub image : Chained( 'family' )
            PathPart( 'image' )
            Args( 1 ) {
  my ( $this, $c, $type ) = @_;
  
  $c->cache_page( 604800 );

  my ( $image_type ) = $type || '' =~ m/^(\w+)$/;
  $c->log->debug( "Family::image: image_type: |$image_type|" )
    if $c->debug;

  unless ( defined $image_type and $image_type ) {
    $c->log->debug( 'Family::image: no valid type specified; defaulting to normal' )
      if $c->debug;
    $image_type = 'normal';
  }

  my $cache_key = 'family_image' . $c->stash->{acc} . $image_type;
  my $image     = $c->cache->get( $cache_key );
  
  if ( defined $image ) {
    $c->log->debug( 'Family::image: retrieved image from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::image: failed to retrieve image from cache; going to DB' )
      if $c->debug;

    my $rs = $c->model('RfamDB::SecondaryStructureImages')
               ->find( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                         type      => $image_type } );

    unless ( defined $rs and
             defined $rs->image ) {
      $c->detach( 'no_alignment' );
      return;
    }

    $image = $rs->image;

    $c->cache->set( $cache_key, $image ) unless $ENV{NO_CACHE}
  }
  
  $c->res->content_type( 'image/png' );
  $c->res->body( $image );
}

#---------------------------------------

=head2 old_image : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_image : Path( '/family/image' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_image: redirecting to "image"' )
    if $c->debug;

  my ( $image_type ) = $c->req->param('type') || 'normal' =~ m/^(\w+)$/;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{type};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'image', $image_type, $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 no_alignment : Private

Drops a PNG image into the stash, with the message "we do not have this
alignment in the database".

=cut

sub no_alignment : Private {
  my ( $this, $c ) = @_;

  $c->cache_page( 604800 );

  my $cache_key = 'no_alignment_image';
  my $image     = $c->cache->get( $cache_key );
  
  if ( defined $image ) {
    $c->log->debug( 'Family::no_alignment: retrieved "no alignment" image from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::no_alignment: failed to retrieve "no alignment" image from cache; building afresh' )
      if $c->debug;

    $image = decode_base64( $no_alignment_image );

    unless ( defined $image ) {
      $c->stash->{errorMsg} = 'We could not find an image for ' 
                              . $c->stash->{acc};
      return;
    }

    $c->cache->set( $cache_key, $image ) unless $ENV{NO_CACHE}
  }
  
  $c->res->content_type( 'image/png' );
  $c->res->body( $image );
}

#-------------------------------------------------------------------------------

=head2 cm : Local

Serves the CM file for this family.

=cut

sub cm : Chained( 'family' )
         PathPart( 'cm' )
         Args() {
  my ( $this, $c, $version ) = @_;
  
  my $rs;
  if ( defined $version and
       $version =~ m/^\d+\.\d+$/ ) {
    $c->log->debug( "Family::cm: looking for CM built with infernal v. |$version| ")
      if $c->debug;
    $rs = $c->stash->{rfam}->search_related( 'rfam_cms',
                                             { version => $version } );
  }
  else {
    $c->log->debug( 'Family::cm: looking for latest CM' ) if $c->debug;  
    $rs = $c->stash->{rfam}->search_related( 'rfam_cms',
                                             {},
                                             { order_by => 'version DESC' } );
  }

  my $gzipped_cm;
  unless ( defined $rs and 
           $rs->first  and
           $gzipped_cm = $rs->first->cm ) {
    $c->stash->{errorMsg} = 'We could not find a covariance model that was built with that version of infernal.';
    return;
  }
  
  my $cm = Compress::Zlib::memGunzip( $gzipped_cm );
  unless ( defined $cm ) {
    $c->stash->{errorMsg} = 'We could not uncompress the covariance model file.';
    return;
  }

  my $filename = $c->stash->{acc} . '.cm';
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $cm );
}

#---------------------------------------

=head2 old_cm : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_cm : Path( '/family/cm' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_cm: redirecting to "cm"' )
    if $c->debug;

  my ( $version ) = $c->req->param('version') || 1.0 =~ m/^(\d+\.\d+)$/;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{version};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'cm', $version || '', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 regions : Local

Builds a tab-delimited file containing all regions for this family

=cut

sub regions : Chained( 'family' )
              PathPart( 'regions' )
              Args( 0 ) {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family::regions: building tab-delimited list of regions' )
    if $c->debug;

  if ( $c->stash->{rfam}->num_full <= $this->{regionsLimits}->{showText} ) {
    $c->log->debug( 'Family::regions: num_full <= showText limit; retrieving regions later' )
      if $c->debug;
    $c->forward( 'get_regions_data' );
  }
  else {
    $c->log->debug( 'Family::regions: num_full > showText limit; not showing regions' )
      if $c->debug;

    $c->res->status( 403 );
    $c->res->body( 'The family has too many regions to list.' );

    return;
  }

  unless ( defined $c->stash->{regions} ) {
    $c->log->debug( 'Family::regions: failed to retrieve regions' )
      if $c->debug;

    $c->res->status( 500 ); # Internal server error
    $c->res->body( 'There was a problem retrieving the regions.' );

    return;
  }
  
  # build a sensible filename
  my $filename = $c->stash->{acc} . '_regions.txt';
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # add a meaningful header
  my $regions = '# Rfam regions for family ' . $c->stash->{rfam}->rfam_id 
                . ' (' . $c->stash->{rfam}->rfam_acc . ")\n"
                . '# file built ' . localtime(). ' using Rfam version ' 
                . $c->stash->{relData}->rfam_release
                . ' (released ' . $c->stash->{relData}->rfam_release_date . ")\n";

  # add the rows
  foreach my $region ( @{ $c->stash->{regions} } ) {
    $regions .= $region->get_column('rfamseq_acc' ) . "\t";
    $regions .= $region->bits_score . "\t";
    $regions .= $region->seq_start . "\t";
    $regions .= $region->seq_end . "\t";
    $regions .= $region->get_column('description' ) . "\t";
    $regions .= $region->get_column('species' ) . "\n";
  }

  # stuff the content into the response and we're done
  $c->res->body( $regions );
}

#---------------------------------------

=head2 old_regions : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_regions : Path( '/family/regions' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_regions: redirecting to "regions"' )
    if $c->debug;

  my ( $version ) = $c->req->param('version') || 1.0 =~ m/^(\d+\.\d+)$/;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'regions', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- tree actions ----------------------------------------------------------------
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

=head2 tree : Chained('family') PathPart('tree') CaptureArgs(1)

Mid-point in a chain for handling trees for a family. Requires one argument,
"species" or "acc", setting the label style to be applied.

=cut

sub tree_labels : Chained( 'tree' )
                  PathPart( '' )
                  CaptureArgs( 1 ) {
  my ( $this, $c, $label ) = @_;
  
  $c->stash->{label} = ( $label || '' ) eq 'species' ? 'species' : 'acc';

  $c->log->debug( 'Family::tree_labels: labelling ' . $c->stash->{alnType} 
                  . ' tree with ' . $c->stash->{label} . ' labels' )
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

  return unless defined $c->stash->{treeData};

  my $filename = $c->stash->{acc} . '_' . $c->stash->{alnType} . '.nhx';
  $c->log->debug( 'Family::tree_data: tree data: |' . $c->stash->{treeData} . '|' )
    if $c->debug;

  $c->log->debug( "Family::tree_data: tree filename: |$filename|" )
    if $c->debug;

  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $c->stash->{treeData} );
}

#-------------------------------------------------------------------------------

=head2 tree_map : Path

Returns an HTML snippet with a link to the tree image and associated image map for 
the specified seed/full alignment with the given label type (acc/species). Also
builds the image and caches it, so that the request for the image won't have to
wait.

=cut

sub tree_map : Chained( 'tree_labels' )
               PathPart( 'map' )
               Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache the page (fragment) for one week
  $c->cache_page( 604800 );

  # stash the tree object
  $c->forward( 'get_tree' );

  # populate the tree nodes with the areas for the image map
  $c->stash->{tree}->plot_core 
    if defined $c->stash->{tree};
  # set up the TT view
  $c->stash->{template} = 'components/blocks/family/treeMap.tt';

  $c->log->debug( 'Family::tree_map: rendering treeMap.tt' )
    if $c->debug;
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

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'tree',
                     $aln_type, $label, 'map', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 tree_image : Chained('tree_labels') PathPart('image') Args(0)

If we successfully generated a tree image, returns it directly as
an "image/gif". Otherwise returns a blank image.

=cut

sub tree_image : Chained( 'tree_labels' )
                 PathPart( 'image' )
                 Args( 0 ) {
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

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'tree',
                     $aln_type, $label, 'image', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- structure actions -----------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 structures : Path

Retrieves the list of PDB entries for this family. If a PDB ID is specified,
the method also retrieves the row of the "pdb" table for that entry.

=cut

# sub structures : Path {
#   my ( $this, $c ) = @_;
# 
#   # see if we were handed a PDB ID and, if so, put the data for that entry into
#   # the stash
#   if ( defined $c->req->param('pdbId') and
#       $c->req->param('pdbId') =~ m/^(\d\w{3})$/ ) {
# 
#     $c->stash->{pdbObj} = $c->model('RfamDB::Pdb')
#                             ->find( { pdb_id => $1 } );
#   }
# 
#   # retrieve the PDB entries for this family
#   my @rs;
#   if ( defined $c->stash->{rfam}->auto_rfam ) {
#     @rs = $c->model('RfamDB::PdbRfamReg')
#             ->search( { auto_rfam  => $c->stash->{rfam}->auto_rfam},
#                       { join       => [ qw( pdb ) ],
#                         prefetch   => [ qw( pdb ) ] } );
#   }
# 
#   my %pdbUnique = map{ $_->pdb_id => $_ } @rs;
#   $c->stash->{pdbUnique} = \%pdbUnique;
# 
#   # set up the view and rely on "end" from the parent class to render it
#   $c->stash->{template} = 'components/blocks/family/familyStructures.tt';
# 
#   # cache the template output for one week
#   #$c->cache_page( 604800 );
# }

#-------------------------------------------------------------------------------

=head2 structures : Chained('family') PathPart('structures') Args(0)

Renders a table showing the mapping between Rfam family, UniProt region and
PDB residues.

=cut

sub structures : Chained( 'family' )
                 PathPart( 'structures' )
                 Args( 0 ) {
  my ( $this, $c ) = @_;

  # cache the template output for one week
  $c->cache_page( 604800 );

  $c->log->debug( 'Family::structures: showing structures that map to ' . $c->stash->{acc} )
    if $c->debug;

  my @mapping = $c->model('RfamDB::PdbRfamReg')
                  ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam },
                            {} );

  $c->stash->{rfamMaps} = \@mapping;

  $c->stash->{template} = 'components/blocks/family/structureTab.tt';
}

#---------------------------------------

=head2 old_structures : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_structures : Path( '/family/structures/mapping' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_structures: redirecting to "structures"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'structures', $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- alignment actions -----------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 alignment : Chained('family') PathPart('alignment') CaptureArgs(1)

Mid-point in a chain for handling alignments. Captures various parameters:

=over

=item alnType

alignment type, "seed" or "full"

=item nseLabels

use "name/start-end" to label sequences ? default is to use species names

=item view

flag the alignment for download (add Content-Disposition header) ?

=item gzip

should the output alignment be gzip-compressed ?

=back

=cut

sub alignment : Chained( 'family' )
                PathPart( 'alignment' )
                CaptureArgs( 1 ) {
  my ( $this, $c, $aln_type ) = @_;

  # which alignment, what type of labels, view or download, and should we gzip
  # the output, if that makes sense for the specified format ?
  $c->stash->{alnType}    = ( $aln_type || '' ) eq 'seed' ? 'seed' : 'full'; 
  $c->stash->{nseLabels}  = ( $c->req->param('nseLabels') || 0 ) ? 1 : 0;
  $c->stash->{view}       = ( $c->req->param('view')      || 0 ) ? 1 : 0;
  $c->stash->{gzip}       = ( $c->req->param('gzip')      || 0 ) ? 1 : 0;

  if ( $c->debug ) {
    $c->log->debug( 'Family::alignment: which alignment type ?      ' . $c->stash->{alnType} );
    $c->log->debug( 'Family::alignment: use name/start-end labels ? ' . $c->stash->{nseLabels} );
    $c->log->debug( 'Family::alignment: view rather than download ? ' . $c->stash->{view} );
    $c->log->debug( 'Family::alignment: gzipped output ?            ' . $c->stash->{gzip} );
  }
}

#---------------------------------------

=head2 alignment_default : Chained('family') PathPart('alignment') Args(0)

Catches requests with no alignment type (seed or full) and no format specified.
Defaults to Stockholm-format seed alignment.

=cut 

sub alignment_default: Chained( 'family' ) 
                       PathPart( 'alignment' )
                       Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->forward( 'alignment',        [ 'seed' ] );
  $c->forward( 'alignment_format', [ 'stockholm' ] );
}

#---------------------------------------

=head2 alignment_format_default : Chained('alignment') PathPart('') Args(0)

Catches requests with an alignment type (seed or full) but no format specified.
Defaults to Stockholm-format alignment.

=cut 

sub alignment_format_default : Chained( 'alignment' ) 
                               PathPart( '' )
                               Args( 0 ) {
  my ( $this, $c ) = @_;

  $c->forward( 'alignment_format', [ 'stockholm' ] );
}

#---------------------------------------

=head2 alignment_format : Chained('alignment') PathPart('') Args(1)

Outputs the specified seed or full alignment in the specified format.
Supported formats, given as the first argument, are:

=over

=item * pfam

Stockholm with sequences on a single line

=item * fasta

Gapped FASTA format

=item * fastau

Vanilla FASTA format; ungapped

=item * stockholm

Standard Stockholm format

=back

The exact styles (other than C<pfam>) are defined by C<esl-reformat>. There is
no default. If the specified format is not in the supported list, we throw an
error.

=cut

# these are the supported output file formats. This is a map between a format
# specified by the user and the format name used by esl-reformat
my %supported_formats = (
  stockholm  => 'stockholm',
  pfam       => 'pfam',
  fasta      => 'afa',   # regular, gapped fasta
  fastau     => 'fasta', # ungapped fasta
);                        

sub alignment_format : Chained( 'alignment' ) 
                       PathPart( '' )
                       Args( 1 ) 
                       ActionClass( 'REST' ) { }

sub alignment_format_GET {
  my ( $this, $c, $format ) = @_;

  unless ( defined $format and 
           $format =~ m/^\w+$/ ) {
    $c->detach( 'alignment_default' );
    return;
  }

  # the output...
  $c->stash->{output_alignment} = ''; # a slot to hold the output alignment
  $c->stash->{is_gzipped} = 0;        # a flag: is it gzipped ?
  $c->stash->{filename} = '';         # the output filename, if needed

  # what format should we write ?
  if ( $format eq 'jalview' ) {
    # hand off to a jalview tool window
    $c->stash->{template} = 'components/tools/jalview.tt';
    return;
  }
  elsif ( $format eq 'html' ) {
    # builds an HTML page that displays the alignment as paged blocks
    $c->forward( 'get_html_alignment' );
    if ( $c->stash->{errorMsg} ) {
      $c->log->debug( 'Family::alignment_format: failed to retrieve HTML alignment' )
        if $c->debug;
      return;
    }

    $c->stash->{template} = 'components/tools/html_alignment.tt';
    return;
  }
  elsif ( $format eq 'colorstock' ) {
    # colorstock: HTML marked-up stockholm
    $c->forward('get_colorstock_alignment' );

    unless ( $c->stash->{gzipped_alignment} ) {
      $c->log->debug( 'Family::alignment_format: failed to retrieve colorstock alignment' )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem retrieving the colorstock alignment for '
                              . $c->stash->{acc};

      return;
    }

    $c->stash->{output_alignment} = $c->stash->{gzipped_alignment};
    $c->stash->{is_gzipped}       = 1;
    $c->stash->{filename}         = $c->stash->{acc} . '.' . $c->stash->{alnType} . '.colorstock.html.gz';
    # $c->stash->{gzip}             = 1; # force colorstock to be gzipped
  }
  else {
    # everything else. This bit handles stockholm, pfam, fasta and ungapped fasta
    $c->forward('get_gzipped_alignment');

    unless ( $c->stash->{gzipped_alignment} ) {
      $c->log->debug( 'Family::alignment_format: failed to retrieve raw alignment' )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem retrieving the raw alignment for '
                              . $c->stash->{acc};
      return;
    }

    $c->forward( 'format', [ $format ] );
  }

  # TODO check for an error in $c->stash->{errorMsg}, originating from the forward

  my $output;

  # should the output by gzipped ?
  if ( $c->stash->{gzip} ) {
    $c->res->content_type( 'application/x-gzip' );
    $output = $c->stash->{is_gzipped}
            ? $c->stash->{output_alignment}
            : Compress::Zlib::memGzip( $c->stash->{output_alignment} );
  }
  else {
    $c->res->content_type( 'text/plain' );
    $output = $c->stash->{is_gzipped}
            ? Compress::Zlib::memGunzip( $c->stash->{output_alignment} )
            : $c->stash->{output_alignment};
  }

  unless ( defined $output ) {
    $c->log->debug( 'Family::alignment_format: failed to gzip/gunzip the alignment' )
      if $c->debug;      

    $c->stash->{errorMsg} = 'There was a problem building the requested alignment for '
                            . $c->stash->{acc};

    return;
  }

  # should we mark the output for download ? (default is to set download header)
  if ( not $c->stash->{view} ) {
    $c->res->header( 'Content-disposition' => 'attachment; filename=' . $c->stash->{filename} );
  }

  # and dump the alignment to the response
  $c->res->body( $output );
}

#---------------------------------------

=head2 old_jalview : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_jalview : Path( '/family/alignment/jalview' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_jalview: redirecting to "jalview"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed' ? 'seed' : 'full';

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{viewer};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                     $aln_type, 'jalview', $c->req->params ) );
}

#---------------------------------------

=head2 old_html : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_html : Path( '/family/alignment/html' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_html: redirecting to "html"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed' ? 'seed' : 'full';

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{viewer};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                     $aln_type, 'html', $c->req->params ) );
}

#---------------------------------------

=head2 old_gzipped : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_gzipped : Path( '/family/alignment/download/gzipped' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_gzipped: redirecting to "gzipped"' )
    if $c->debug;

  my $aln_type   = ( $c->req->param('alnType') || '' ) eq 'seed' ? 'seed' : 'full';
  my $colorstock = ( $c->req->param('cs') || 0 ) ? 1 : 0;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{cs};

  if ( $colorstock ) {
    $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                       $aln_type, 'colorstock', $c->req->params ) );
  }
  else {
    # firkle with the params. Need to add "gzip=1" to make sure the output 
    # really is gzipped
    my $params = { %{ $c->req->params }, 'gzip' => 1 };

    $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                       $aln_type, 'stockholm', $params ) );
  }
}

#---------------------------------------

=head2 old_format : Path

Deprecated. Stub to redirect to the chained action.

=cut

sub old_format : Path( '/family/alignment/download/format' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::old_format: redirecting to "format"' )
    if $c->debug;

  my $aln_type = ( $c->req->param('alnType') || '' ) eq 'seed' ? 'seed' : 'full';

  my $output_format = $this->{default_output_format};
  if ( defined $c->req->param('format') and
       exists $supported_formats{ $c->req->param('format') } ) {
    $output_format = $c->req->param('format');
  }

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{alnType};
  delete $c->req->params->{format};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'alignment', 
                     $aln_type, $output_format, $c->req->params ) );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get_data : Private

Retrieves family data for the given entry. Accepts the entry ID or accession as
the first argument. Does not return any value but drops the L<ResultSet> for
the relevant row into the stash.

=cut

sub get_data : Private {
  my ( $this, $c, $entry ) = @_;
  
  # check for a family
  my $rfam = $c->model('RfamDB::Rfam')
               ->search( [ { rfam_acc => $entry },
                           { rfam_id  => $entry } ] )
               ->single;
                         
  unless ( defined $rfam ) {
    $c->log->debug( 'Family::get_data: no row for that accession/ID' )
      if $c->debug;
  
    $c->stash->{errorMsg} = 'No valid Rfam family accession or ID';

    return;
  }  

  $c->log->debug( 'Family::get_data: got a family' )
    if $c->debug;

  $c->stash->{rfam} = $rfam;
  $c->stash->{acc}  = $rfam->rfam_acc;

}

#-------------------------------------------------------------------------------

=head2 get_summary_data : Private

Retrieves summary data for the family. For most fields this is a simple look-up
on the Rfam object that we already have, but for the number of interactions
we have to do one more query.

=cut

sub get_summary_data : Private {
  my ( $this, $c ) = @_;

  my $summaryData = {};

  # number of sequences in full alignment
  $summaryData->{numSequences} = $c->stash->{rfam}->num_full;

  # number of structures known for the domain
  my $rs = $c->model('RfamDB::PdbRfamReg')
             ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam },
                       {} );

  $summaryData->{numStructures} = $rs->count;

  # Number of species
  $summaryData->{numSpecies} = $c->stash->{rfam}->number_of_species;

  # number of interactions
  $summaryData->{numInt} = 0;

  $c->stash->{summaryData} = $summaryData;

  # get tree curation data. Limit retrieved columns to avoid pulling down
  # alignments and tree data until we really need them.
  my $full_tree_data = $c->model( 'RfamDB::AlignmentsAndTrees' )
                         ->find( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                                   type      => 'full' },
                                 { columns => [ qw{ type 
                                                    treemethod 
                                                    average_length 
                                                    percent_id 
                                                    number_of_sequences 
                                                    most_unrelated_pair } ] } );

  my $seed_tree_data = $c->model( 'RfamDB::AlignmentsAndTrees' )
                         ->find( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                                   type      => 'seed' },
                                 { columns => [ qw{ type 
                                                    treemethod 
                                                    average_length 
                                                    percent_id 
                                                    number_of_sequences 
                                                    most_unrelated_pair } ] } );

  $c->stash->{alignment_info}->{full} = $full_tree_data;
  $c->stash->{alignment_info}->{seed} = $seed_tree_data;
}

#-------------------------------------------------------------------------------

=head2 get_regions_data : Private

Retrieves sequence data for the family.

=cut

sub get_regions_data : Private {
  my ( $this, $c ) = @_;
  
  # save some typing...
  my $rfam = $c->stash->{rfam};

  $c->log->debug( 'Family::get_regions_data: family has |'
                  . $rfam->num_full . '| regions' ) if $c->debug;

  my @regions = $c->model('RfamDB::RfamRegFull')
                  ->search( { auto_rfam => $rfam->auto_rfam },
                            { join      => { 'auto_rfamseq' => 'ncbi_id' },
                              '+select' => [ qw( auto_rfamseq.rfamseq_acc
                                                 auto_rfamseq.description
                                                 ncbi_id.species
                                                 auto_rfamseq.length ) ],
                              '+as'     => [ qw( rfamseq_acc
                                                 description
                                                 species
                                                 length ) ],
                              order_by  => [ 'bits_score DESC' ] } );
                                            
  $c->stash->{regions} = \@regions;

  $c->log->debug( 'Family::get_regions_data: added |' . scalar @regions
                  . '| regions to stash' ) if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 get_wikipedia : Private

Retrieves the wikipedia content, if any, for this family.

=cut

sub get_wikipedia : Private {
  my ( $this, $c ) = @_;

  my $article = $c->model('WebUser::ArticleMapping')
                  ->search( { accession => $c->stash->{acc} },
                            { join     => [ 'wikitext' ],
                              prefetch => [ 'wikitext' ] } )
                  ->next;
  
  return unless ( $article and $article->wikitext );

  $c->log->debug( 'Family::get_wikipedia: got wiki title: |'
                  . $article->title . '|' )
    if $c->debug;

  $c->stash->{article} = $article;
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
    $c->log->debug( 'Family::Tree::get_tree_data: extracted tree data from cache' )
      if $c->debug;  
  }
  else {
    $c->log->debug( 'Family::Tree::get_tree_data: failed to extract tree data from cache; going to DB' )
      if $c->debug;  

    # retrieve the tree from the DB
    my $rs = $c->model('RfamDB::AlignmentsAndTrees')
               ->search( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                           type      => $c->stash->{alnType} },
                         { columns => [ 'tree' ] } );
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
    $tree_data = Compress::Zlib::memGunzip( $row->tree );
    if ( defined $tree_data ) {
      $c->log->debug( 'Family::Tree::get_tree_data: successfully gunzipped tree data' )
        if $c->debug;
    }
    else {
      $c->log->debug( 'Family::Tree::get_tree_data: tree data not gzipped...' )
        if $c->debug;
      $tree_data = $row->tree;
    }

    unless ( defined $tree_data ) {
      $c->log->debug( 'Family::Tree::get_tree_data: failed to retrieve tree data' )
        if $c->debug;
      $c->stash->{errorMsg} = 'We could not extract the tree data for '
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
#- private alignment download actions ------------------------------------------
#-------------------------------------------------------------------------------

=head2 format : Private

Builds a plain text (no markup) alignment. Re-formats alignments (using
esl-reformat) into various output formats.

=cut

sub format : Private {
  my ( $this, $c, $format ) = @_;

  # translate the requested format into a real "output format". That is,
  # something that esl-reformat will be happy with
  $format ||= '';
  $format   = lc $format;

  my $output_format;
  if ( exists $supported_formats{ $format } ) {
    $output_format = $supported_formats{ $format };
    $c->log->debug( "Family::format: generating '$output_format' alignment" )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::format: no supported format specified' )
      if $c->debug;

    $c->stash->{errorMsg} = 'Not a valid alignment format';

    return;
  }

  #---------------------------------------

  if ( $output_format eq 'stockholm' ) {

    # special case: we can dump out the raw alignment directly if the user
    # wants stockholm format
    $c->log->debug( 'Family::format: stockholm format requested' )
      if $c->debug;

    $c->stash->{output_alignment} = $c->stash->{gzipped_alignment};
    $c->stash->{is_gzipped}       = 1;
    $c->stash->{filename}         = $c->stash->{acc} . '_' . $c->stash->{alnType}. '.stockholm.txt';
  }
  else {

    # use esl-reformat to output the requested format

    # get a temporary file and filehandle
    my ( $fh, $fn ) = tempfile();
    unless ( $fn and $fh ) {
      $c->log->debug( "Family::format: couldn't open temp file for alignment: $!" )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem while reformatting the alignment for '
                              . $c->stash->{acc};

      return;
    }

    # dump the alignment to that file
    print $fh Compress::Zlib::memGunzip( $c->stash->{gzipped_alignment} );
    close $fh;

    # build the command for running esl-reformat    
    my $cmd = $this->{eslreformat_binary} . " -u -r --mingap --informat stockholm $output_format $fn";
    $c->log->debug( "Family::format: running system command: |$cmd|" )
      if $c->debug;

    unless ( open OUTPUT, "$cmd|" ) {    
      $c->log->debug( "Family::format: couldn't run esl-reformat: $!" )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem reformatting the alignment for '
                              . $c->stash->{acc};

      return;
    }

    # stick the output of esl-reformat back together and we're done
    $c->stash->{output_alignment} = join '', <OUTPUT>;
    $c->stash->{is_gzipped}       = 0;
    $c->stash->{filename}         = $c->stash->{acc} . '.' . $c->stash->{alnType} . ".$output_format.txt";

    # tidy up
    close OUTPUT;
    unlink $fn;

    # as far as I can see, this SHOULD work too, and, if it did, would have the 
    # advantage of not requiring any temp files at all...
    #    my $cmd = $this->{eslreformat_binary} . " --informat stockholm $output_format -";
    #    $c->log->debug( "Family::format: running esl-reformat command: |$cmd|" )
    #      if $c->debug;
    #     
    #    my ( $out, $in );
    #    my $pid = open2( $out, $in, $cmd );
    #    unless ( $pid ) {
    #      $c->log->error( "Family::format: failed to run esl-reformat command ($cmd): $!" );
    #      return undef;
    #    }
    #    $c->log->debug( "Family::format: running esl-reformat with PID $pid" )
    #      if $c->debug;
    #
    #    $c->log->debug( 'Family::format: raw alignment: '
    #                    . $c->stash->{alignment} )
    #      if $c->debug;
    #
    #    print $in $c->stash->{alignment};
    #    close $in;
    #    $c->log->debug( 'Family::format: printed input...' )
    #      if $c->debug;
    #
    #    $output = join '', <$out>;
    #
    #    $c->log->debug( "Family::format: esl-reformat output: |$output|" )
    #      if $c->debug;
  }
}

#-------------------------------------------------------------------------------

=head2 get_html_alignment : Private

Retrieves the HTML alignment. We first try to extract the HTML from the cache
or, if that fails, we retrieve it from the DB.

=cut

sub get_html_alignment : Private {
  my ( $this, $c ) = @_;

  # get all of the blocks  
  my @rs = $c->stash->{rfam}->search_related( 'html_alignments',
                                              { type => $c->stash->{alnType} } );

  unless ( scalar @rs ) {
    $c->log->debug( 'Family::html_alignment: failed to retrieve an alignment block' )
      if $c->debug;

    $c->stash->{errorMsg} = 'There was a problem extracting the requested alignment block for '
                            . $c->stash->{acc};

    return;
  }

  $c->log->debug( 'Family::html_alignment: found ' . scalar @rs . ' blocks' )
    if $c->debug;

  # decide which block to show
  my ( $block_num ) = ( $c->req->param('block') || 0 ) =~ m/^(\d+)$/;
  $block_num ||= 0;

  $c->log->debug( "Family::html_alignment: showing block $block_num" )
    if $c->debug;

  # gunzip the html
  my $gzipped_html = $rs[$block_num]->html;
  my $block = Compress::Zlib::memGunzip( $gzipped_html );
  unless ( defined $block ) {
    $c->log->debug( 'Family::html_alignment: failed to gunzip the alignment block' )
      if $c->debug;

    $c->stash->{errorMsg} = 'There was a problem uncompressing an alignment block for '
                            . $c->stash->{acc};

    return;
  }
 
  # stash the stuff that's used for paging in the template
  $c->stash->{alignment_block}   = $block;
  $c->stash->{current_block_num} = $block_num;
  $c->stash->{last_block_num}    = scalar @rs - 1;
}

#-------------------------------------------------------------------------------

=head2 get_gzipped_alignment : Private

Retrieves the gzipped alignment from the database and stashes it, still 
compressed. Caches the gzipped alignment too, if caching is enabled. 

=cut

sub get_gzipped_alignment : Private {
  my ( $this, $c ) = @_;

  my $alnType = $c->stash->{alnType} . ( $c->stash->{nseLabels} ? '' : 'Tax' );
  $c->log->debug( "Family::Alignment::Download::get_gzipped_alignment: setting alignment type to |$alnType|" )
    if $c->debug;
  
  # first try the cache...
  my $cacheKey = 'gzipped_alignment'
                 . $c->stash->{acc}
                 . $alnType;
  my $gzipped_alignment = $c->cache->get( $cacheKey );

  if ( defined $gzipped_alignment ) {
    $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: extracted gzipped alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: failed to extract gzipped alignment from cache; going to DB' )
      if $c->debug;

    # failed to get a cached version; retrieve the alignment from the DB
    my $rs = $c->stash->{rfam}->search_related( 'alignments_and_trees',
                                                { type => $alnType },
                                                { columns => [ 'alignment' ] } );

    # make sure the query returned something
    unless ( defined $rs ) {
      $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: failed to retrieve a row' )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem retrieving the alignment data for '
                              . $c->stash->{acc};

      return;
    }

    # make sure we can get the alignment out of the returned row
    unless ( $gzipped_alignment = $rs->first->alignment ) {
      $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: failed to retrieve an alignment' )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem uncompressing the alignment data for '
                              . $c->stash->{acc};

      return;
    }

    # cache the gzipped alignment
    $c->log->debug( 'Family::Alignment::Download::get_gzipped_alignment: retrieved gzipped alignment from DB' )
      if $c->debug;
    $c->cache->set( $cacheKey, $gzipped_alignment ) unless $ENV{NO_CACHE};
  }

  $c->stash->{gzipped_alignment} = $gzipped_alignment;
}

#-------------------------------------------------------------------------------

=head2 get_colorstock_alignment : Private

Retrieves the gzipped "colorstock" HTML alignment from the database and stashes 
it, still compressed. Caches the gzipped alignment too, if caching is enabled.

=cut

sub get_colorstock_alignment : Private {
  my ( $this, $c ) = @_;

  my $alnType = $c->stash->{alnType} . 'Colorstock';
  $c->log->debug( "Family::get_colorstock_alignment: setting alignment type to |$alnType|" )
    if $c->debug;
  
  # first try the cache...
  my $cache_key = 'gzipped_alignment'
                  . $c->stash->{acc}
                  . $alnType;
  my $gzipped_alignment = $c->cache->get( $cache_key );
  
  if ( defined $gzipped_alignment ) {
    $c->log->debug( 'Family::get_colorstock_alignment: extracted gzipped alignment from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::get_colorstock_alignment: failed to extract gzipped alignment from cache; going to DB' )
      if $c->debug;

    # failed to get a cached version; retrieve the alignment from the DB
    my $rs = $c->stash->{rfam}->search_related( 'html_alignments',
                                                { type => $alnType },
                                                { columns => [ 'html' ] } );

    # make sure the query returned something
    unless ( defined $rs ) {
      $c->log->debug( 'Family::get_colorstock_alignment: failed to retrieve a row' )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem retrieving the colorstock alignment for '
                              . $c->stash->{acc};

      return;
    }

    # make sure we can retrieve the gzipped HTML alignment from the row object
    $gzipped_alignment = $rs->first->html;

    unless ( defined $gzipped_alignment ) {
      $c->log->debug( 'Family::get_colorstock_alignment: failed to retrieve HTML' )
        if $c->debug;

      $c->stash->{errorMsg} = 'There was a problem retrieving the HTML colorstock alignment for '
                              . $c->stash->{acc};

      return;
    }

    # cache the gzipped alignment
    $c->log->debug( 'Family::get_colorstock_alignment: retrieved gzipped alignment from DB' )
      if $c->debug;
    $c->cache->set( $cache_key, $gzipped_alignment ) unless $ENV{NO_CACHE};
  }

  $c->stash->{gzipped_alignment} = $gzipped_alignment;
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

__PACKAGE__->meta->make_immutable;

1;

__DATA__
