
# Methods.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Family::Methods - role to add various methods to the family page
controller

=cut

package RfamWeb::Roles::Family::Methods;

=head1 DESCRIPTION

This is a role to add various family page-related methods to the Family 
controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

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

  # get the gzip compressed JSON string for the structure annotation
  my $rs = $c->stash->{db}->resultset('SecondaryStructureImage')
             ->find( { auto_rfam => $c->stash->{rfam}->auto_rfam,
                       type      => 'ss' } );

  unless ( $rs and $rs->image ) {
    $c->stash->{rest}->{error} ||= 
      'We could not retrieve the secondary structure data for Rfam family ' 
      . $c->stash->{acc} . '.';
  }

  # try to uncompress it
  my $json_string = Compress::Zlib::memGunzip( $rs->image );
  unless ( $json_string ) {
    $c->stash->{rest}->{error} ||=
      'We could not encode the secondary structure data for Rfam family ' 
      . $c->stash->{acc} . '.';
  }

  # bail out now if there was an error
  if ( $c->stash->{rest}->{error} ) {
    $c->res->status(500); # Internal server error

    $c->stash->{template} = 'components/blocks/family/error.tt';

    return;
  }

  # data appear to be intact; return the VARNA applet wrapper page
  $c->stash->{template} = 'components/tools/varna.tt';

  # for the benefit of VARNA, we need to decode the JSON so we can work with it
  # as a regular perl data structure, convert the A/a notation to [/] and
  # similarly for B/b to {/}, then re-encode it and stash it for the template
  my $json = JSON->new;
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

  $c->log->debug( 'Family::Methods::old_varna: redirecting to "varna"' )
    if $c->debug;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $entry_arg, 'varna', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 image : Local

Retrieves and returns a secondary structure image from the database. Caches the
image, unless C<$ENV{NO_CACHE}> is true. 

=cut

sub image : Chained( 'family' )
            PathPart( 'image' )
            Args( 1 ) {
  my ( $this, $c, $type ) = @_;
  
  my ( $image_type ) = $type || '' =~ m/^(\w+)$/;
  $c->log->debug( "Family::Methods::image: image_type: |$image_type|" )
    if $c->debug;

  unless ( defined $image_type and $image_type ) {
    $c->log->debug( 'Family::Methods::image: no valid type specified; defaulting to normal' )
      if $c->debug;
    $image_type = 'normal';
  }

  my $cache_key = 'family_image' . $c->stash->{acc} . $image_type;
  my $image     = $c->cache->get( $cache_key );
  
  if ( defined $image ) {
    $c->log->debug( 'Family::Methods::image: retrieved image from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Family::Methods::image: failed to retrieve image from cache; going to DB' )
      if $c->debug;

    my $rs = $c->stash->{db}->resultset('SecondaryStructureImage')
               ->find( { rfam_acc => $c->stash->{acc},
                         type      => $image_type } );

    unless ( defined $rs and
             defined $rs->image ) {
      $c->detach( 'no_alignment' );
      return;
    }

    $image = Compress::Zlib::memGunzip( $rs->image );
    unless ( $image ) {
      $c->log->debug( "Family::Methods::image: couldn't uncompress image: $Compress::Zlib::gzerrno" );
      $c->detach( 'no_alignment' );
      return;
    }

    $c->cache->set( $cache_key, $image ) unless $ENV{NO_CACHE}
  }
  
  # cache the template output for one week
  $c->cache_page( 604800 );

  $c->res->content_type( $image_type eq 'rchie' ? 'image/png' : 'image/svg+xml' );
  $c->res->body( $image );
}

#---------------------------------------

=head2 old_image : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_image : Path( '/family/image' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::Methods::old_image: redirecting to "image"' )
    if $c->debug;

  my ( $image_type ) = $c->req->param('type') || 'normal' =~ m/^(\w+)$/;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};
  delete $c->req->params->{type};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'image', $image_type, $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 cm : Local

Serves the CM file for this family.

=cut

sub cm : Chained( 'family' )
         PathPart( 'cm' )
         Args( 0 ) {
  my ( $this, $c ) = @_;

  # set the template that will show any error messages, in cases when the
  # client requests XML or HTML. Default to an HTML error message
  $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' )
                        ? 'rest/family/error_xml.tt'
                        : 'components/blocks/family/error.tt';

  $c->log->debug( 'Family::Methods::cm: returning compressed CM' )
    if $c->debug;
  my $cm = $c->stash->{rfam}->annotated_file->unzipped_cm;

  unless ( defined $cm ) {
    $c->log->debug( 'Family::Methods::cm: failed to retrieve CM' )
      if $c->debug;

    $c->res->status(500); # Internal server error
    $c->stash->{rest}->{error} = "We could not return the covariance model file.";

    return;
  }

  # cache the output for one week
  $c->cache_page( 604800 );

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

  $c->log->debug( 'Family::Methods::old_cm: redirecting to "cm"' )
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
              Args( 0 )
              ActionClass( 'REST' ) {}

sub regions_GET : Private {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family::Methods::regions: building tab-delimited list of regions' )
    if $c->debug;

  # default to the error template
  $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' ) 
                        ? 'rest/family/error_xml.tt'
                        : 'rest/family/error_text.tt';

  # impose a limit on the number of regions we're prepared to show
  if ( $c->stash->{rfam}->num_full > $this->{regionsLimits}->{showText} ) {
    $c->log->debug( 'Family::Methods::regions: num_full > showText limit; not showing regions' )
      if $c->debug;

    $this->status_forbidden( $c, message => 'The family has too many regions to list.' );

    return;
  }

  $c->log->debug( 'Family::Methods::regions: num_full <= showText limit; retrieving regions' )
    if $c->debug;

  $c->forward( 'get_regions_data' );

  # make sure we got *some* regions
  unless ( defined $c->stash->{region_rs} ) {
    $c->log->debug( 'Family::Methods::regions: failed to retrieve regions' )
      if $c->debug;

    $c->res->status(500); # Internal server error
    $c->stash->{rest}->{error} ||= 'There was a problem retrieving the regions.';

    return;
  }
  
  # build a sensible filename
  my $filename = $c->stash->{acc} . '_regions';

  $filename .= ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml'  ) 
             ? '.xml'
             : '.txt';

  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # cache the template output for one week
  $c->cache_page( 604800 );

  # set the template for serialisers that use TT. Default to plain text
  $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml'  ) 
                        ? 'rest/family/regions_xml.tt'
                        : 'rest/family/regions_text.tt';
}

#---------------------------------------

=head2 old_regions : Path

Deprecated. Stub to redirect to the chained action(s).

=cut

sub old_regions : Path( '/family/regions' ) {
  my ( $this, $c ) = @_;

  $c->log->debug( 'Family::Methods::old_regions: redirecting to "regions"' )
    if $c->debug;

  my ( $version ) = $c->req->param('version') || 1.0 =~ m/^(\d+\.\d+)$/;

  delete $c->req->params->{id};
  delete $c->req->params->{acc};
  delete $c->req->params->{entry};

  $c->res->redirect( $c->uri_for( '/family', $c->stash->{param_entry}, 'regions', $c->req->params ) );
}

#-------------------------------------------------------------------------------

=head2 refseq : Chained('family') PathPart('refseq') Args(0) ActionClass('REST')

Builds a tab-delimited file containing all refseq regions for this family

=cut

sub refseq : Chained( 'family' )
             PathPart( 'refseq' )
             Args( 0 )
             ActionClass( 'REST' ) {}

sub refseq_GET : Private {
  my ( $this, $c ) = @_;
  
  $c->log->debug( 'Family::Methods::refseq: building tab-delimited list of refseqs' )
    if $c->debug;

  # default to the error template
  $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' ) 
                        ? 'rest/family/error_xml.tt'
                        : 'rest/family/error_text.tt';

  # retrieve all regions. We have no way of knowing how many regions there are
  # for this family until we do the query. However, we can build the query and
  # the use "count" to find out (fairly) cheaply.
  my $regions = $c->stash->{db}->resultset('RfamRefseq')
                  ->search( { rfam_acc => $c->stash->{acc} },
                            { } );
 
  # make sure we got a ResultSet back at least
  unless ( defined $regions ) {
    $c->log->debug( 'Family::Methods::refseq: failed to retrieve refseqs' )
      if $c->debug;

    $c->res->status(500); # Internal server error
    $c->stash->{rest}->{error} ||= 'There was a problem retrieving the refseqs.';

    return;
  }
  
  my $num_refseqs = $regions->count;
  $c->stash->{regions} = [ $regions->all ];

  # make sure there are some refseq regions for this family
  unless ( $num_refseqs ) {
    $c->log->debug( 'Family::Methods::refseq: no refseq regions for this fmaily' )
      if $c->debug;

    $this->status_no_content( $c );

    return;
  }

  # impose a limit on the number of refseqs we're prepared to show
  if ( $regions->count > $this->{refseqRegionsLimits}->{showText} ) {
    $c->log->debug( 'Family::Methods::refseq: num refseqs > showText limit; not showing refseqs' )
      if $c->debug;

    $this->status_forbidden( $c, message => 'The family has too many refseqs to list ('
                                            . $regions->count . ')' );

    return;
  }

  $c->log->debug( 'Family::Methods::refseqs: num refseqs <= showText limit; showing' )
    if $c->debug;

  # build a sensible filename
  my $filename = $c->stash->{acc} . '_refseq_regions';
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );

  # add the rows
  $c->stash->{rest}->{refseqs} = [];
  foreach my $region ( @{ $c->stash->{regions} } ) {
    push @{ $c->stash->{rest}->{refseqs} }, [
      $region->refseq_accession,
      $region->bits_score,
      $region->refseq_start,
      $region->refseq_end,
      $region->description,
      $region->species,
      $region->ncbi_id,
    ];
  }

  # cache the template output for one week
  $c->cache_page( 604800 );

  # set the template for serialisers that use TT. Default to plain text
  $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml'  ) 
                        ? 'rest/family/refseqs_xml.tt'
                        : 'rest/family/refseqs_text.tt';
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Sarah Burge, C<sb30@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2012: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Sarah Burge (sb30@sanger.ac.uk), 
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

