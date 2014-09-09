
# Methods.pm
# jt6 20120514 WTSI
#
# $Id$

=head1 NAME

RfamWeb::Roles::Motif::Methods - role to add various methods to the motif page
controller

=cut

package RfamWeb::Roles::Motif::Methods;

=head1 DESCRIPTION

This is a role to add various family page-related methods to the Family 
controller.

$Id$

=cut

use MooseX::MethodAttributes::Role;
use namespace::autoclean;

#-------------------------------------------------------------------------------
#- motif page components ------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 image : Local

Retrieves and returns a secondary structure image from the database. Caches the
image, unless C<$ENV{NO_CACHE}> is true. 

=cut

sub image : Chained( 'motif' )
            PathPart( 'image' )
            Args( 1 ) {
  my ( $this, $c, $family ) = @_;
  
  my ( $rfam_acc ) = $family || '' =~ m/^(\w+)$/;
  $c->log->debug( "Motif::Methods::image: rfam_acc: |$rfam_acc|" )
    if $c->debug;

  unless ( defined $rfam_acc) {
    $c->log->debug( 'Motif::Methods::image: no valid rfam_acc specified' )
      if $c->debug;
  }

  my $cache_key = 'motif_match_image' . $c->stash->{acc} . $rfam_acc;
  my $image     = $c->cache->get( $cache_key );
  
  if ( defined $image ) {
    $c->log->debug( 'Motif::Methods::image: retrieved image from cache' )
      if $c->debug;
  }
  else {
    $c->log->debug( 'Motif::Methods::image: failed to retrieve image from cache; going to DB' )
      if $c->debug;

    my $rs = $c->stash->{db}->resultset('MotifSecondaryStructureImage')
               ->find( { motif_acc => $c->stash->{acc},
                         rfam_acc      => $rfam_acc } );

    unless ( defined $rs and
             defined $rs->image ) {
      $c->detach( 'no_alignment' );
      return;
    }

    $image = Compress::Zlib::memGunzip( $rs->image );
    unless ( $image ) {
      $c->log->debug( "Motif::Methods::image: couldn't uncompress image: $Compress::Zlib::gzerrno" );
      $c->detach( 'no_alignment' );
      return;
    }

    $c->cache->set( $cache_key, $image ) unless $ENV{NO_CACHE}
  }
  
  # cache the template output for one week
  $c->cache_page( 604800 );

  $c->res->content_type( 'image/svg+xml' );
  $c->res->body( $image );
}

#-------------------------------------------------------------------------------

=head2 cm : Local

Serves the CM file for this motif.

=cut

sub cm : Chained( 'motif' )
         PathPart( 'cm' )
         Args() {
  my ( $this, $c) = @_;

 # set the template that will show any error messages, in cases when the
 # client requests XML or HTML. Default to an HTML error message

  $c->stash->{template} = ( ( $c->req->accepted_content_types->[0] || '' ) eq 'text/xml' )
                        ? 'rest/motif/error_xml.tt'
                        : 'components/blocks/motif/error.tt';

  my $rs;
  $c->log->debug( 'Motif::Methods::cm: looking for latest CM' ) if $c->debug;
  $rs = $c->stash->{db}->resultset('MotifFile')
               ->search( { motif_acc => $c->stash->{acc} }, {} );

  my $gzipped_cm = $rs->first->cm;
  unless ( defined $rs and $gzipped_cm) {
    $c->log->debug( 'Motif::Methods::cm:: failed to retrieve a CM' ) if $c->debug;
    $this->status_not_found( $c, message => 'Could not find a covariance model' );
    return;
  }

  my $cm = Compress::Zlib::memGunzip( $gzipped_cm );

  unless ( defined $cm ) {
    $c->log->debug( 'Motif::Methods::cm:: failed to uncompress CM' ) if $c->debug;
    $c->res->status(500);
  }

  my $filename = $c->stash->{acc} . '.cm';
  $c->res->content_type( 'text/plain' );
  $c->res->header( 'Content-disposition' => "attachment; filename=$filename" );
  $c->res->body( $cm );
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

