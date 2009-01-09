
# Image.pm
# jt6 20081215 WTSI
#
# $Id: Image.pm,v 1.2 2009-01-09 12:57:54 jt6 Exp $

=head1 NAME

PfamWeb::Image- a custom L<Image|Bio::Pfam::Drawing::Image::Image> object

=cut

package PfamWeb::Image;

=head1 DESCRIPTION

A custom wrapper around the basic L<Image|Bio::Pfam::Drawing::Image::Image>.
This version stores its images in a CouchDB store, rather than dumping to a
temporary file.


$Id: Image.pm,v 1.2 2009-01-09 12:57:54 jt6 Exp $

=cut

use strict;
use warnings;

use Net::CouchDb;
use MIME::Base64;
use Data::UUID;

use base 'Bio::Pfam::Drawing::Image::Image';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 image_class

Overrides the original method and stores the images in a CouchDB store, rather
than writing the file to disk.

=cut

sub print_image {
  my $this = shift;
  
  # print STDERR "PfamWeb::Image: printing image...\n";

  # check that we have a location for the store
  die "ERROR: no location for image store"
    unless defined $ENV{PFAM_DOMAIN_IMAGES_STORE};
  
  # get a connection to the CouchDB store
  my $cdb = Net::CouchDb->new( uri => $ENV{PFAM_DOMAIN_IMAGES_STORE} );
  
  # get a handle on the database where we'll store the images
  my $image_store = $cdb->db( 'pfam_images' );
  unless ( defined $image_store ) {
    # print STDERR "PfamWeb::Image: no pre-existing database; creating\n";
    $cdb->create_db( 'pfam_images' );
    $image_store = $cdb->db( 'pfam_images' );
  }

  # generate a UUID and use that as the ID for the document in the store and
  # its identifier in the URL  
  my $uuid = Data::UUID->new()->create_str;    

  # create a new Document for every image
  my $doc = Net::CouchDb::Document->new( $uuid );
  
  # set a timestamp and an expiry date
  my $now = DateTime->now;
  my $expires = DateTime->now->add( minutes => 2 );
  
  $doc->created = $now->strftime( '%a %b %d %H:%M:%S %Y' );
  $doc->expires = $expires->strftime( '%a %b %d %H:%M:%S %Y' );
  
  # Base64 encode the image and store it
  $doc->image = encode_base64( $this->image->png );

  $image_store->put( $doc );
  
  # set the image location
  $this->file_location( "/store/image/$uuid" );
  
  # print STDERR "PfamWeb::Image: stored image with ID: |" . $doc->id . "|\n";
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
