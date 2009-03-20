
# Store.pm
# jt6 20081217 WTSI
#
# $Id: Store.pm,v 1.4 2009-03-20 15:56:41 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Store - controller to interface to the CouchDB store

=cut

package PfamWeb::Controller::Store;

=head1 DESCRIPTION

This controller provides actions for accessing documents in a CouchDB store.

$Id: Store.pm,v 1.4 2009-03-20 15:56:41 jt6 Exp $

=cut

use strict;
use warnings;

use MIME::Base64;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Tries to "require" the CouchDB interface module and breaks the processing
chain if it can't be loaded.

=cut

sub auto : Private {
  my ( $this, $c ) = @_;

  eval {
    require Net::CouchDb;
  };
  if ( $@ ) {
    $c->log->error( 'Store::auto: failed to "require" the CouchDB interface module' );
    return 0;
  }
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 image : Local

Description...

=cut

sub image : Local {
  my ( $this, $c, $document_id ) = @_;
  
  $c->log->debug( 'Store::image: retrieving an image from the store' )
    if $c->debug;

  # validate the document ID
  unless ( $document_id =~ m/^([A-F0-9\-]{36})$/i ) {
    $c->log->debug( 'Store::get: "document_id" is not valid' ) if $c->debug;
    $c->res->status( 400 );
    $c->res->body( 'Invalid document identifier' );
    return;
  }

  $c->log->debug( "Store::get: retrieving item from Document with ID |$document_id|" )
    if $c->debug;

  $c->forward( 'get', [ $document_id, 'image' ] );
  
  if ( $c->stash->{getErrorMsg} ) {
    $c->log->debug( 'Store::image: problem retrieving image: |' 
                    . $c->stash->{getErrorMsg} . '|' ) if $c->debug;
    $c->res->status( 500 );
    $c->res->body( $c->stash->{getErrorMsg} );
    return;
  }
  
  $c->log->debug( 'Store::image: successfully retrieved image' )
    if $c->debug;
    
  my $image = decode_base64( $c->stash->{item} );
  unless ( $image ) {
    $c->log->debug( 'Store::image: failed to decode image' ) 
      if $c->debug;
    $c->res->status( 500 );
    $c->res->body( 'Failed to decode image' );
    return;
  }
  
  $c->res->content_type( 'image/png' );
  $c->res->body( $image );
}

#-------------------------------------------------------------------------------
#- private actions -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 get : Private
  
Retrieves an item from the store. Note that there is no validation of the 
document ID or key here; calling methods should detaint anything that comes
from users.

=cut

sub get : Private {
  my ( $this, $c, $document_id, $key ) = @_;

  # get a handle on the CouchDB server
  my $cdb = Net::CouchDb->new( uri => $ENV{PFAM_DOMAIN_IMAGES_STORE} );
  
  # show debug from CouchDB only if we're debugging in catalyst...
  #$cdb->debug( 1 ) if $c->debug;
  
  # get a handle on the CouchDB database
  my $db = $cdb->db( 'pfam_images' );
  unless ( defined $db ) {
    $c->log->debug( "Store::get: couldn't get the CouchDB database" )
      if $c->debug;

    $c->stash->{getErrorMsg} = 'Failed to connect to the store';

    return;
  }
  
  # retrieve the Net::CouchDB::Document 
  my $document = $db->get( $document_id );
  
  # get the requested item from the Document
  my $item = $document->$key;
  unless ( defined $item ) {
    $c->log->debug( "Store::get: couldn't get the requested item ($key) from the Document" )
      if $c->debug;

    $c->stash->{getErrorMsg} = 'Failed to get requested item';

    return;
  }

  # stash the successfully retrieved item
  $c->stash->{item} = $item;
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
