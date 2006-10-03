
# GetImage.pm
# jt6 20060314 WTSI
#
# $Id: GetImage.pm,v 1.7 2006-10-03 14:31:47 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Structure::GetImage - serve an image of the
structure

=cut

package PfamWeb::Controller::Structure::GetImage;

=head1 DESCRIPTION

Retrieves an image of the specified PDB entry from the DB and serves
it up with the appropriate MIME type (image/gif). Relies on
L<Structure::begin|PfamWeb::Controller::Structure/"begin : Private">
to extract the PDB ID from the URL.

Generates an B<image file>, MIME type C<image/gif>.

$Id: GetImage.pm,v 1.7 2006-10-03 14:31:47 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Structure";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Path

Pick up http://localhost:3000/structure/getimage?id=1abc and serves
the image for that entry. Returns a blank image if no image is found
for this entry.

=cut

sub default : Path {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pdb};
}

#-------------------------------------------------------------------------------

=head2 end : Private

Push the file to the response

=cut

sub end : Private {
  my( $this, $c ) = @_;

  return unless defined $c->stash->{pdb};

  if( defined $c->stash->{pdb}->pdb_image ) {
	$c->res->content_type( "image/gif" );
	$c->res->write( $c->stash->{pdb}->pdb_image );
  } else {
	$c->res->redirect( $c->uri_for( "/images/blank.gif" ) );
  }

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
