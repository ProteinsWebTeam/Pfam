
# GetImage.pm
# jt6 20060314 WTSI
#
# $Id: GetImage.pm,v 1.1 2009-12-11 14:45:46 pg6 Exp $

=head1 NAME

iPfamWeb::Controller::Structure::GetImage - serve an image of the
structure

=cut

package iPfamWeb::Controller::Structure::GetImage;

=head1 DESCRIPTION

Retrieves an image of the specified PDB entry from the DB and serves
it up with the appropriate MIME type (image/gif). Relies on
L<Structure::begin|iPfamWeb::Controller::Structure/"begin : Private">
to extract the PDB ID from the URL.

Generates an B<image file>, MIME type C<image/gif>.

$Id: GetImage.pm,v 1.1 2009-12-11 14:45:46 pg6 Exp $

=cut

use strict;
use warnings;

use base 'iPfamWeb::Controller::Structure';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 get_image : Path

Serves the image for the specified PDB entryentry. Redirects to a blank image 
if no image is found for this entry.

=cut

sub get_image : Path {
  my ( $this, $c ) = @_;

  unless ( defined $c->stash->{pdb} ) {
    $c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
    return;
  }

  if ( $c->stash->{pdb}->pdb_image->pdb_image_sml ) {
  	$c->res->content_type( 'image/gif' );
  	if ( defined $c->req->param('size') and
  	     $c->req->param('size') eq 's' ) {
  	  $c->res->body( $c->stash->{pdb}->pdb_image->pdb_image_sml );
  	}
  	else {
  	  $c->res->body( $c->stash->{pdb}->pdb_image->pdb_image );
  	}
  }
  else {
    # TODO we shouldn't be hard-coding the location for blank images...
  	$c->res->redirect( $c->uri_for( '/shared/images/blank.gif' ) );
  }
}

#-------------------------------------------------------------------------------

=head2 end : ActionClass

Override "end" from the super-class and let RenderView take care of things.

=cut

#sub end : ActionClass( 'RenderView' ) {}

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
