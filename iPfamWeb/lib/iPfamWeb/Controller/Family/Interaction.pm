
# Interaction.pm
# pg6 20091118 WTSI
#
# $Id: Interaction.pm,v 1.2 2010-01-04 10:07:34 pg6 Exp $

=head1 Name

iPfamWeb::Controller::Family::Interaction

Controller to get the image of the domain and its interaction.

=cut

package iPfamWeb::Controller::Family::Interaction;

use strict;
use warnings;

use base 'iPfamWeb::Controller::Family';

=head1 Methods

=head2 getImage : Path: ActionClass( 'REST' )

Base action for all restful call, to retrieve the image.

code is repeated for both GET and POST requests.

Retrieves the image using the graph viewer.

=cut

sub getImage: Path( '/interaction/getImage') : ActionClass( 'REST' ){}

sub getImage_GET{
  my ( $self, $c ) = @_;
  $c->log->debug( "Family::Interaction::getImage Inside the action corresponding to GET method");
  $c->stash->{ template } = 'components/blocks/family/interactionImages.tt';
  
}

sub getImage_POST{
  my ( $self, $c ) = @_;
  $c->log->debug( "Family::Interaction::getImage Inside the action corresponding to POST method");
  $c->stash->{ template } = 'components/blocks/family/interactionImages.tt';
  
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
