
# Root.pm
# jt6 20100422 WTSI
#
# $Id$

=head1 NAME

WikiApp::Controller::Root - top-level controller for the application

=cut 

package WikiApp::Controller::Root;

=head1 DESCRIPTION

This is the main (root) controller for the WikiApp application. 

=cut

use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller' }

#-------------------------------------------------------------------------------

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config(namespace => '');

#-------------------------------------------------------------------------------

=head1 ACTIONS

=head2 index

The root page (/)

=cut

sub index :Path :Args(0) {
  my ( $this, $c ) = @_;

  # copy the list of approved types into the stash, so that we can use it to
  # build the list of article types in the index page
  $c->stash->{handled_article_types} = 
    $c->forward( $c->controller('Articles')->action_for('type_list') );

  $c->stash->{template} = 'index.tt';
}

#-------------------------------------------------------------------------------

=head2 default

Standard 404 error page

=cut

sub default :Path {
  my ( $this, $c ) = @_;
  $c->res->body( 'Page not found' );
  $c->res->status(404);
}

#-------------------------------------------------------------------------------

=head2 end

Attempt to render a view, if needed.

=cut

sub end : ActionClass('RenderView') {}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>
Paul Gardner, C<pg5@sanger.ac.uk>
Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
         Paul Gardner (pg5@sanger.ac.uk)

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

