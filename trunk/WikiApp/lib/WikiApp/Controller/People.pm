
# People.pm
# jt6 20100422 WTSI
#
# $Id$

=head1 NAME

WikiApp::Controller::People - handles authentication/authorisation for the app

=cut

package WikiApp::Controller::People;

=head1 DESCRIPTION

This controller handles the authentication and authorisation of users for
the whole application. 

=cut

use Moose;
use namespace::autoclean;
use Data::Dump qw(dump);

BEGIN {extends 'Catalyst::Controller'; }

#-------------------------------------------------------------------------------

=head1 ACTIONS

=head2 login : Global

Authenticates the user and logs them in.

=cut

sub login : Global : Args(0) {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'people/login.tt';

  return unless exists $c->req->params->{username};
    
  if ( $c->authenticate( { username => $c->req->params->{username},
                           password => $c->req->params->{password} } ) ) {
    $c->stash->{message} = 'You are now logged in.';

    # $c->log->debug( 'People::login: user supports roles: |'
    #                 . dump( $c->user->supports( qw( roles ) ) ) . '|' )
    #   if $c->debug;

    # $c->log->debug( 'People::login: roles: |'
    #                 . dump( $c->user->roles ) . '|' )
    #   if $c->debug;

    # $c->check_user_roles( 501 );
    # $c->log->debug( "|", dump( $c->user->roles ), "|" );

    $c->res->redirect( $c->uri_for( '/' ) );
    $c->detach();
    return;
  }
  else {
    $c->stash->{message} = 'Invalid login.';
  }

}

#-------------------------------------------------------------------------------

=head2 logout : Global

Logs the user out of the application.

=cut

sub logout : Global : Args(0) {
  my ( $this, $c ) = @_;

  $c->stash->{template} = 'people/logout.tt';
  $c->stash->{message} = 'You have been logged out.';
  $c->logout;
}

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
