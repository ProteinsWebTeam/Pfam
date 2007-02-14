
# Auth.pm
# jt6 20061207 WTSI
#
# $Id: Auth.pm,v 1.1 2007-02-14 10:52:07 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Auth - authentication for PfamWeb

=cut

package PfamWeb::Controller::Auth;

=head1 DESCRIPTION

$Id: Auth.pm,v 1.1 2007-02-14 10:52:07 jt6 Exp $

=cut

use strict;
use warnings;

use Data::Dump qw( dump );

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 login : Global

Authenticates a user against the password database.

=cut

sub login : Global {
  my( $this, $c ) = @_;

	my $u = $c->req->param( "username" ) || "";
	my $p = $c->req->param( "password" ) || "";

	if( $u and $p ) {

		if( $c->login( $u, $p ) ) {
			$c->log->debug( "Auth::login: user authenticated: " . $c->user->name );
		  $c->res->redirect( $c->uri_for( "/" ) );
			return;
		} else {
			$c->stash->{error} = "Bad username or password";
		}
	}
	$c->stash->{template} = "pages/login.tt";
}

#-------------------------------------------------------------------------------

sub logout : Global {
  my( $this, $c ) = @_;

	$c->logout;
	$c->res->redirect( $c->uri_for( "/login" ) );

}

#-------------------------------------------------------------------------------

sub end : ActionClass('RenderView') { }

#-------------------------------------------------------------------------------

1;

