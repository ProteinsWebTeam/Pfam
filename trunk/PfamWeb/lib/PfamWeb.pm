
# PfamWeb.pm
# jt 20060316 WTSI
#
# This is the main class for the Pfam website catalyst
# application. Configuration is all done through the pfamweb.yml
# config file and there's (currently) not much else in here.
#
# $Id: PfamWeb.pm,v 1.11 2006-08-14 10:23:50 jt6 Exp $

package PfamWeb;

use strict;
use warnings;

#
# Set flags and add plugins for the application
#
#         -Debug: activates the debug mode for very useful log messages
# Static::Simple: will serve static files from the application's root
# directory
#
use Catalyst qw/ -Debug
                 ConfigLoader
                 Prototype
                 Session
                 Session::Store::FastMmap
                 Session::State::Cookie
                 Cache::FastMmap
				 Static::Simple /;

# add the following to enable session handling:
#                Session
#                Session::Store::FastMmap
#                Session::State::Cookie

use PfamConfig qw( pfamlib );

our $VERSION = '0.01';

# configure the application
__PACKAGE__->setup;


sub auto : Private {
  my( $self, $c ) = @_;

  my $tab;
  ( $tab ) = $c->req->param( "tab" ) =~ /^(\w+)$/
	if defined $c->req->param( "tab" );

  $c->stash->{showTab} = $1 if defined $tab;

  return 1;
}


# catch-all method that displays the main site index page

sub default : Private {
  my( $self, $c ) = @_;

  # Hello World
  #$c->response->body( $c->welcome_message );

  # catch the root of the application. This could be moved into a
  # lower level controller sometime, with a Global action that
  # captures "/"...
  unless( $c->response->body ) {
    $c->stash->{fullPage} = 1;
	$c->stash->{template} = "pages/index.tt";
	$c->forward( "PfamWeb::View::TT" );
  }
}



1;
