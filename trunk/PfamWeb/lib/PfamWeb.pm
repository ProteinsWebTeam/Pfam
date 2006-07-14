
# PfamWeb.pm
# jt 20060316 WTSI
#
# This is the main class for the Pfam website catalyst
# application. Configuration is all done through the pfamweb.yml
# config file and there's (currently) not much else in here.
#
# $Id: PfamWeb.pm,v 1.8 2006-07-14 13:11:45 jt6 Exp $

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
                 Cache::FastMmap
				 Static::Simple /;

# add the following to enable session handling:
#                Session
#                Session::Store::FastMmap
#                Session::State::Cookie

use PfamConfig qw( pfamlib );

our $VERSION = '0.01';


#
# Configure the application
#

#__PACKAGE__->config( file => "../PfamConfig/pfamweb.yml" );

# use the default location for the config file, i.e. the root of the
# catalyst application

#
# Start the application
#
__PACKAGE__->setup;

=head1 NAME

PfamWeb - Catalyst based application

=head1 SYNOPSIS

    script/pfamweb_server.pl

=head1 DESCRIPTION

Catalyst based application.

=head1 METHODS

=cut


#
# Output a friendly welcome message
#
=head2 default

=cut

sub default : Private {
    my ( $self, $c ) = @_;

    # Hello World
    $c->response->body( $c->welcome_message );
}

#
# Uncomment and modify this end action after adding a View component
#
#=head2 end
#
#=cut
#
#sub end : Private {
#    my ( $self, $c ) = @_;
#
#    # Forward to View unless response body is already defined
#    $c->forward( $c->view('') ) unless $c->response->body;
#}

=head1 AUTHOR

John Tate,,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
