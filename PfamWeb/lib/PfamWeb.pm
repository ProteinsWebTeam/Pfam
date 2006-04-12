
# PfamWeb.pm
# jt 20060316 WTSI
#
# This is the main class for the Pfam website catalyst
# application. Configuration is all done through the pfamweb.yml
# config file and there's (currently) not much else in here.
#
# $Id: PfamWeb.pm,v 1.3 2006-04-12 16:29:44 jt6 Exp $

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
				 Static::Simple
				 Session
				 Session::Store::FastMmap
				 Session::State::Cookie /;

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



# get the row in the Pfam table for this entry

sub begin : Private {
  my( $this, $c ) = @_;

  if( defined $c->req->param("acc") ) {

	$c->req->param("acc") =~ m/^(PF\d{5})$/;
	$c->log->info( "$this: found accession |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_acc => $1 } )
	  if defined $1;

  } elsif( defined $c->req->param("id") ) {

	$c->req->param("id") =~ m/(^\w+$)/;
	$c->log->info( "$this: found ID |$1|" );

	$c->stash->{pfam} = PfamWeb::Model::Pfam->find( { pfamA_id => $1 } )
	  if defined $1;

  }	

  $c->log->warn( "$this: no ID or accession" )
	unless defined $c->stash->{pfam};

}



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
