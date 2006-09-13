
# PfamWeb.pm
# jt 20060316 WTSI
#
# $Id: PfamWeb.pm,v 1.12 2006-09-13 08:31:54 jt6 Exp $

=head1 NAME

PfamWeb - main class for the Pfam website application

=cut

package PfamWeb;

=head1 DESCRIPTION

This is the main class for the Pfam website catalyst
application. Configuration is all done through the C<pfamweb.yml>
config file.

$Id: PfamWeb.pm,v 1.12 2006-09-13 08:31:54 jt6 Exp $

=cut

use strict;
use warnings;

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

# configure library loading. This module reads the pfamweb.ini file in
# the root of the application and gets the path to the specified
# "library module," which it then adds to the include path

use PfamConfig qw( pfamlib );

our $VERSION = '0.01';

#-------------------------------------------------------------------------------

=head1 CONFIGURATION

Configuration is done through a single YAML file, C<pfamweb.yml>,
which contains all of the catalyst-specific settings, and an INI file,
C<pfamweb.ini>, which points C<Module::PortablePath> at associated
library modules in other filesystem trees.

=cut

__PACKAGE__->setup;

#-------------------------------------------------------------------------------

=head2 default : Private

Re-directs straight to the index page for the whole site. This is the
default page for any errors, broken (internal) links, etc.

=cut

sub default : Private {
  my( $this, $c ) = @_;

  # Hello World - the default catalyst welcome page
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

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Checks the request parameters for a "tab" parameter and sets the
appropriate tab name in the stash. That will be picked up by the
tab_layout.tt view, which will use it to figure out which tab to show
by default.

=cut

sub auto : Private {
  my( $this, $c ) = @_;

  my $tab;
  ( $tab ) = $c->req->param( "tab" ) =~ /^(\w+)$/
	if defined $c->req->param( "tab" );

  $c->stash->{showTab} = $1 if defined $tab;

  return 1;
}

=head2 default : Private

A catch-all method that displays the main site index page.

=cut

#-------------------------------------------------------------------------------

=head2 reportError : Private

Records site errors in the database.

=cut

sub reportError : Private {
  my( $this, $c ) = @_;

  foreach my $e ( @{$c->error} ) {

	$c->log->debug( "PfamWeb::reportError: reporting a site error: |$e|" );

	my $rs = $c->model( "WebUser::ErrorLog" )->find( { message => $e } );

	if( $rs ) {
	  $c->log->debug( "PfamWeb::reportError: seen that error before; update" );
	  $rs->update( { num => $rs->num + 1 } );
	} else {
	  $c->log->debug( "PfamWeb::reportError: new error message; creating" );
	  $rs->create( { message => $e,
					 num     => 1,
					 first   => [ "CURRENT_TIMESTAMP" ] } );
	}
	
  }

  $c->log->debug( "PfamWeb::reportError: there are now "
				  . $c->model( "WebUser::ErrorLog" )->count . " errors in the table" );

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
