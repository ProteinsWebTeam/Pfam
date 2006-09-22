
# PfamWeb.pm
# jt 20060316 WTSI
#
# $Id: PfamWeb.pm,v 1.14 2006-09-22 10:41:34 jt6 Exp $

=head1 NAME

PfamWeb - main class for the Pfam website application

=cut

package PfamWeb;

=head1 DESCRIPTION

This is the main class for the Pfam website catalyst application. It
handles error reporting for the whole application and tab selection
for all the view.

$Id: PfamWeb.pm,v 1.14 2006-09-22 10:41:34 jt6 Exp $

=cut

use strict;
use warnings;

# Set flags and add plugins for the application
#
#         -Debug: activates the debug mode for very useful log messages
# Static::Simple: will serve static files from the application's root
# directory
#
use Catalyst qw/
				-Debug
				ConfigLoader
				Prototype
				Session
				Session::Store::FastMmap
				Session::State::Cookie
				Session::State::URI
				Cache::FastMmap
				Static::Simple
				/;

__PACKAGE__->config( file => "/nfs/team71/pfam/jt6/server/PfamWeb/conf/pfamweb.conf" );

# add PageCache as the last plugin to enable page caching. Careful
# though... doesn't work with Static::Simple

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

Configuration is done through two main files:

=over

=item C<pfamweb.yml>

a YAML format file that contains all of the catalyst-specific
settings

=item C<pfamweb.ini>

an INI format file that points C<Module::PortablePath> at associated
library modules in other filesystem trees

=back

These two files are explicitly excluded from the PfamWeb CVS module
because they will contain passwords which can't be made public along
with the rest of the code. Separating the configuration into a
separate CVS module (PfamConfig) means that the code CVS modules can
be published, if we need to do that.

=cut

__PACKAGE__->setup;

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 index : Private

Drops straight to the site index page.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "PfamWeb::index: generating site index" );

  # we really *shouldn't* have any content, but we'll check
  # anyway. Maybe we need to reset the response body... if that's even
  # possible.

}

#-------------------------------------------------------------------------------

=head2 default : Private

Catch unexpected redirects, etc. This action just drops straight to
the site index page but it records the redirect in the error database
before that.

=cut

sub default : Private {
  my( $this, $c ) = @_;

  # Hello World - the default catalyst welcome page
  #$c->response->body( $c->welcome_message );

  # record an error message, because we shouldn't error arrive here
  # unless via a broken link, missing page, etc.

  # first, figure out where the broken link was, internal or external
  my $where = ( $c->req->referer =~ /sanger/ ) ? "internal" : "external";

  # record the error
  $c->error("Found a broken $where link: \"" . $c->req->uri	. "\", referer: "
			. ( $c->req->referer eq "" ? "unknown" : "\"" . $c->req->referer . "\"" ) );

  # report it
  $c->forward( "/reportError" );

  # and clear the errors before we render the page
  $c->clear_errors;

}

#-------------------------------------------------------------------------------

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

#  $c->cache_page( expires  => "300",
#				  auto_uri => [ "/*" ],
#				  debug    => 1 );

  return 1;
}

#-------------------------------------------------------------------------------

=head2 end : Private

Renders the index page for the site, ignoring any errors that were
encountered up to this point.

=cut

sub end : Private {
  my( $this, $c ) = @_;

  unless( $c->response->body ) {
    $c->stash->{fullPage} = 1;
	$c->stash->{template} = "pages/index.tt";
	$c->forward( "PfamWeb::View::TT" );
  }

}

#-------------------------------------------------------------------------------

=head2 reportError : Private

Records site errors in the database. Because we could be getting
failures constantly, e.g. from SIMAP web service, we want to avoid
just mailing admins about that, so instead we insert an error message
into a database table.

The table has four columns:

=over 8

=item message

the raw message from the caller

=item num

the number of times this precise message has been seen

=item first

the timestamp for the first occurrence of the message

=item last

the timestamp for that most recent occurence of the
message. Automatically updated on insert or update

=back

An external script or plain SQL query should then be able to retrieve
error logs when required and we can keep track of errors without being
deluged with mail.

=cut

sub reportError : Private {
  my( $this, $c ) = @_;

  foreach my $e ( @{$c->error} ) {

	$c->log->debug( "PfamWeb::reportError: reporting a site error: |$e|" );

	my $rs = $c->model( "WebUser::ErrorLog" )->find( { message => $e } );

	if( $rs ) {
	  $rs->update( { num => $rs->num + 1 } );
	} else {
	  eval {
		$c->model( "WebUser::ErrorLog" )->create( { message => $e,
													num     => 1,
													first   => [ "CURRENT_TIMESTAMP" ] } );
	  };
	  if( $@ ) {
		# really bad; an error while reporting an error...
		$c->log->error( "PfamWeb::reportError: couldn't create a error log: $@" );
	  }
	}
	
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
