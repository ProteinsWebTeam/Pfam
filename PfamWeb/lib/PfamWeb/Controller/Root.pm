
# Root.pm
# jt 20061003 WTSI
#
# $Id: Root.pm,v 1.3 2006-10-23 12:19:21 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Root - main class for the PfamWeb application

=cut

package PfamWeb::Controller::Root;

=head1 DESCRIPTION

This is the root class for the Pfam website catalyst application. It
installs global actions for the main site index page, a catch for
errors from within the site, an C<auto> action that handles tab
selection for the whole site, and a default C<end> that renders the
index page.

$Id: Root.pm,v 1.3 2006-10-23 12:19:21 jt6 Exp $

=cut

use strict;
use warnings;

use base "Catalyst::Controller";

# sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config->{namespace} = '';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 index : Private

Drops straight to the site index page.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  $c->log->debug( "PfamWeb::index: generating site index" );

}

#-------------------------------------------------------------------------------

=head2 default : Private

Catch unexpected redirects, etc. This action just drops straight to
the site index page but it records the redirect in the error database
before that.

=cut

sub default : Private {
  my( $this, $c ) = @_;

  # record an error message, because we shouldn't error arrive here
  # unless via a broken link, missing page, etc.

  # first, figure out where the broken link was, internal or external
  my $ref = ( defined $c->req->referer ) ? $c->req->referer : "";
  my $where = ( $ref =~ /sanger/ ) ? "internal" : "external";

  # record the error
  $c->error("Found a broken $where link: \"" . $c->req->uri	. "\", referer: "
			. ( $ref eq "" ? "unknown" : "\"" . $ref . "\"" ) );

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

=head2 about : Global

Show an "about Pfam" page.

=cut

sub about : Global {
  my( $this, $c ) = @_;

  $c->stash->{template} = "pages/about.tt";
}

#-------------------------------------------------------------------------------

=head2 annotate : Global

Show an annotation upload form.

=cut

sub annotate : Global {
  my( $this, $c ) = @_;

  $c->stash->{template} = "pages/annotation.tt";
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
	$c->stash->{template} ||= "pages/index.tt";
	$c->forward( "PfamWeb::View::TT" );
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
