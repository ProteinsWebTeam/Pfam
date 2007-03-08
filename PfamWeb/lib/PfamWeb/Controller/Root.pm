
# Root.pm
# jt 20061003 WTSI
#
# $Id: Root.pm,v 1.9 2007-03-08 14:14:59 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Root - main class for the PfamWeb application

=cut

package PfamWeb::Controller::Root;

=head1 DESCRIPTION

This is the root class for the Pfam website catalyst application. It
installs global actions for the main site index page and other top-level
functions.

$Id: Root.pm,v 1.9 2007-03-08 14:14:59 jt6 Exp $

=cut

use strict;
use warnings;
use Data::Dump qw( dump );

use base "Catalyst::Controller";

# sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
__PACKAGE__->config->{namespace} = '';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Private

Generates the main site index page. Also traps 404s and redirects to a 404 page.

=cut

sub default : Private {
  my ( $this, $c ) = @_;

  # before we go too far, make sure this isn't a broken URL
  $c->res->redirect( $c->uri_for("/404") )
    unless $c->req->uri eq $c->req->base;

  # now, knowing that this is a request for the site index page...

  # set the page to be cached for an hour
  $c->cache_page( 3600 );
  
  # retrieve the news items from the feed
  my @entries =
    $c->model("WebUser::News")->search( {}, { order_by => "pubDate DESC" } );
  $c->stash->{newsItems} = \@entries;

  # get the top-ten families
  my @topTen =
    $c->model("WebUser::Family_count")
    ->search( {}, { order_by => "view_count DESC" } );
  $c->stash->{topTen} = \@topTen;

  $c->log->debug("PfamWeb::default: generating site index");
}

#-------------------------------------------------------------------------------

=head2 auto : Private

Checks the request parameters for a "tab" parameter and sets the
appropriate tab name in the stash. That will be picked up by the
tab_layout.tt view, which will use it to figure out which tab to show
by default.

Also adds the Pfam version data to the stash, so that it's generally 
accessible throughout the app.

=cut

sub auto : Private {
  my ( $this, $c ) = @_;

  #----------------------------------------
  # authentication/authorisation stuff...

  #  return 1 if $c->controller eq $c->controller("Auth");
  #
  #  unless( $c->user_exists ) {
  #    $c->log->debug( "Root::auto: unknown user; redirecting to login" );
  #    $c->res->redirect( $c->uri_for( "/login" ) );
  #    return 0;
  #  }
  #  $c->log->debug( "Root::auto: user authenticated" );

  #----------------------------------------
  # tack on a trailing slash, if required

#  if ( $c->req->path and $c->req->path !~ /\/$/ and not $c->req->param ) {
#    $c->res->redirect( $c->req->base . $c->req->path . "/", 301 );
#    return 0;
#  }

  #----------------------------------------
  # pick a tab

  my $tab;
  ($tab) = $c->req->param("tab") =~ /^(\w+)$/
    if defined $c->req->param("tab");

  $c->stash->{showTab} = $1 if defined $tab;

  # stash some details of the Pfam release
  my $releaseData = $c->model("PfamDB::Version")->find( {} );
  $c->stash->{relData} = $releaseData if $releaseData;

  return 1;
}

#-------------------------------------------------------------------------------

=head2 graph : Local

Generates a graph showing Pfam coverage.

=cut

#sub graph : Local {
#  my ( $this, $c ) = @_;
#
#  $c->res->header( "Content-Type" => "image/png" );
#  $c->stash->{template} = "components/graph.tt";
#}

#-------------------------------------------------------------------------------

=head2 about : Local

Show an "about Pfam" page.

=cut

sub about : Local {
  my ( $this, $c ) = @_;

  # set the page to be cached for two weeks
  $c->cache_page( 1209600 );

  $c->stash->{template} = "components/tools/about.tt";
}

#-------------------------------------------------------------------------------

=head2 annotate : Local

Show an annotation upload form.

=cut

sub annotate : Local {
  my ( $this, $c ) = @_;

  $c->stash->{template} = "pages/annotation.tt";
}

#-------------------------------------------------------------------------------

=head2 fourOhFour : Path( "/404" )

Generates a "404" page.

=cut

sub fourOhFour : Path("/404") {
  my ( $this, $c ) = @_;

  # set the page to be cached for two weeks
  $c->cache_page( 1209600 );

  #----------------------------------------
  # first, figure out where the broken link was, internal or external

  my $ref = ( defined $c->req->referer ) ? $c->req->referer : "";
  $c->stash->{where} = ( $ref =~ /sanger/ ) ? "internal" : "external";

  # record the error
  $c->error(   "Found a broken "
             . $c->stash->{where}
             . " link: \""
             . $c->req->uri
             . "\", referer: "
             . ( $ref eq "" ? "unknown" : "\"" . $ref . "\"" ) );

  # report it
  $c->forward("/reportError");

  # and clear the errors before we render the page
  $c->clear_errors;

  #----------------------------------------
  # set the HTTP status and point at the 404 page

  $c->res->status(404);
  $c->stash->{template} = "pages/404.tt";
}

#-------------------------------------------------------------------------------

=head2 end : Private

Renders the index page for the site by default, but the default template can be 
overridden by setting it in an action (eg for a 404 template).

=cut

sub end : ActionClass("RenderView") {
  my ( $this, $c ) = @_;
  $c->stash->{template} ||= "pages/index.tt";
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
