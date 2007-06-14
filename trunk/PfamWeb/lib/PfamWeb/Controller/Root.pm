
# Root.pm
# jt 20061003 WTSI
#
# $Id: Root.pm,v 1.14 2007-06-14 21:35:00 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Root - main class for the PfamWeb application

=cut

package PfamWeb::Controller::Root;

=head1 DESCRIPTION

This is the root class for the Pfam website catalyst application. It
installs global actions for the main site index page and other top-level
functions.

$Id: Root.pm,v 1.14 2007-06-14 21:35:00 jt6 Exp $

=cut

use strict;
use warnings;

use base 'Catalyst::Controller';

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
  $c->res->redirect( $c->uri_for('/404') )
    unless $c->req->uri eq $c->req->base;
  
  # tell the navbar where we are
  $c->stash->{nav} = 'home';

  $c->log->debug('PfamWeb::default: generating site index');
}

#-------------------------------------------------------------------------------

=head2 auto : Private

Checks the request parameters for a "tab" parameter and sets the
appropriate tab name in the stash. That will be picked up by the
tab_layout.tt view, which will use it to figure out which tab to show
by default.

Also adds the Pfam version data to the stash, so that it's accessible 
throughout the site.

Checks for the existence of various files that act as switches, enabling
different behaviours...
=cut

sub auto : Private {
  my ( $this, $c ) = @_;

#  # check for the existence of the various files that act as switches for
#  # various server behaviours
#  if( -e $this->{BREAK_SWITCH} ) {
#    $c->log->warn( 'Root::auto: found the break file; redirecting' );
#    $c->stash->{template} = $this->{BREAK_TEMPLATE}; 
#    return 0;
#  }
#
#  # add a service message to the index page
#  if( -e $this->{ANNOUNCE_SWITCH} ) {
#    $c->log->warn( 'Root::auto: found the announcement switch; adding announcement to index page' );
#    $c->stash->{serverAnnouncement} = $this->{ANNOUNCE_TEMPLATE};
#  }
#
#  # add a message to the navbar
#  if( -e $this->{MESSAGE_SWITCH} ) {
#    $c->log->warn( 'Root::auto: found the message file; adding message to header' );
#    $c->stash->{serverMessage} = $this->{MESSAGE_TEMPLATE};
#  }

  # pick a tab
  my $tab;
  ($tab) = $c->req->param('tab') =~ /^(\w+)$/
    if defined $c->req->param('tab');

  $c->stash->{showTab} = $1 if defined $tab;

  # see if we can get a DB ResultSet, which is effectively a test of whether we
  # can connect to the DB. If we can't, thrown an error and let the end action
  # clean up
  my $releaseData;
  eval {
    # stash some details of the Pfam release
    $releaseData = $c->model( 'PfamDB::Version' )->find( {} );
  };
  if( $@ ) {
    $c->error( 'There appears to be a problem with the Pfam database.' );
    $c->forward('/reportError');
  }
  
  $c->stash->{relData} = $releaseData if $releaseData;
  
  return 1;
}

#-------------------------------------------------------------------------------

=head2 browseIndex : Global

Show an index page for the various "browse" pages.

=cut

sub browseIndex : Global {
  my ( $this, $c ) = @_;

  # tell the navbar where we are
  $c->stash->{nav} = 'browse';

  $c->stash->{template} = 'pages/browseIndex.tt';
}

#-------------------------------------------------------------------------------

=head2 fourOhFour : Path( '/404' )

Generates a '404' page.

=cut

sub fourOhFour : Path('/404') {
  my ( $this, $c ) = @_;

  # first, figure out where the broken link was, internal or external

  my $ref = ( defined $c->req->referer ) ? $c->req->referer : '';
  $c->stash->{where} = ( $ref =~ /sanger/ ) ? 'internal' : 'external';

  # record the error
  $c->error(   'Found a broken '
             . $c->stash->{where}
             . ' link: \''
             . $c->req->uri
             . '\', referer: '
             . ( $ref eq '' ? 'unknown' : '\'' . $ref . '\'' ) );

  # report it
  $c->forward('/reportError');

  # and clear the errors before we render the page
  $c->clear_errors;

  #----------------------------------------
  # set the HTTP status and point at the 404 page

  $c->res->status(404);
  $c->stash->{template} = 'pages/404.tt';
}

#-------------------------------------------------------------------------------

=head2 end : Private

Renders the index page for the site by default, but the default template can be 
overridden by setting it in an action (eg for a 404 template). Patterned on
the "end" method from the DefaultEnd plugin.

=cut

sub end : Private {
  my( $this, $c ) = @_;
  
  # were there any errors ? If so, render the error page into the response
  if( scalar @{ $c->error } ) {
    $c->log->debug( 'Root::end: found some errors from previous methods' );
    $c->stash->{errorMsg} = $c->error;
    $c->stash->{template} = 'pages/error.tt';
    
    # make sure the error page isn't cached
    $c->res->header( 'Pragma' => 'no-cache' );
    $c->res->header( 'Expires' => 'Thu, 01 Jan 1970 00:00:00 GMT' );
    $c->res->header( 'Cache-Control' => 'no-store, no-cache, must-revalidate,'.
                                        'post-check=0, pre-check=0, max-age=0' );
    
    $c->forward( 'PfamWeb::View::TT' );
    $c->clear_errors;
  }

  # don't render anything else if the response already has content
  return 1 if $c->response->body;
  return 1 if $c->response->status =~ /^3\d\d$/;

  # set the content type to the default of "text/html", unless it's already set
  $c->response->content_type( 'text/html; charset=utf-8' )
    unless $c->response->content_type;

  # finally, default to the index page template and we're done
  $c->stash->{template} ||= 'pages/index.tt';
  $c->forward( 'PfamWeb::View::TT' );
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
