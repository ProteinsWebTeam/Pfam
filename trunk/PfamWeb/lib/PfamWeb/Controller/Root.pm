
# Root.pm
# jt 20061003 WTSI
#
# $Id: Root.pm,v 1.30 2010-01-13 14:44:53 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Root - main class for the PfamWeb application

=cut

package PfamWeb::Controller::Root;

=head1 DESCRIPTION

This is the root class for the Pfam website catalyst application. It
installs global actions for the main site index page and other top-level
functions.

$Id: Root.pm,v 1.30 2010-01-13 14:44:53 jt6 Exp $

=cut

use utf8;
use strict;
use warnings;

use base 'PfamBase::Controller::Root';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Adds the Pfam version data to the stash, so that it's accessible
throughout the site.

=cut

sub auto : Private {
  my $this = shift;
  my ( $c ) = @_;

  # see if there's a maintenance message in the configuration file
  if ( $c->config->{maintenance} ) {
    $c->log->debug( 'Root::auto: found a maintenance message' )
      if $c->debug;

    $c->stash->{title}   = $c->config->{maintenance}->{title};
    $c->stash->{message} = $c->config->{maintenance}->{message};

    $c->stash->{template} = 'pages/maintenance.tt';

    # break out of the processing chain now and go straight to the "end" action
    return 0;
  }

  # see if we can get a DB ResultSet from the VERSION table, which is
  # effectively a test of whether we can connect to the DB. If we can't,
  # set the template to point to a page that will apologise and let the "end"
  # actin do its stuff
  eval {
    # stash some details of the Pfam release
    $c->stash->{relData} = $c->model( 'PfamDB::Version' )
                             ->search( {}, {} )
                             ->first;
  };
  if ( $@ ) {
    $c->stash->{template} = 'pages/db_down.tt';

    # break out of the processing chain now and go straight to the "end" action
    return 0;
  }

  # call the "auto" method on the super-class, which will do the work of
  # picking the correct tab for us
  return $c->SUPER::auto( @_ );
}

#-------------------------------------------------------------------------------

=head2 index : Private

Generates the main site index page. This overrides the version from
PfamBase::Root, so that we can use cache_page for PfamWeb.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # # set the page to be cached for one week
  $c->cache_page( 604800 );

  # tell the navbar where we are
  $c->stash->{nav} = 'home';

  $c->log->debug('PfamWeb::index: generating site index') if $c->debug;
}

#-------------------------------------------------------------------------------

=head2 blocked : Path

Displays a message telling the user that their access has been blocked for
potentially abusive usage.

=cut

sub blocked : Global {
  my( $this, $c ) = @_;

  $c->log->debug( 'Root::blocked: sending "blocked" message for IP: '
                  . $c->req->address ) if $c->debug;

  my $ip = $c->req->address;

  $c->res->status( 403 ); # Forbidden
  $c->res->content_type( 'text/plain' );
  $c->res->body( <<EOF );

REQUEST BLOCKED

We have detected excessive or potentially abusive usage of the Pfam website
from your IP address ($ip). Access to this section of the site is
currently blocked for all users of that address.

Please return to the home page and contact the Pfam helpdesk as soon as
possible, using the email address at the bottom of the page.

EOF

}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

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

1;
