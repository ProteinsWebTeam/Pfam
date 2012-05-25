
# Root.pm
# jt 20080306 WTSI
#
# $Id: Root.pm,v 1.3 2008-07-25 13:25:40 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Root - main class for the RfamWeb application

=cut

package RfamWeb::Controller::Root;

=head1 DESCRIPTION

This is the root class for the Rfam website catalyst application. It
installs global actions for the main site index page and other top-level
functions.

$Id: Root.pm,v 1.3 2008-07-25 13:25:40 jt6 Exp $

=cut

use Moose;
use namespace::autoclean;

BEGIN {
  extends 'Catalyst::Controller';
}

with 'PfamBase::Roles::Root';

__PACKAGE__->config( namespace => '' );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Adds the version data to the stash, so that it's accessible throughout the
site. Runs before the "auto" from the Root role from PfamBase.

=cut

before 'auto' => sub {
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
  # action do its stuff
  my $releaseData;
  eval {
    # stash some details of the Pfam release
    $releaseData = $c->model( 'RfamDB::Version' )
                     ->find( {} );
  };
  if ( $@ ) {
    $c->log->error( "DBIC error on database check: $@" ) if $c->debug;
    $c->stash->{template} = 'pages/db_down.tt';

    # break out of the processing chain now and go straight to the "end" action
    return 0;
  }
  
  $c->stash->{relData} = $releaseData if $releaseData;

  # runs before the "auto" action from the role, which will return a true value
  # so that we don't need to do that here
};

#-------------------------------------------------------------------------------

=head2 index : Private

Generates the main site index page.

=cut

sub index : Private {
  my( $this, $c ) = @_;

  # cache the page output for one week
  $c->cache_page( 604800 );
  
  # tell the navbar where we are
  $c->stash->{nav} = 'home';

  $c->log->debug('RfamWeb::Root::index: generating site index') if $c->debug;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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
