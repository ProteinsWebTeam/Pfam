
# WikiApp.pm
# jt6 20100422 WTSI
#
# $Id$

=head1 NAME

WikiApp - wikipedia approval application

=cut 

package WikiApp;

=head1 SYNOPSIS

    script/wikiapp_server.pl

=head1 DESCRIPTION

A web application for approving edits for Pfam- and Rfam-related wikipedia 
articles.

=cut

use Moose;
use namespace::autoclean;
use MRO::Compat;

use Catalyst::Runtime 5.80;

#-------------------------------------------------------------------------------

# Set flags and add plugins for the application
#
#         -Debug: activates the debug mode for very useful log messages
#   ConfigLoader: will load the configuration from a Config::General file in the
#                 application's home directory
# Static::Simple: will serve static files from the application's root
#                 directory

use Catalyst qw/
  ConfigLoader
  Static::Simple
	Unicode

  Authentication
  Session
  Session::State::Cookie
  Session::Store::FastMmap
/;

extends 'Catalyst';

our $VERSION = '0.01';
$VERSION = eval $VERSION;

# load configuration from a specified file
if ( $ENV{WIKIAPP_CONFIG} and
     -e $ENV{WIKIAPP_CONFIG} ) {
  __PACKAGE__->config( 'Plugin::ConfigLoader' => { file => $ENV{WIKIAPP_CONFIG} } );
}

# Start the application
__PACKAGE__->setup();

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 dump_these

When debug mode is enabled, errors will generate the catalyst error page. That
normally includes a dump of the configuration of the application, which may
contain passwords and other sensitive information. This method filters the 
"config" section out of the error page entirely.

=cut

sub dump_these {
  my $c = shift;
  my @variables = $c->next::method(@_);
  return grep { $_->[0] ne 'Config' } @variables;
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>
Paul Gardner, C<pg5@sanger.ac.uk>
Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
         Paul Gardner (pg5@sanger.ac.uk)

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

