
# PfamWeb.pm
# jt 20060316 WTSI
#
# $Id: PfamWeb.pm,v 1.56 2010-01-19 09:45:09 jt6 Exp $

=head1 NAME

PfamWeb - application class for the Pfam website

=cut

package PfamWeb;

=head1 DESCRIPTION

This is the main class for the Pfam website catalyst application.

$Id: PfamWeb.pm,v 1.56 2010-01-19 09:45:09 jt6 Exp $

=cut

use utf8;
use Moose;
use namespace::autoclean;

use Catalyst::Runtime 5.80;
use Sys::Hostname;
use Config::General;
use Log::Log4perl::Catalyst;

extends 'PfamBase';

our $VERSION = '1.7';

#-------------------------------------------------------------------------------

=head1 CONFIGURATION

Configuration is done through a series of external "Apache-style"
configuration files.

=cut

# add to the configuration the name of the host that's actually serving
# the site and its process ID. These will be pulled out later in the header
# template
__PACKAGE__->config( server_name => hostname() );
__PACKAGE__->config( server_pid  => $$ );
__PACKAGE__->config( using_frontend_proxy => 1); #This is supposed to enable https

# grab the location of the configuration file from the environment and
# detaint it. Doing this means we can configure the location of the
# config file in httpd.conf rather than in the code
my ( $conf ) = $ENV{PFAMWEB_CONFIG} =~ m/([\d\w\/-]+)/;

# set up the ConfigLoader plugin. Point to the configuration file
__PACKAGE__->config( 'Plugin::ConfigLoader' => { file => $conf } );

# read the configuration, configure the application and load these
# catalyst plugins
__PACKAGE__->setup( qw( HTML::Widget
                        Static::Simple) );

# use Log4perl for logging to a separate log file. We need to set up the
# l4p output using a config file, which we specify in the main server
# config.
__PACKAGE__->log( Log::Log4perl::Catalyst->new( __PACKAGE__->config->{l4p_config} ) );

sub secure_uri_for {
  my ($c, @args) = @_;
  my $uri = $c->uri_for(@args);
  return $c->relativised_uri($uri);
}

sub relativised_uri {
  my ($c, $uri) = @_;
  my $base = $c->req->base;
  my $relative_uri = $uri;
  #$c->log->warn("PRE URL $relative_uri");
  $relative_uri =~ s|$base|/|g;
  #$c->log->warn("FIXED URL $relative_uri");
  return $relative_uri
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
