
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

use strict;
use warnings;

use Sys::Hostname;
use Config::General;

use base 'PfamBase';

our $VERSION = '1.7';

#-------------------------------------------------------------------------------

=head1 CONFIGURATION

Configuration is done through a series of external "Apache-style"
configuration files.

=cut

# add to the configuration the name of the host that's actually serving
# the site and its process ID. These will be pulled out later in the header
# template
__PACKAGE__->config->{server_name} = hostname();
__PACKAGE__->config->{server_pid}  = $$;

# grab the location of the configuration file from the environment and
# detaint it. Doing this means we can configure the location of the
# config file in httpd.conf rather than in the code
my( $conf ) = $ENV{PFAMWEB_CONFIG} =~ m/([\d\w\/-]+)/;

# set up the ConfigLoader plugin. Point to the configuration file
__PACKAGE__->config->{'Plugin::ConfigLoader'}->{file} = $conf;

# read the configuration, configure the application and load these
# catalyst plugins
__PACKAGE__->setup( qw(
                        HTML::Widget
                        PageCache
                        GzipCompressor
                      ) );

                        # the session plugins were used by the protein features
                        # viewer, but, since it's disabled...
                        # Session
                        # Session::Store::FastMmap
                        # Session::State::Cookie

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 is_cache_enabled

Returns true if the configuration parameter C<enable_cache> is defined and is
set to a true value. Used by the PageCache plugin to decide if it should step
in to cache a page/serve a page from cache.

If C<enable_cache> is true, page caching will be enabled.

=cut

sub is_cache_enabled {
  my ( $c ) = @_;
  return ( exists $c->config->{enable_cache} and $c->config->{enable_cache} );
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

