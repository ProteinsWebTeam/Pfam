
# RfamWeb.pm
# jt 20080306 WTSI
#
# $Id: RfamWeb.pm,v 1.1.1.1 2008-03-17 16:37:43 jt6 Exp $

=head1 NAME

RfamWeb - application class for the Rfam website

=cut

package RfamWeb;

=head1 DESCRIPTION

This is the main class for the Rfam website catalyst application.

$Id: RfamWeb.pm,v 1.1.1.1 2008-03-17 16:37:43 jt6 Exp $

=cut

use strict;
use warnings;

use Sys::Hostname;
use Config::General;

use base 'PfamBase';

our $VERSION = '0.1';

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
my( $conf ) = $ENV{RFAMWEB_CONFIG} =~ m/([\d\w\/-]+)/;

# set up the ConfigLoader plugin. Point to the configuration file
__PACKAGE__->config->{'Plugin::ConfigLoader'}->{file} = $conf;

# read the configuration, configure the application and load these 
# catalyst plugins
__PACKAGE__->setup( qw() ); # no extra plugins required yet

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk), 
         Jennifer Daub (jd7@sanger.ac.uk)

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
