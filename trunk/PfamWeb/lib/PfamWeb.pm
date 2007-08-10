
# PfamWeb.pm
# jt 20060316 WTSI
#
# $Id: PfamWeb.pm,v 1.39 2007-08-10 15:55:00 jt6 Exp $

=head1 NAME

PfamWeb - application class for the Pfam website

=cut

package PfamWeb;

=head1 DESCRIPTION

This is the main class for the Pfam website catalyst application. It
handles configuration of the application classes and error reporting
for the whole application.

$Id: PfamWeb.pm,v 1.39 2007-08-10 15:55:00 jt6 Exp $

=cut

use strict;
use warnings;

# a useful trick to get Catalyst to confess errors on startup, rather than
# simply dying with a cryptic error about barewords
#use Carp; $SIG{__DIE__} = \&Carp::confess;

# set flags and add plugins for the application
use Catalyst qw/
                 Cache
                 ConfigLoader
                 HTML::Widget
                 Email
                 Session
                 Session::Store::FastMmap
                 Session::State::Cookie
                 PageCache
               /;

# PageCache must be last in the list

# in order to use sessions via cookies
#                 Session
#                 Session::Store::FastMmap
#                 Session::State::Cookie

our $VERSION = '1.0b';

#-------------------------------------------------------------------------------

=head1 CONFIGURATION

Configuration is done through a series of external "Apache-style"
configuration files.

=cut

# grab the location of the configuration file from the environment and
# detaint it. Doing this means we can configure the location of the
# config file in httpd.conf rather than in the code
my( $conf ) = $ENV{PFAMWEB_CONFIG} =~ /([\d\w\/-]+)/;

__PACKAGE__->config( file => $conf );
__PACKAGE__->setup;

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 finalize_config

Overrides the empty C<finalize_config> method from the ConfigLoader plugin, 
turning it into a dumb by-pass of the perl taint checking on configuration
parameters.

=cut

sub finalize_config {
  my $c = shift;
  my $v = Data::Visitor::Callback
            ->new( plain_value => sub {
               return unless defined $_;
               /^(.*)$/;
               $_ = $1;
            }
          );
  $v->visit( $c->config );
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
