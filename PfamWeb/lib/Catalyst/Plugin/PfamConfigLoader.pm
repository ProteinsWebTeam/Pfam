
# PfamConfigLoader.pm
# jt 20070119 WTSI
#
# $Id

=head1 NAME

PfamConfigLoader - a plugin to modify the default behaviour of the
ConfigLoader plugin

=cut

package Catalyst::Plugin::PfamConfigLoader;

=head1 DESCRIPTION

This is a plugin that overrides the default C<finalize_config> method
in the ConfigLoader plugin that ships with catalyst, for the express
purpose of detainting values that are loaded from configuration files.

The values that are read from the Apache-style configuration files are
all tainted when we run under perl's taint mode. This
C<finalize_config> method uses the same mechanism as the original,
walking down each value in the configuration hash before it's passed
to the rest of the catalyst system, and passing each value through a
dumb regex: C</^(.*)$/; $_ = $1>.

This isn't meant to be smart, just practical. We're loading the values
from config files that are specified by the server administrator, so
we pretty much have to trust the values that he's put there. Don't we ?

This plugin should be loaded before the standard ConfigLoader.

=cut

use strict;
use warnings;

# a natty little number that allows us to walk the data structure (in
# this case just a hash) and have a callback function run in response
# to the data type of each element
use Data::Visitor::Callback;

# override the standard ConfigLoader plugin
use base "Catalyst::Plugin::ConfigLoader";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 finalize_config

Overrides the C<finalize_config> method from the base class, turning
it into a dumb by-pass of the perl taint checking on configuration
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
