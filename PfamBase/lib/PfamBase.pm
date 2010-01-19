
# PfamBase.pm
# jt 20080304 WTSI
#
# $Id: PfamBase.pm,v 1.3 2010-01-19 09:57:49 jt6 Exp $

=head1 NAME

PfamBase - base class for the Pfam group website application classes

=cut

package PfamBase;

=head1 DESCRIPTION

This is the base class for catalyst web applications in the Pfam group. It's
intended to set up the common features of the various sites, with site-specific
configuration being done in sub-classes, one for each site.

$Id: PfamBase.pm,v 1.3 2010-01-19 09:57:49 jt6 Exp $

=cut

use strict;
use warnings;

use local::lib '/opt/perl5';

use Config::General;

# a useful trick to get Catalyst to confess errors on startup, rather than
# simply dying with a cryptic error about barewords
#use Carp; $SIG{__DIE__} = \&Carp::confess;

# load common plugins
use Catalyst qw/
                 ConfigLoader
                 Cache
               /;
# PageCache must be last in the list

our $VERSION = '0.1';

#-------------------------------------------------------------------------------

=head1 CONFIGURATION

Configuration is done through a series of external "Apache-style"
configuration files. All that we do here is configure the L<Config::General>
module so that it will look along relative paths and will allow Apache-style
"include" directives, both of which we need to load various configuration files
into the main site config.

=cut

__PACKAGE__->config->{'Plugin::ConfigLoader'}->{driver} =
  { General => { -IncludeRelative  => 1,
                 -UseApacheInclude => 1 } };

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 finalize_config

Overrides the default C<finalize_config> method from the ConfigLoader plugin, 
turning it into a dumb by-pass of the perl taint checking on configuration
parameters.

=cut

sub finalize_config {
  my $c = shift;
  my $v = Data::Visitor::Callback
            ->new( plain_value => sub {
               return unless defined $_;
               /^(.*)$/s;
               $_ = $1;
            }
          );
  $v->visit( $c->config );

  # make sure we run the original finalize_config, so that the substitutions
  # happen as per the documentation
  $c->NEXT::finalize_config( @_ );
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
