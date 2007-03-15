
# PfamConfigFormat.pm
# jt 20070119 WTSI
#
# $Id

=head1 NAME

PfamConfigFormat - a Config::Any plugin to load the PfamWeb config file

=cut

package Config::Any::PfamConfigFormat;

=head1 DESCRIPTION

This is a plugin for the Config::Any configuration loading system that
Catalyst uses. The PfamWeb config files are just Apache-style files that
could be loaded using the Config::Any::General loader, but that calls
Config::General in such a way that we'd need to specific absolute
paths for any included component files. Since we want to avoid
absolute paths, we need to override the default behaviour of the 
original loader and add a switch or two to our call to Config::General.

All of this requires that we change the extension for our config files,
otherwise they get loaded by Config::Any::General and we're no further
forward.

=cut

use strict;
use warnings;

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 extensions

Just returns a list of the extensions that this plugin can load. This is 
where we get to specific our "custom" extension type, jcf.

=cut

sub extensions {
    return qw( jcf );
}

=head2 load

The business end of the module. All it does is "require Config::General" and
then loads the file using that, returning the Config::General object.

=cut

sub load {
    my $class = shift;
    my $file  = shift;

    require Config::General;
    my $configfile = Config::General->new( -ConfigFile      => $file,
                                           -IncludeRelative => 1 );
    my $config     = { $configfile->getall };

    return $config;
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
