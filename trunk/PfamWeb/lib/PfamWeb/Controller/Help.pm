
# Help.pm
# jt6 20060925 WTSI
#
# $Id: Help.pm,v 1.8 2008-03-19 13:34:04 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Help - controller to build the help pages

=cut

package PfamWeb::Controller::Help;

=head1 DESCRIPTION

Displays the help pages for the PfamWeb site.

Generates a B<tabbed page>.

$Id: Help.pm,v 1.8 2008-03-19 13:34:04 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::Section';

# set the name of the section
__PACKAGE__->config( { SECTION => 'help' } );

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
