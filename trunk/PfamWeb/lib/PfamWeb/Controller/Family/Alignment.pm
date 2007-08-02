
# Alignment.pm
# jt6 20070725 WTSI
#
# $Id: Alignment.pm,v 1.2 2007-08-02 15:24:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Alignment - a base class for alignment handling

=cut

package PfamWeb::Controller::Family::Alignment;

=head1 DESCRIPTION

This is intended as the basis for alignment-related code. Its only function
is to set up the paths to make the URLs for sub-classes sensible.

$Id: Alignment.pm,v 1.2 2007-08-02 15:24:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Family';

#-------------------------------------------------------------------------------

=head1 METHODS

None.

=cut

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
