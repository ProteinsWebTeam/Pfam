
# Section.pm
# jt6 20080307 WTSI
#
# $Id: Section.pm,v 1.1.1.1 2008-03-17 16:37:43 jt6 Exp $

=head1 NAME

RfamWeb::Controller::Section - base class for section pages, e.g. Family

=cut

package RfamWeb::Controller::Section;

=head1 DESCRIPTION

This is the base class for the various "section" controllers, such as
Family, Clan, etc. Sub-classed from the PfamBase class.

$Id: Section.pm,v 1.1.1.1 2008-03-17 16:37:43 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamBase::Controller::Section';

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
