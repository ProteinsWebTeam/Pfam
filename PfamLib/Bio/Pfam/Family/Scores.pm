# Scores.pm
#
# Author:        finnr
# Maintainer:    $Id: Scores.pm,v 1.2 2009-10-28 14:27:32 jt6 Exp $
# Version:       $Revision: 1.2 $
# Created:       Nov 30, 2008
# Last Modified: $Date: 2009-10-28 14:27:32 $
=head1 NAME

Bio::Pfam::Family::Scores - An object to represent a DESC file

=cut

package Bio::Pfam::Family::Scores;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: Scores.pm,v 1.2 2009-10-28 14:27:32 jt6 Exp $

=head1 COPYRIGHT

File: Scores.pm

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk)

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

use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'numRegions' => (
  is        => 'ro',
  isa       => 'Int',
  required  => 1
);

has 'regions' => (
  is    => 'ro',
  isa   => 'HashRef[ArrayRef]',
  required => 1
);

__PACKAGE__->meta->make_immutable;

1;
