# TransmembraneConfig.pm
#
# Author:        rdf
# Maintainer:    $Id: TransmembraneConfig.pm,v 1.6 2010-01-12 09:41:42 jm14 Exp $
# Version:       $Revision: 1.6 $
# Created:       Aug 10, 2009
# Last Modified: $Date: 2010-01-12 09:41:42 $
=head1 NAME

Template - a short description of the class

=cut

package Bio::Pfam::Drawing::Layout::Config::TransmembraneConfig;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: TransmembraneConfig.pm,v 1.6 2010-01-12 09:41:42 jm14 Exp $

=head1 COPYRIGHT

File: TransmembraneConfig.pm

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

use strict;
use warnings;
use Convert::Color;

use Moose;
use Moose::Util::TypeConstraints;

extends 'Bio::Pfam::Drawing::Layout::Config::GenericMotifConfig';


#-------------------------------------------------------------------------------

=head1 METHODS

=cut


#This sets the generic region to a dark grey colour
sub _setColour{
  my ($self, $motif) = @_;
  $motif->colour( Convert::Color->new( 'rgb8:FF4848') );
}



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

