
# $Author: jt6 $


package Bio::Pfam::Drawing::Layout::Config::DisorderConfig;

use strict;
use warnings;
use Convert::Color;

use Moose;

extends 'Bio::Pfam::Drawing::Layout::Config::GenericMotifConfig';

sub _setColour{
  my ($self, $motif) = @_;
  $motif->colour( Convert::Color->new( 'rgb8:CCCCCC' ) );
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
