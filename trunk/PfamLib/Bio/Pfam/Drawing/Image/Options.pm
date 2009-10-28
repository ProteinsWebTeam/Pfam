package Bio::Pfam::Drawing::Image::Options;

use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;

subtype 'jsBoolean'
  => as Str
  => where { $_ eq 'true' or $_ eq 'false' }
  => message { "$_ does not look like javascript boolean [true/false]\n"};

has 'imageMap' => (
  isa => 'jsBoolean',
  is  => 'ro',
  default => 'true'
);

has 'lables' => (
  isa => 'jsBoolean',
  is  => 'ro',
  default => 'true'
);

has 'tips' => (
  isa => 'jsBoolean',
  is  => 'ro',
  default => 'true'
);

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
