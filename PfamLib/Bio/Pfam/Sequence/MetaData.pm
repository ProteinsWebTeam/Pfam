package Bio::Pfam::Sequence::MetaData;

use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;


has 'database' => (
  isa => enum([ qw( pfam uniprot UniProt genpept metagenomics Phobius ncoils seg ) ] ),
  is  => 'rw',
);

has 'accession' => (
  isa => 'Str',
  is  => 'rw'
);

has 'identifier' => (
  isa => 'Str',
  is  => 'rw'
);

has 'description' => (
  isa => 'Str',
  is  => 'rw'
);

has 'organism' => (
  isa => 'Str',
  is  => 'rw'
);

has 'type' => (
  isa => 'Str',
  is  => 'rw'
);

has 'score' => (
  isa => 'Num',
  is  => 'rw'
);

has 'scoreName' => (
  isa => 'Str',
  is  => 'rw',
);

has 'start' => (
  isa      => 'Int',
  is       => 'rw',
);

has 'end' => (
  isa => 'Int',
  is  => 'rw',
);

has 'aliStart' => (
  isa      => 'Int',
  is       => 'rw',
);

has 'aliEnd' => (
  isa => 'Int',
  is  => 'rw',
);

has 'taxid' => (
  isa => 'Int',
  is  => 'rw'
);

__PACKAGE__->meta->make_immutable;

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
