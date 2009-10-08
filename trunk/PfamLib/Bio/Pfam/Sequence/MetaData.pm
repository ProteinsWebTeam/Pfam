package Bio::Pfam::Sequence::MetaData;

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

1;
