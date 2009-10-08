package Bio::Pfam::Sequence::Markup;

use Moose;
use Moose::Util::TypeConstraints;

extends 'Bio::Pfam::Sequence::Feature';

has 'headStyle' => (
  isa => enum( [ qw( square diamond circle arrow line ) ] ),
  is  => 'rw'
);

has 'v_align' => (
  isa => enum( [ qw( top bottom )]),
  is  => 'rw'
);

has 'residue' => (
  isa => 'Str',
  is  => 'rw'
);

has 'type' => (
  isa => 'Str',
  is  => 'rw',
  required => 1,
  default => 'unknown'
);

has 'lineColour' => (
  isa => 'ColourStr',
  is  => 'rw',
  coerce => 1,
  default => 'red' 
);

__PACKAGE__->meta->make_immutable;

1;
