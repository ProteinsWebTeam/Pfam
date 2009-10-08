
package Bio::Pfam::HMM::HMMMatch;

use Moose;
use Moose::Util::TypeConstraints;


subtype 'evalue'
  => as Str
  => where { $_ =~ m/^(\d+(\.\d+){0,1}e[+|-]\d+|\d+\.\d+|\d+)$/ }
  => message { "$_ does not look like an evalue" };

has 'evalue' => (
  isa       => 'evalue', 
  is        => 'rw',
  required  => 1
);

has 'bits' => (
  isa => 'Str',
  is  => 'rw',
  required => 1
);

has 'name' => (
  isa => 'Str',
  is  => 'rw',
  required => 1
);

has bias => (
  isa => 'Num',
  is  => 'rw'
);

  __PACKAGE__->meta->make_immutable;
1;
