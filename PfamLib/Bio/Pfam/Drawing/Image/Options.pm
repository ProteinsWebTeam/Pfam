package Bio::Pfam::Drawing::Image::Options;

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

1;