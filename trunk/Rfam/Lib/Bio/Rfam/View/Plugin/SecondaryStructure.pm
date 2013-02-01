package Bio::Rfam::View::Plugin::SecondaryStructure;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $this = shift;
  print "In Bio::Rfam::View::Plugin::SecondaryStructure::process\n";
  print 'Work on this ' . $this->parent->family->SEED->path . "\n";
}

1;
1;
