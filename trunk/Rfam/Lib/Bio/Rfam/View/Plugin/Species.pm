package Bio::Rfam::View::Plugin::Species;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $this = shift;
  print "In Bio::Rfam::View::Plugin::Species::process\n";
  print 'Making subburst for ' . $this->parent->family->DESC->AC . "\n";
}

1;
1;
