package Bio::Rfam::View::Plugin::Alignment;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $this = shift;
  print "In Bio::Rfam::View::Plugin::Alignment::process\n";
  print 'Family alignment stuff ' . $this->parent->family->DESC->AC . "\n";
}

1;
1;
