package Bio::Rfam::View::Plugin::CM;

use Moose;
with 'MooseX::Role::Pluggable::Plugin';

has foo => (
  is  => 'rw',
  isa => 'Int'
);

sub process {
  my $self = shift;
  print "In Bio::Rfam::View::Plugin::CM::process\n";
  print 'Work on this ' . $this->parent->family->SEED->path . "\n";
  $self->annotatedCM;
}

sub annotatedCM {
  my ( $self ) = @_;
    
}

1;
