package RfamLive::ResultSet::Family;

use strict;
use warnings;

use base 'DBIx::Class::ResultSet';

sub id2acc {
  my ($self, $id) = @_;

  my $fam = $self->find({rfam_id=> $id});

  return (defined($fam) ? $fam->rfam_acc() : undef);
}
1;