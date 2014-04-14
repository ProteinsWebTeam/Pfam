package RfamLive::ResultSet::DeadFamily;

use strict;
use warnings;

use base 'DBIx::Class::ResultSet';

sub getAllDeadFamilyAcc {
  my ($self) = @_;
  
  my @rows = $self->search;
  my $accs = [];
  foreach my $r (@rows){
    push(@$accs, $r->rfam_acc);
  }
  
  return($accs);
}


1;
