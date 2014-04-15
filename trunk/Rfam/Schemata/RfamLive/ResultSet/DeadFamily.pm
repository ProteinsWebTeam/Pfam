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

sub createFromFamilyRow {
  my ($self, $entry, $comment, $forward, $user) = @_; 
  
  if(!defined($user) or $user !~ /\S+/){
    die "Need a user name for deleting\n";  
  }
  
  if(!defined($comment) or $comment !~ /\S+/){
    die "Need a comment for deleting\n";  
  }
  
  #Use the supplied data from the family row.
  $self->create(
    {
      rfam_acc    => $entry->rfam_acc,
      rfam_id     => $entry->rfam_id,
      comment     => $comment,
      forward_to  => $forward,
      user        => $user,
      killed      => \'NOW()'
    }
  );
}


1;
