package RfamDB::ResultSet::DeadClan;

use strict;
use warnings;

use base 'DBIx::Class::ResultSet';

sub getAllDeadClanAcc {
  my ($self) = @_;
  
  my @rows = $self->search;
  my $accs = [];
  foreach my $r (@rows){
    push(@$accs, $r->clan_acc);
  }
  
  return($accs);
}

sub createFromClanRow {
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
      clan_acc    => $entry->clan_acc,
      clan_id     => $entry->id,
      comment     => $comment,
      forward_to  => $forward,
      user        => $user
     }
  );
}


1;
