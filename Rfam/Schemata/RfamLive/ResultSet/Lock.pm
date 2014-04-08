package RfamLive::ResultSet::Lock;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

sub lockTheDatabase {
  my ( $self, $user, $commit, $commit_list ) = @_;
  
  my $lock_data = $self->search({ 'locked' => 1})->first;

  if($lock_data) {   
    croak("Database is already locked by " . $lock_data->locker."\n");
  } else {
     $self->update_or_create( { locked => 1, 
                                locker => $user, 
                                allowcommits => $commit,  
                                alsoallow  => $commit_list });
   print STDERR "Database is now locked.\n";
  }
}

sub unlockTheDatabase {
  my ( $self, $user, $force ) = @_;
  my $lock_data = $self->search({ 'locked' => 1})->first;

  if($lock_data){   
    if($user ne $lock_data->locker and !$force) {
        croak("The database is locked by ".$lock_data->locker.
                ", if you [$user] want to unlock it, you need to use the -force option");
    } else {
      $lock_data->delete;
      print STDERR "Database unlocked\n";
    }
  }else{
    print STDERR "The database is already unlocked\n";
  }
}
1;
