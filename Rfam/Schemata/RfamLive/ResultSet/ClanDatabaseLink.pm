package RfamDB::ResultSet::ClanDatabaseLink;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

sub find_or_createFromClanObj {
  my ($self, $clanObj) = @_;
  
  if(!$clanObj or !$clanObj->isa('Bio::Rfam::Clan')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  if(defined($clanObj->DESC->DBREFS)){
    foreach my $db (@{$clanObj->DESC->DBREFS}){
      my $link = $self->find_or_create( { clan_acc     => $clanObj->DESC->AC,
                                          db_id        => $db->{db_id},
                                          comment      => defined($db->{comment}) ? $db->{comment} : '',
                                          db_link      => $db->{db_link},
                                          other_params => defined($db->{other_params}) ? $db->{other_params} : ''
      } );
    }
  }
}

1;