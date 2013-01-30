package RfamLive::ResultSet::DatabaseLink;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

sub find_or_createFromFamilyObj {
  my ($self, $familyObj) = @_;
  
  if(!$familyObj or !$familyObj->isa('Bio::Rfam::Family')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  if(defined($familyObj->DESC->DBREFS)){
    foreach my $db (@{$familyObj->DESC->DBREFS}){
      my $link = $self->find_or_create( { rfam_acc     => $familyObj->DESC->AC,
                                          db_id        => $db->{db_id},
                                          comment      => defined($db->{comment}) ? $db->{comment} : '',
                                          db_link      => $db->{db_link},
                                          other_params => defined($db->{other_params}) ? $db->{other_params} : ''
      } );
    }
  }
}

1;