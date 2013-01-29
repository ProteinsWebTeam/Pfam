package RfamLive::ResultSet::Family;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

sub id2acc {
  my ($self, $id) = @_;

  my $fam = $self->find({rfam_id=> $id});

  return (defined($fam) ? $fam->rfam_acc() : undef);
}




sub updateFamilyFromObj{
  my ($self, $familyObj) = @_;
  if(!$familyObj or !$familyObj->isa('Bio::Rfam::Family')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  
  my $fam = $self->find({rfam_acc=> $familyObj->DESC->AC});
  
  unless($fam){
    croak("Failed to find a database entry in the family table for ".
            $familyObj->DESC->AC.". This is really bad!\n");
  }
  
  #We have found a family, so now set all the essential attributes and
  #zero all calculated data fields.
  
  
}

sub createFamilyFromObj{
  
}
1;