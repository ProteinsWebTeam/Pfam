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

  # delete family records if any
  my $family_records = $self->search({rfam_acc => $familyObj->{DESC}->{AC}});
  if(defined($family_records)){
	$family_records->delete();
  }

  if(defined($familyObj->DESC->DBREFS)){
    foreach my $db (@{$familyObj->DESC->DBREFS}){
	my $comment = defined($db->{comment}) ? $db->{comment} : '';
	my $other_params = defined($db->{other_params}) ? $db->{other_params} : '';
	
	my $link = $self->find_or_create( { rfam_acc   => $familyObj->{DESC}->{AC},
                                          db_id        => $db->{db_id},
                                          db_link      => $db->{db_link}},
					  {key => 'composite_key'} );
      	
	if(!defined($link)){	
		croak("Error updating database_link entry for $familyObj->{DESC}->{AC}");
    	}
	else{
  		$link->update({comment => $comment,
                               other_params => $other_params},
                              {key => 'composite_key'});
	}
	}
}
}
1;
