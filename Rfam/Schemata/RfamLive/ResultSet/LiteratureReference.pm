package RfamLive::ResultSet::LiteratureReference;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

sub find_or_createFromFamilyObj {
  my ($self, $familyObj) = @_;
  
  if(!$familyObj or !$familyObj->isa('Bio::Rfam::Family')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  if(defined($familyObj->DESC->REFS)){
    foreach my $ref (@{$familyObj->DESC->REFS}){
      my $lit = $self->find_or_create( { pmid => $ref->{RM},
                                         title => $ref->{RT},
                                         author => $ref->{RA},
                                         journal => $ref->{RL}
      } );
      $lit->update_or_create_related( 'family_literature_references', 
                                      { rfam_acc    => $familyObj->DESC->AC,
                                        pmid        => $ref->{RM},
                                        comment     => defined($ref->{RC}) ? $ref->{RC} : '',
                                        order_added => $ref->{RN}} );
    }
  }
}

1;