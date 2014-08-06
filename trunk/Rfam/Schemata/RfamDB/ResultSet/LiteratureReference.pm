package RfamDB::ResultSet::LiteratureReference;

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

sub find_or_createFromClanObj {
  my ($self, $clanObj) = @_;
  
  if(!$clanObj or !$clanObj->isa('Bio::Rfam::Clan')){
    croak('Either the Bio::Rfam::Clan object was undefined or not an object of that type.');
  }
  if(defined($clanObj->DESC->REFS)){
    foreach my $ref (@{$clanObj->DESC->REFS}){
      my $lit = $self->find_or_create( { pmid => $ref->{RM},
                                         title => $ref->{RT},
                                         author => $ref->{RA},
                                         journal => $ref->{RL}
      } );
      $lit->update_or_create_related( 'clan_literature_references', #the relationship
                                      { clan_acc    => $clanObj->DESC->AC,
                                        pmid        => $ref->{RM},
                                        comment     => defined($ref->{RC}) ? $ref->{RC} : '',
                                        order_added => $ref->{RN}} );
    }
  }
}

sub find_or_createFromMotifObj {
  my ($self, $motifObj) = @_;

  if(!$motifObj or !$motifObj->isa('Bio::Rfam::Motif')){
    croak('Either the Bio::Rfam::Motif object was undefined or not an object of that type.');
  }
  if(defined($motifObj->DESC->REFS)){
    foreach my $ref (@{$motifObj->DESC->REFS}){
      my $lit = $self->find_or_create( { pmid => $ref->{RM},
                                         title => $ref->{RT},
                                         author => $ref->{RA},
                                         journal => $ref->{RL}
      } );
      $lit->update_or_create_related( 'motif_literature_references', #the relationship
                                      { motif_acc    => $motifObj->DESC->AC,
                                        pmid         => $ref->{RM},
                                        comment      => defined($ref->{RC}) ? $ref->{RC} : '',
                                        order_added  => $ref->{RN}} );
    }
  }
}

1;
