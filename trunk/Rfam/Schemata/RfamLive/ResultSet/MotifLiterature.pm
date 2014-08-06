package RfamLive::ResultSet::MotifLiterature;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

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
      $lit->update_or_create_related( 'motif_literature', 
                                      { motif_acc    => $motifObj->DESC->AC,
                                        pmid        => $ref->{RM},
                                        comment     => defined($ref->{RC}) ? $ref->{RC} : '',
                                        order_added => $ref->{RN}} );
    }
  }
}

1;
