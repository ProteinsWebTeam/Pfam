package RfamDB::ResultSet::MotifMatch;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

sub find_or_createFromMotifMatchObj {
  my ($self, $motifMatchObj) = @_;
  
  if(!$motifMatchObj or !$motifMatchObj->isa('Bio::Rfam::MotifMatch')){
    croak('Either the Bio::Rfam::MotifMatch object was undefined or not an object of that type.');
  }
  $self->find_or_create(
                                 { motif_acc       => $motifMatchObj->{MOTIF_ACC},
                                   rfam_acc        => $motifMatchObj->{RFAM_ACC},
                                   rfamseq_acc     => $motifMatchObj->{RFAMSEQ_ACC},
                                   rfamseq_start   => $motifMatchObj->{RFAMSEQ_START},
                                   rfamseq_stop    => $motifMatchObj->{RFAMSEQ_STOP}, 
                                   query_start     => $motifMatchObj->{QUERY_START},
                                   query_stop      => $motifMatchObj->{QUERY_STOP},
                                   motif_start     => $motifMatchObj->{MOTIF_START},
                                   motif_stop      => $motifMatchObj->{MOTIF_STOP},
                                   e_value         => $motifMatchObj->{E_VALUE},
                                   bit_score       => $motifMatchObj->{BIT_SCORE}
                       } );
}

1;
