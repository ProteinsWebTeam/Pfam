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
  my ($self, $familyObj, $wiki) = @_;
  if(!$familyObj or !$familyObj->isa('Bio::Rfam::Family')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  my $fam = $self->find({rfam_acc=> $familyObj->DESC->AC});
  if($fam){
    croak("Found a database entry in the family table for ".
            $familyObj->DESC->AC." when there should not be one. This is really bad!\n");
  }
  
  $self->create({ rfam_acc    => $familyObj->DESC->AC,
                  rfam_id     => $familyObj->DESC->ID,
                  description => $familyObj->DESC->DE,
                  auto_wiki   => $wiki,
                  author      => $familyObj->DESC->AU,
                  seed_source => $familyObj->DESC->SE,
                  gathering_cutoff => $familyObj->DESC->CUTGA,
                  trusted_cutoff => $familyObj->DESC->CUTTC,
                  noise_cutoff => $familyObj->DESC->CUTNC,
                  comment      => $familyObj->DESC->CC,
                  #No previous ID as it is new.
                  cmbuild      => $familyObj->DESC->BMETH->[0],
                  cmcalibrate  => $familyObj->DESC->BMETH->[1],
                  cmsearch     => $familyObj->DESC->SM,
                  num_seed     => $familyObj->SEED->nseq,
                  num_full     => $familyObj->SCORES->numRegions,
                  num_genome_seq => 0, #PP field
                  num_refseq    => 0, # PP field
                  type          => $familyObj->DESC->TP,
                  structure_source => $familyObj->DESC->SS,
                  number_of_species => 0,
                  number_3d_structures => 0,
                  tax_seed => '',
                  ecmli_lambda => $familyObj->CM->cmHeader->{ecmli}->[0],
                  ecmli_mu => $familyObj->CM->cmHeader->{ecmli}->[1],
                  ecmli_cal_db => $familyObj->CM->cmHeader->{ecmli}->[3],
                  ecmli_cal_hits => $familyObj->CM->cmHeader->{ecmli}->[4],
                  maxl => $familyObj->CM->cmHeader->{maxl},
                  clen => $familyObj->CM->cmHeader->{clen},
                  match_pair_node => $familyObj->CM->match_pair_node,
                  hmm_tau => $familyObj->CM->hmmHeader->{forwardStats}->{tau},
                  hmm_tau => $familyObj->CM->hmmHeader->{forwardStats}->{lambda} });
  
}
1;