package RfamLive::ResultSet::Clan;

use strict;
use warnings;
use Carp;
use Data::Printer;
use DateTime;
use DateTime::Format::MySQL;

use base 'DBIx::Class::ResultSet';

sub id2acc {
  my ( $self, $id ) = @_;

  my $clan = $self->find( { id => $id } );

  return ( defined($clan) ? $clan->clan_acc() : undef );
}

sub updateClanFromObj {
  my ( $self, $clanObj, $wiki ) = @_;
  
  if ( !$clanObj or !$clanObj->isa('Bio::Rfam::Clan') ) {
    croak(
'Either the Bio::Rfam::Clan object was undefined or not an object of that type.'
    );
  }

  my $clan = $self->find( { clan_acc => $clanObj->DESC->AC } );

  unless ($clan) {
    croak("Failed to find a database entry in the clan table for "
        . $clanObj->DESC->AC
        . ". This is really bad!\n" );
  }

  #We have found a family, so now set all the essential attributes and
  #zero all calculated data fields.

  $clan->id( $clanObj->DESC->ID );
  $clan->description( $clanObj->DESC->DE );
  $clan->auto_wiki($wiki) if($wiki);
  $clan->author( $clanObj->DESC->AU );
  $clan->comment( $clanObj->DESC->CC ) if(defined($clanObj->DESC->CC));
  $clan->previous_id( $clanObj->DESC->PI ) if(defined($clanObj->DESC->PI) );
 
  my $dt = DateTime->now( time_zone  => 'Europe/London');
  #$clan->updated( DateTime::Format::MySQL->format_datetime($dt));
  
  $clan->update;
}

sub createFamilyFromObj {
  my ( $self, $familyObj, $wiki ) = @_;
  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    croak(
'Either the Bio::Rfam::Family object was undefined or not an object of that type.'
    );
  }
  my $fam = $self->find( { rfam_acc => $familyObj->DESC->AC } );
  if ($fam) {
    croak("Found a database entry in the family table for "
        . $familyObj->DESC->AC
        . " when there should not be one. This is really bad!\n" );
  }
  my $dt = DateTime->now( time_zone  => 'Europe/London');
  $self->create(
    {
      rfam_acc         => $familyObj->DESC->AC,
      rfam_id          => $familyObj->DESC->ID,
      description      => $familyObj->DESC->DE,
      auto_wiki        => $wiki,
      author           => $familyObj->DESC->AU,
      seed_source      => $familyObj->DESC->SE,
      gathering_cutoff => $familyObj->DESC->CUTGA,
      trusted_cutoff   => $familyObj->DESC->CUTTC,
      noise_cutoff     => $familyObj->DESC->CUTNC,
      comment          => $familyObj->DESC->CC,

      #No previous ID as it is new.
      cmbuild              => $familyObj->DESC->BM,
      cmcalibrate          => $familyObj->DESC->CB,
      cmsearch             => $familyObj->DESC->SM,
      num_seed             => $familyObj->SEED->nseq,
      num_full             => $familyObj->SCORES->numRegions,
      num_genome_seq       => 0,                                #PP field
      num_refseq           => 0,                                # PP field
      type                 => $familyObj->DESC->TP,
      structure_source     => $familyObj->DESC->SS,
      number_of_species    => 0,
      number_3d_structures => 0,
      tax_seed => defined( $familyObj->DESC->TX ) ? $familyObj->DESC->TX : '',
      ecmli_lambda    => $familyObj->CM->cmHeader->{ecmli}->[0],
      ecmli_mu        => $familyObj->CM->cmHeader->{ecmli}->[1],
      ecmli_cal_db    => $familyObj->CM->cmHeader->{ecmli}->[3],
      ecmli_cal_hits  => $familyObj->CM->cmHeader->{ecmli}->[4],
      maxl            => $familyObj->CM->hmmHeader->{maxl},
      clen            => $familyObj->CM->cmHeader->{clen},
      match_pair_node => $familyObj->CM->match_pair_node,
      hmm_tau         => $familyObj->CM->hmmHeader->{forwardStats}->{tau},
      hmm_lambda      => $familyObj->CM->hmmHeader->{forwardStats}->{lambda},
      updated         => DateTime::Format::MySQL->format_datetime($dt),
      created         => \'NOW()'
    }
  );
}


1;
