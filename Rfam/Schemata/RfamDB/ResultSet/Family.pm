package RfamDB::ResultSet::Family;

use strict;
use warnings;
use Carp;
use Data::Printer;
use DateTime;
use DateTime::Format::MySQL;

use base 'DBIx::Class::ResultSet';

sub id2acc {
  my ( $self, $id ) = @_;

  my $fam = $self->find( { rfam_id => $id } );

  return ( defined($fam) ? $fam->rfam_acc() : undef );
}

sub updateFamilyFromObj {
  my ( $self, $familyObj, $wiki ) = @_;
  
  if ( !$familyObj or !$familyObj->isa('Bio::Rfam::Family') ) {
    croak(
'Either the Bio::Rfam::Family object was undefined or not an object of that type.'
    );
  }

  my $fam = $self->find( { rfam_acc => $familyObj->DESC->AC } );

  unless ($fam) {
    croak("Failed to find a database entry in the family table for "
        . $familyObj->DESC->AC
        . ". This is really bad!\n" );
  }

  #We have found a family, so now set all the essential attributes and
  #zero all calculated data fields.

  $fam->rfam_id( $familyObj->DESC->ID );
  $fam->description( $familyObj->DESC->DE );
  $fam->auto_wiki($wiki) if($wiki);
  $fam->author( $familyObj->DESC->AU );
  $fam->seed_source( $familyObj->DESC->SE );
  $fam->gathering_cutoff( $familyObj->DESC->CUTGA );
  $fam->trusted_cutoff( $familyObj->DESC->CUTTC );
  $fam->noise_cutoff( $familyObj->DESC->CUTNC );
  $fam->comment( $familyObj->DESC->CC );
  $fam->previous_id( $familyObj->DESC->PI ) if(defined($familyObj->DESC->PI) );
  #NopreviousIDasitisnew.
  $fam->cmbuild( $familyObj->DESC->BM );
  $fam->cmcalibrate( $familyObj->DESC->CB );
  $fam->cmsearch( $familyObj->DESC->SM );
  $fam->num_seed( $familyObj->SEED->nseq );
  $fam->num_full( $familyObj->SCORES->numRegions );
  $fam->num_genome_seq(0);    #PPfield
  $fam->num_refseq(0);        #PPfield
  $fam->type( $familyObj->DESC->TP );
  $fam->structure_source( $familyObj->DESC->SS );
  $fam->number_of_species(0);
  $fam->number_3d_structures(0);
  $fam->tax_seed( defined( $familyObj->DESC->TX ) ? $familyObj->DESC->TX : '' );
  $fam->ecmli_lambda( $familyObj->CM->cmHeader->{ecmli}->[0] );
  $fam->ecmli_mu( $familyObj->CM->cmHeader->{ecmli}->[1] );
  $fam->ecmli_cal_db( $familyObj->CM->cmHeader->{ecmli}->[3] );
  $fam->ecmli_cal_hits( $familyObj->CM->cmHeader->{ecmli}->[4] );
  $fam->maxl( $familyObj->CM->hmmHeader->{maxl} );
  $fam->clen( $familyObj->CM->cmHeader->{clen} );
  $fam->match_pair_node( $familyObj->CM->match_pair_node );
  $fam->hmm_tau( $familyObj->CM->hmmHeader->{forwardStats}->{tau} );
  $fam->hmm_lambda( $familyObj->CM->hmmHeader->{forwardStats}->{lambda} );
  my $dt = DateTime->now( time_zone  => 'Europe/London');
  $fam->updated( DateTime::Format::MySQL->format_datetime($dt));
  
  $fam->update;
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


sub getDESCData {
  my ($self, $acc, $descData) = @_;
  
  my $row = $self->find( { rfam_acc => $acc });
  
  $descData->{AC} = $row->rfam_acc;
  $descData->{ID} = $row->rfam_id;
  $descData->{AU} = $row->author;
  $descData->{DE} = $row->description;
  $descData->{SE} = $row->seed_source;
  
  $descData->{BM} = $row->cmbuild;
  $descData->{CB} = $row->cmcalibrate;
  $descData->{SM} = $row->cmsearch;
  $descData->{TP} = $row->type;
  $descData->{SS} = $row->structure_source;
  
  
  $descData->{CC} = $row->comment 
    if(defined($row->comment) and $row->comment =~ /\S+/);
  $descData->{PI} = $row->previous_id 
     if(defined($row->previous_id) and $row->previous_id =~ /\S+/);
  $descData->{TX} = $row->tax_seed 
     if(defined($row->tax_seed) and $row->tax_seed =~ /\S+/);
  
  
  $descData->{CUTGA} = $row->gathering_cutoff;
  $descData->{CUTTC} = $row->trusted_cutoff;
  $descData->{CUTNC} = $row->noise_cutoff;
  
  
  $descData->{WIKI}->{ $row->auto_wiki->title }++;

  my @dblinks = $row->database_links;
  foreach my $l (@dblinks){
  
   #These two should be there
    my $lHash = { db_id   => $l->db_id,
                  db_link => $l->db_link};
                  
    #These are optional.
    $lHash->{other_params} = $l->other_params 
      if (defined($l->other_params) and $l->other_params =~ /\S+/);
    $lHash->{db_comment}   = $l->comment
          if (defined($l->comment) and $l->comment =~ /\S+/);
  
    push(@{$descData->{DBREFS}}, $lHash);
  }
  
  my @litRefs = $row->family_literature_references;
  
  my $lits;
  foreach my $l ( @litRefs){
    my $ref = {};
    $ref->{RC} = $l->comment 
      if(defined($l->comment) and $l->comment =~ /\S+/);
    
    $ref->{RN} = $l->order_added; 
    $ref->{RM} = $l->pmid->pmid;
    $ref->{RT} = $l->pmid->title;
    $ref->{RA} = $l->pmid->author;
    $ref->{RL} = $l->pmid->journal;
    $lits->[($ref->{RN} - 1 )] = $ref;
  }
  $descData->{REFS} = $lits;
}

sub getAllFamilyAcc {
  my ($self) = @_;
  
  my @rows = $self->search;
  my $accs = [];
  foreach my $r (@rows){
    push(@$accs, $r->rfam_acc);
  }
  return($accs);
}


1;
