package RfamLive::ResultSet::Motif;

use strict;
use warnings;
use Carp;
use Data::Printer;
use DateTime;
use DateTime::Format::MySQL;

use base 'DBIx::Class::ResultSet';


sub updateMotifFromObj {
  my ( $self, $motifObj) = @_;

  # Check that the object is a motif object
  if ( !$motifObj or !$motifObj->isa('Bio::Rfam::Motif') ) {
    croak(
      'Either the Bio::Rfam::Motif object was undefined or not an object of that type.'
    );
  }

  # Confirm that the motif is already in the database
  my $mot = $self->find( { motif_acc => $motifObj->DESC->AC } );
  unless ($mot) {
    croak('The motif was not found in the database. Something has gone wrong.');
  }

  $mot->motif_acc( $motifObj->DESC->AC);  
  $mot->motif_id( $motifObj->DESC->ID );
  $mot->description( $motifObj->DESC->DE );
  $mot->author( $motifObj->DESC->AU );
  $mot->seed_source( $motifObj->DESC->SE );
  $mot->gathering_cutoff( $motifObj->DESC->CUTGA );
  $mot->trusted_cutoff( $motifObj->DESC->CUTTC );
  $mot->noise_cutoff( $motifObj->DESC->CUTNC );
 
  $mot->cmbuild( $motifObj->DESC->BM );
  $mot->cmcalibrate( $motifObj->DESC->CB );
  
  $mot->type( $motifObj->DESC->TP );
  
  $mot->ecmli_lambda( $motifObj->CM->cmHeader->{ecmli}->[0] );
  $mot->ecmli_mu( $motifObj->CM->cmHeader->{ecmli}->[1] );
  $mot->ecmli_cal_db( $motifObj->CM->cmHeader->{ecmli}->[3] );
  $mot->ecmli_cal_hits( $motifObj->CM->cmHeader->{ecmli}->[4] );
  $mot->maxl( $motifObj->CM->hmmHeader->{maxl} );
  $mot->clen( $motifObj->CM->cmHeader->{clen} );
  $mot->match_pair_node( $motifObj->CM->match_pair_node );
  $mot->hmm_tau( $motifObj->CM->hmmHeader->{forwardStats}->{tau} );
  $mot->hmm_lambda( $motifObj->CM->hmmHeader->{forwardStats}->{lambda} );
  
  my $dt = DateTime->now( time_zone  => 'Europe/London');
  $mot->updated( DateTime::Format::MySQL->format_datetime($dt));
  
  $mot->update;         
 
}

sub createMotifFromObj {  
    
  my ( $self, $motifObj) = @_;

  # Check that the object is a motif object
  if ( !$motifObj or !$motifObj->isa('Bio::Rfam::Motif') ) {
    croak('Either the Bio::Rfam::Motif object was undefined or not an object of that type.');
  }
 
  # Confirm that the motif is NOT already in the database
  my $mot = $self->find( { motif_acc => $motifObj->DESC->AC } );
  if ($mot) {
    croak('The motif was found in the database. Something has gone wrong.');
  }
  
  my $dt = DateTime->now( time_zone  => 'Europe/London');
    $self->create(
      {
        motif_acc           => $motifObj->DESC->AC,
        motif_id            => $motifObj->DESC->ID,
        description         => $motifObj->DESC->DE,
        author              => $motifObj->DESC->AU,
        seed_source         => $motifObj->DESC->SE,

        gathering_cutoff    => $motifObj->DESC->CUTGA,
        trusted_cutoff      => $motifObj->DESC->CUTTC,
        noise_cutoff        => $motifObj->DESC->CUTNC,
        cmbuild             => $motifObj->DESC->BM,
        cmcalibrate         => $motifObj->DESC->CB,
        type                => $motifObj->DESC->TP,

        ecmli_lambda    => $motifObj->CM->cmHeader->{ecmli}->[0],
        ecmli_mu        => $motifObj->CM->cmHeader->{ecmli}->[1],
        ecmli_cal_db    => $motifObj->CM->cmHeader->{ecmli}->[3],
        ecmli_cal_hits  => $motifObj->CM->cmHeader->{ecmli}->[4],
        maxl            => $motifObj->CM->hmmHeader->{maxl},
        clen            => $motifObj->CM->cmHeader->{clen},
        match_pair_node => $motifObj->CM->match_pair_node,
        hmm_tau         => $motifObj->CM->hmmHeader->{forwardStats}->{tau},
        hmm_lambda      => $motifObj->CM->hmmHeader->{forwardStats}->{lambda},
        updated         => DateTime::Format::MySQL->format_datetime($dt),
        created         => \'NOW()'
    } );
}

1;
