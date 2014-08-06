package RfamLive::ResultSet::MotifPdb;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';


sub updateOrCreateMotifPDBReferences {

  my ($self, $motifObj) = @_;

  if ( !$motifObj or !$motifObj->isa('Bio::Rfam::Motif') ) {
  croak('Either the Bio::Rfam::Motif object was undefined or not an object of that type.');
  }

  my $MSAObj = $motifObj->SEED;
  my $nseq = $MSAObj->nseq();

  for (my $i=0; $i < $nseq; $i++) {
    my $seqID = $MSAObj->get_sqname($i);
    if ($seqID =~ /\b(\d[0-9A-Za-z]{3})_([A-Za-z0-9])\/(\d+)-(\d+)\b/) {
      my $PDBid = $1;
      my $chainID = $2;
      my $seqStart = $3;
      my $seqEnd = $4;
      my $pdbLink = $self->find_or_create ( { motif_acc        => $motifObj->DESC->AC,
                                              pdb_id           => $PDBid,
                                              chain            => $chainID,
                                              pdb_start        => $seqStart,
                                              pdb_end          => $seqEnd } );
    }
    elsif ($seqID =~ /\b(\d[0-9A-Za-z]{3})_([A-Za-z0-9])\b/) {
      my $PDBid = $1;
      my $chainID = $2;
      my $pdbLink = $self->find_or_create ( { motif_acc        => $motifObj->DESC->AC,
                                              pdb_id           => $PDBid,
                                              chain            => $chainID } );
    }
    elsif ($seqID =~ /\b(\d[0-9A-Za-z]{3})\/(\d+)-(\d+)\b/) {
      my $PDBid = $1;
      my $seqStart = $2;
      my $seqEnd = $3;
      my $pdbLink = $self->find_or_create ( { motif_acc        => $motifObj->DESC->AC,
                                              pdb_id           => $PDBid,
                                              pdb_start        => $seqStart,
                                              pdb_end          => $seqEnd } );
    }
  }
}

1;
