package RfamDB::ResultSet::SeedRegion;

use strict;
use warnings;
use Carp;

use base 'DBIx::Class::ResultSet';

sub updateSeedRegionsFromFamilyObj {
  my ($self, $familyObj) = @_;
  
  if(!$familyObj or !$familyObj->isa('Bio::Rfam::Family')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  
  #Always start of with no regions!
  my $rfam_acc = $familyObj->DESC->AC;
  $self->search({rfam_acc => $rfam_acc})->delete;
  
  my $count = 0;
  my @row;
  my $nseq = $familyObj->SEED->nseq;
  for( my $i = 0; $i< $nseq; $i++){
    my $seq = $familyObj->SEED->get_sqname($i);
    if( my($rfamseq_acc, $start, $end) = $seq =~ /^(\S+)\/(\d+)\-(\d+)$/){
    push(@row, { rfam_acc    => $rfam_acc,
                 rfamseq_acc => $rfamseq_acc,
                 seq_start   => $start,
                 seq_end     => $end} );
    $count++;
    next if(scalar(@row) < 1000);
    $self->populate(\@row);
    @row = ();
    }else{
      croak("Failed to parse $seq!");
    }
  }
  $self->populate(\@row);
  if($count != $familyObj->SEED->nseq){
    craok("Did not upload the correct number of sequences.\n");
  }
  #TODO - search related to check number of SEED sequence match num_seed.
}

1;
