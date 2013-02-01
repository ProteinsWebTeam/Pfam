package RfamLive::ResultSet::FullRegion;

use strict;
use warnings;
use Carp;
use Data::Printer;
use base 'DBIx::Class::ResultSet';

sub updateFullRegionsFromFamilyObj {
  my ($self, $familyObj) = @_;
  
  if(!$familyObj or !$familyObj->isa('Bio::Rfam::Family')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  
  #Always start of with no regions!
  my $rfam_acc = $familyObj->DESC->AC;
  $self->search({rfam_acc => $rfam_acc})->delete_all;
  
  my $count = 0;
  my @row;
  
  foreach my $hit ( @{ $familyObj->SCORES->regions }){
    push(@row, { rfam_acc     => $rfam_acc,
                 rfamseq_acc  => $hit->[3],
                 seq_start    => $hit->[1],
                 seq_end      => $hit->[2],
                 bit_score    => $hit->[4],
                 evalue_score => $hit->[5],
                 cm_start     => $hit->[6],
                 cm_end       => $hit->[7],
                 truncated    => $hit->[8],
                 type         => $hit->[9]} );
    $count++;
    next if(scalar(@row) < 1000);
    $self->populate(\@row);
    @row = ();
  }
  
  $self->populate(\@row);
  if($count != $familyObj->SCORES->numRegions){
    craok("Did not upload the correct number of full regions.\n");
  }
  #TODO - search related to check number of FULL sequence match num_seed.
}

sub allRegions {
  my ($self, $rfam_acc) = @_;
  
  my @regions;
  #TODO - add Pagination also stop row inflation - Jody did it for Dfam
  my @row = $self->search({rfam_acc => $rfam_acc});
  foreach my $r (@row){
    push(@regions, [ $r->rfamseq_acc.'/'.$r->seq_start.'-'.$r->seq_end,
                     $r->seq_start,
                     $r->seq_end,
                     $r->rfamseq_acc,
                     $r->bit_score,
                     $r->evalue_score,
                     $r->cm_start,
                     $r->cm_end,
                     $r->truncated,
                     $r->type ]) 
  }
  return \@regions;
}


1;