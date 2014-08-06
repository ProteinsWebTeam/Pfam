package RfamLive::ResultSet::FullRegion;

use strict;
use warnings;
use Carp;
use Data::Printer;
use DBIx::Class::ResultClass::HashRefInflator;
use base 'DBIx::Class::ResultSet';

sub updateFullRegionsFromFamilyObj {
  my ($self, $familyObj) = @_;
  
  if(!$familyObj or !$familyObj->isa('Bio::Rfam::Family')){
    croak('Either the Bio::Rfam::Family object was undefined or not an object of that type.');
  }
  
  #Always start of with no regions!
  my $rfam_acc = $familyObj->DESC->AC;
  $self->search({rfam_acc => $rfam_acc})->delete;
  
  my $count = 0;
  my @row;
  
  foreach my $hit ( @{ $familyObj->SCORES->regions }){
    push(@row, { rfam_acc       => $rfam_acc,
                 rfamseq_acc    => $hit->[3],
                 seq_start      => $hit->[1],
                 seq_end        => $hit->[2],
                 bit_score      => $hit->[4],
                 evalue_score   => $hit->[5],
                 cm_start       => $hit->[6],
                 cm_end         => $hit->[7],
                 truncated      => $hit->[8],
                 type           => $hit->[9],
                 is_significant => 1} );
    $count++;
    next if(scalar(@row) < 1000);
    $self->populate(\@row);
    @row = ();
  }
  
  $self->populate(\@row);
  if($count != $familyObj->SCORES->numRegions){
    croak("Did not upload the correct number of full regions.\n");
  }
  #TODO - search related to check number of FULL sequence match num_seed.
}

sub allRegions {
  my ($self, $rfam_acc) = @_;
  
  my @regions;
  #TODO - add Pagination also stop row inflation - Jody did it for Dfam
  my @row = $self->search({rfam_acc => $rfam_acc},
                          { join => 'rfamseq_acc',
                            '+select' => ['rfamseq_acc.rfamseq_acc'],
                            '+as'     => ['seq_acc'] });
  foreach my $r (@row){
    push(@regions, [ $r->get_column('seq_acc').'/'.$r->seq_start.'-'.$r->seq_end,
                     $r->seq_start,
                     $r->seq_end,
                     $r->get_column('seq_acc'),
                     $r->bit_score,
                     $r->evalue_score,
                     $r->cm_start,
                     $r->cm_end,
                     $r->truncated,
                     $r->type,
                     1]) 
  }
  return \@regions;
}

sub getMatchList{
  my ( $self, $famRow) = @_;
  
  my $expected = $famRow->num_full;
  
  my ($list, $count);
  my $rfam_acc = $famRow->rfam_acc;
  
  my $rs = $self->search({rfam_acc => $rfam_acc }, 
                {select => [{'LENGTH' => 'rfamseq_acc.rfamseq_acc'}, { 'LENGTH' => 'ncbi.species'}],
                 join   => {'rfamseq_acc' => 'ncbi'},
                 as     => [ qw(acc_len species_len)] });
  my $spLen = $rs->get_column('species_len')->max;
  my $acLen = $rs->get_column('acc_len')->max;
  
  p($acLen);
  #TODO - add Pagination
  # also stop row inflation
  #  - use DBIx::Class::ResultClass::HashRefInflator!
  my @row = $self->search({rfam_acc => $rfam_acc},
                          { join => {'rfamseq_acc' => 'ncbi'},
                            '+select' => ['rfamseq_acc.rfamseq_acc', 'ncbi.species'],
                            '+as'     => ['seq_acc', 'species'] });
  foreach my $r (@row){
    $count++;
    $list.= $rfam_acc.' '.$r->get_column('species').' '.$r->get_column('seq_acc').'/'.$r->seq_start.'-'.$r->seq_end."\n";
  }
  unless($count == $expected){
    die "Monumental cock up has occured, mismatch between the number of regions";
  }
  return $list;
}

sub regionsByRfamseqAcc {
  my ($self, $rfamseqAcc, $flipCoos) = @_;
  
  my $rs = $self->search({ 'me.rfamseq_acc' => $rfamseqAcc },
                         { result_class => 'DBIx::Class::ResultClass::HashRefInflator'});
  
  my @regions;
  while(my $rowHashRef = $rs->next){
    push(@regions, [ $rowHashRef->{rfamseq_acc},
                     $rowHashRef->{rfam_acc},
                     $rowHashRef->{seq_start},
                     $rowHashRef->{seq_end},
                     $rowHashRef->{seq_start} > $rowHashRef->{seq_end} ? -1 : 1 ]);
  }
  return \@regions;
}

1;
