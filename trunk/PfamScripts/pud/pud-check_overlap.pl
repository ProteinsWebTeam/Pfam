#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Range;

my $fam1=shift;
my $fam2=shift;

unless($fam1 and -d $fam1 and $fam2 and -d $fam2) {
  die "Need to specify the two families you wish to compare on the command line\nE.g. $0 PF00001 PF00002\n";
}

my $config = Bio::Pfam::Config->new;

my $scores1="$fam1/scores";
my $scores2="$fam2/scores";

my $seed1="$fam1/SEED";
my $seed2="$fam2/SEED";

my $id1=fam_id("$fam1/DESC");
my $id2=fam_id("$fam2/DESC");


#Store regions from fam1 SEED
my %seed;
open(SEED1, $seed1) or die "Couldn't open fh to $seed1, $!";
while(<SEED1>) {
  if(/^(\S+)\/(\d+)-(\d+)/) {
    my ($acc, $st, $en) = ($1, $2, $3);
    push(@{$seed{$acc}}, {st=>$st, en=>$en});
  }
}
close SEED1;

#Store regions from fam1 FULL
my (%regions, $fam1_size, $fam2_size);
open(S1, $scores1) or die "Couldn't open fh to $scores1, $!";
while(<S1>) {
  #328.3 K5UPK2.1/67-469 71-468 6.3e-95
  if(/^(\S+)\s+(\S+)\/(\d+)-(\d+)\s+(\d+)-(\d+)/) {
    my ($bits, $acc, $st, $en, $ali_st, $ali_en) = ($1, $2, $3, $4, $5, $6);
    push(@{$regions{$acc}}, {ali_st=>$ali_st, ali_en=>$ali_en, st=>$st, en=>$en, bits=>$bits}); 
    $fam1_size++;
  }
  else {
    die "Unrecognised line in $scores1: $_";
  }
}
close S1;


#Compare fam2 seed to seed and full regions from fam1
my $overlap;
my @overlapArray;
open(SEED2, $seed2) or die "Couldn't open fh to $seed2, $!";
while(<SEED2>) {
  if(/^(\S+)\/(\d+)-(\d+)/) {
    my ($acc, $st, $en) = ($1, $2, $3);

    #Compare to fam1 seed
    foreach my $reg (@{$seed{$acc}}) {
      unless($reg->{st} > $en or $reg->{en} < $st) {
        push(@overlapArray, "Sequence [$acc] overlap $id1 $fam1/".$reg->{st}."-".$reg->{en}." SEED overlaps with $id2 $fam2/$st-$en SEED");
        $overlap=1;
      }
    }

    #Compare to fam1 full
    foreach my $reg (@{$regions{$acc}}) {
      unless($reg->{ali_st} > $en or $reg->{ali_en} < $st) {
        push(@overlapArray, "Sequence [$acc] overlap $id1 $fam1/".$reg->{ali_st}."-".$reg->{ali_en}." ($fam1/".$reg->{st}."-".$reg->{en}.", ".$reg->{bits}." bits) FULL overlaps with $id2 $fam2/$st-$en SEED"); 
        $overlap=1;
      }
    } 
  }
}
close SEED2;



#Compare fam2 full to seed and full regions from fam1
my (@fam1_short, @fam2_short);
open(S2, $scores2) or die "Couldn't open fh to $scores2, $!";
while(<S2>) {
  if(/^(\S+)\s+(\S+)\/(\d+)-(\d+)\s+(\d+)-(\d+)/) {
    my ($bits, $acc, $st, $en, $ali_st, $ali_en) = ($1, $2, $3, $4, $5, $6);

    #Compare to fam1 seed
    foreach my $reg (@{$seed{$acc}}) {
      unless($reg->{st} > $ali_en or $reg->{en} < $ali_st) {
        push(@overlapArray, "Sequence [$acc] overlap $id1 $fam1/".$reg->{st}."-".$reg->{en}." SEED overlaps with $id2 $fam2/$ali_st-$ali_en ($fam2/$st-$en$bits bits) FULL");
        $overlap=1;
      }
    }

    #Compare to fam1 full
    foreach my $reg (@{$regions{$acc}}) {
      unless($reg->{ali_st} > $ali_en or $reg->{ali_en} < $ali_st) {

        #Need to filter these overlap according to parameters in config file
        my $region1 = new Bio::Range(-start => $reg->{ali_st}, -end => $reg->{ali_en}, -strand => +1);
        my $region2 = new Bio::Range(-start => $ali_st, -end => $ali_en, -strand => +1);

        my ($s, $e, $d) = $region1->intersection($region2);
        my $overlapLength = ($e - $s) + 1;
        my $ovRegion;
        if($bits > $reg->{bits}) {
          $ovRegion=$region1;
        }
        else {
          $ovRegion=$region2;
        }
        my $regionLength=$ovRegion->length;

        my $percOverlap=$overlapLength/$regionLength*100;
        if($percOverlap >= $config->sequenceOverlapRule) { #Long overlap so must report
          push(@overlapArray, "Sequence [$acc] overlap $id1 $fam1/".$reg->{ali_st}."-".$reg->{ali_en}." ($fam1/".$reg->{st}."-".$reg->{en}.", ".$reg->{bits}." bits) FULL overlaps with $id2 $fam2/$ali_st-$ali_en ($fam2/$st-$en, $bits bits) FULL");
          $overlap=1;
        }
        else { #Short overlap so will depend of how many short overlaps there are as to if we should report them
          if($ovRegion==$region1) { #Family with lowest bit score gets the overlap
            push(@fam1_short, "Sequence [$acc] overlap $id1 $fam1/".$reg->{ali_st}."-".$reg->{ali_en}." ($fam1/".$reg->{st}."-".$reg->{en}.", ".$reg->{bits}." bits) FULL overlaps with $id2 $fam2/$ali_st-$ali_en ($fam2/$st-$en, $bits bits) FULL");
          }
          else {
            push(@fam2_short, "Sequence [$acc] overlap $id1 $fam1/".$reg->{ali_st}."-".$reg->{ali_en}." ($fam1/".$reg->{st}."-".$reg->{en}.", ".$reg->{bits}." bits) FULL overlaps with $id2 $fam2/$ali_st-$ali_en ($fam2/$st-$en, $bits bits) FULL");
          }
        }
      }
    }
    $fam2_size++;
  }
}
close S2;

#Print any seed overlaps
foreach my $overlap (@overlapArray) {
  print "$overlap\n";
}

#Now look to see if any full-full overlaps are low enough in proportion to not need reporting
my $fam1_short=@fam1_short;
my $propO=($fam1_short/$fam1_size)*100;
if($propO >= $config->familyOverlapRule)  {
  foreach my $ov (@fam1_short) {
    print "$ov\n";
    $overlap=1;
  }
}

my $fam2_short=@fam2_short;
$propO=($fam2_short/$fam2_size)*100;
if($propO >= $config->familyOverlapRule)  {
  foreach my $ov (@fam2_short) {
    print "$ov\n";
    $overlap=1;
  } 
} 

#If we didn't see any overlaps, lets say so
unless($overlap) {
  print "There are 0 overlaps between $fam1 and $fam2\n";
}


sub fam_id {
  my ($desc_file) = @_;

  my $id;
  open(DESC, $desc_file) or die "Couldn't open fh to $desc_file, $!";
  while(<DESC>) {
    if(/^ID\s+(\S+)/) {
      $id=$1;
      last;
    }
  }
  close DESC;

  return $id;
}
