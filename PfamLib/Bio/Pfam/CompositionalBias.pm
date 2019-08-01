#!/usr/bin/env perl

=head1 NAME

CompositionalBias

=head1 SYNOPSIS

Calculates the proportion of residues in a fasta file or alignment that
are disordered or contain low complexity as defined by mobiDBlite and 
segmasker respectively

=cut

package Bio::Pfam::CompositionalBias;

use strict;
use warnings;


sub calculate_compositional_bias {

  my $aln_or_fasta = shift;

  my ($fasta, $total_residues) = count_residues($aln_or_fasta);

  my $low_complexity_disorder = count_compositional_bias_residues($fasta);

  my $perc_comp_bias = $low_complexity_disorder/$total_residues*100;
  
  unlink($fasta);

  return($perc_comp_bias);
}


sub count_residues {

  my $aln_or_fasta = shift;
  my $fasta = "fasta.$$";
  my $total_residues;

  open(OUT, ">$fasta") or die "Couldn't open fh to $fasta, $!";
  open(FA, "esl-reformat -u fasta $aln_or_fasta |") or die "Couldn't open fh to 'esl-reformat -u fasta $aln_or_fasta |', $!";
  while(<FA>) {
    if(/^>/) {
      if(/^>(\S+)\//) {     #modbidb_lite doesn't like accessions in the form A0A1Y4TGR4.1/136-354, so strip out co-ordinates
        print OUT ">$1\n";
      }
      else {
        print OUT $_;
      }
    } 
    else {
      chomp($_);
      $total_residues+= length($_);
      print OUT "$_\n";
    }
  }
  close FA;

  return($fasta, $total_residues);

}


sub count_compositional_bias_residues {

  my $fasta = shift;

  #Run seg-masker
  my $seg_out = "seg.out";
  system("segmasker -in $fasta -out $seg_out") and die "Couldn't run 'segmasker -in fasta -out $seg_out', $!";

  my %residues;
  my $acc;
  open(SEG, $seg_out) or die "Couldn't open fh to $seg_out, $!";
  while(<SEG>) {
    if(/^>(\S+)/) {
      $acc=$1;
    }
    elsif(/^(\d+) - (\d+)/) {
      my ($st, $en) = ($1, $2);
      for(my $i=$st; $i<=$en; $i++) {
        my $j=$i+1;
        $residues{$acc}{$j}=1;
      }
    }
    else {
      die "Unrecognised line in seg output: $_";
    }
  }
  close SEG;

  #Run MobiDBlite
  my $mobiDB_out = "mobiDB.out";
  system("mobidb_lite.py -o $mobiDB_out $fasta") and die "Couln't run 'python3 mobidb_lite.py -o $mobiDB_out $fasta', $!";
  open(MOBIDB, $mobiDB_out) or die "Couldn't open fh to $mobiDB_out, $!";
  while(<MOBIDB>) {
    if(/^(\S+)\s+(\d+)\s+(\d+)/) {  #MGYP000688283853        321     360
      my ($acc, $st, $en) = ($1, $2, $3);
      for(my $i=$st; $i<=$en; $i++) {
        $residues{$acc}{$i}=1;
      }
    }
    else {
      die "Unrecognised line in mobiDBlite output: $_";
    }
  }
  close MOBIDB;

  #Count up the residues
  my $total=0;
  foreach my $seq (keys %residues) {
    $total += keys %{$residues{$seq}};
  }

  unlink("seg.out");
  unlink("mobiDB.out");
  return($total);
}

1;
