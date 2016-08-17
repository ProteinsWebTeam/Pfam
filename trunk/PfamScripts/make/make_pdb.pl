#!/usr/bin/env perl

#Before running this script
#Run pfam_scan_farm.pl on the pdb fasta file to generate pfam_scan results:
#pfam_scan_farm.pl -fasta <pdb_fasta_file> -dir <dir location of hmms> -clan_overlap
#
#Then run this script - need to pass in the pdb_fasta file and the results from pfam_scan
#This script will make a fasta file of all pdb regions >=75aa that are not covered by Pfam
#It will reduce the redundancy of this file (40% seq id) using cdhit and then run a pfjbuild
#on each one



use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::FamilyIO;

my ($pfam_scan_results, $fasta);
GetOptions("pfam_scan=s" => \$pfam_scan_results,
            "fasta=s" =>   \$fasta);

die "Usage: $0 -pfam_scan <pfam_scan_results> -fasta <fasta_file>" unless($pfam_scan_results and $fasta and -s $pfam_scan_results and -s $fasta);

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );


#Read fasta file and store protein sequences
print STDERR "Reading $fasta\n";
my $id;
my %fasta;
open(FA, $fasta) or die "Coudln't open fh to $fasta, $!";
while(<FA>) {
  if(/^>(\S+)/) {
    $id=$1;
    unless(/protein/) {
      $id="";
    }
  }
  elsif(/^(\S+)$/) {
    $fasta{$id}.=$1 if($id);  #Only look at protein pdbs
  }
  else {
    die "$_";
  }
}
close FA;


#Read pfam_scan results and store residues that are part of a Pfam family
print STDERR "Reading $pfam_scan_results\n";
my %regs;
open(SCAN, $pfam_scan_results) or die "Coudn't open to $pfam_scan_results, $!";
while(<SCAN>) {

  if(/^#/ or /^\s*$/) { #Comment lines and blank lines
  }
  elsif(/^(\S+)\s+\d+\s+\d+\s+(\d+)\s+(\d+)/) {
    my ($acc, $st, $en) = ($1, $2, $3);

    for(my $i=$st; $i<=$en; $i++) {
      $regs{$acc}{$i}=1;
    }
  }
  else {
    die "$_";
  }
}
close SCAN;


#Create fasta file of all regiona >=75 aa in length that are not covered by Pfam
print STDERR "Creating fasta file of all regions not covered by Pfam\n";
my $fasta_outfile="pdb_regions.fasta";
open(OUT, ">$fasta_outfile") or die "Coudln't open fh to $fasta_outfile, $!";
foreach my $acc (keys %fasta) {

  my $protein_length=length($fasta{$acc});

  my ($start, $end);
  for(my $i=1; $i<=$protein_length; $i++) {
    if(exists($regs{$acc}{$i})) {
      if($start and $end) {
        my $len=$end-$start+1;
        if($len >=75) {  #Ignore regions <75aa
          my $subseq=substr($fasta{$acc}, $start-1, $end);
          print OUT ">$acc/$start-$end\n$subseq\n";
        }
        ($start, $end) = ("", "");
      }
    }
    else {
      if($start) {
        $end=$i;
      }
      else {
        $start=$i;
        $end=$i;
      }
    }
  }
}
close OUT;


#Run cd-hit to reduce redundancy - use a 40% seq id threshold
print STDERR "Running cd-hit to reduce redundancy\n";
my $pdb_reduced_fasta="pdb_regions.reduced";
system("cd-hit -i pdb_regions.fasta -o $pdb_reduced_fasta -c 0.4 -n 2 -S 30 -M 0 -B 1 -T 4") and die "Problem running cd-hit, $!";
unless(-s $pdb_reduced_fasta) {
  die "Problem running cd-hit\n";
}



#Read the redundancy reduced fasta file
$id="";
my %fasta_reduced;
open(FH, $pdb_reduced_fasta) or die "Couldn't open fh to $pdb_reduced_fasta, $!";
while(<FH>) {
  if(/^>(\S+)/) {
    $id=$1;
  }
  else {
    $fasta_reduced{$id}.=$_;
  }
}
close FH;


#Make directory for each pdb region and run pfjbuild
print STDERR "Going to run jackhmmers\n";
foreach my $acc (keys %fasta_reduced) {
  print STDERR "Running jackhmmer on $acc\n";
  
  #Extract pdb_id
  my $pdb_id;
  if($acc =~ /(\S+)\//) {  #3klo_A/1-157
    $pdb_id=$1;
  }
  else {
    die "Couldn't extract pdb_id from $acc\n";
  }

  #Create dir
  my $dir_name = $acc;   #3klo_A/1-157

  $dir_name =~ s/\//_/g;
  $dir_name =~ s/-/_/g;

  mkdir($dir_name, 0755) or die "Couldn't mkdir $dir_name, $!";
  chdir($dir_name) or die "Couldn't chdir into $dir_name, $!";

  #Write fasta file 
  open(FA, ">seq.fa") or die "Coudn't open fh to seq.fa, $!";
  print FA ">$acc\n$fasta_reduced{$acc}";
  close FA;

  #Create DESC file, and fill in the SE line
  my %desc = (
    ID    => 'ShortName',
    DE    => 'Family description',
    AU    => 'Who RU',
    SE    => 'pdb:$pdb_id',
    CUTGA => { seq => '27.00', dom => '27.00' },
    CUTNC => { seq => '27.00', dom => '27.00' },
    CUTTC => { seq => '27.00', dom => '27.00' },
    BM    => 'hmmbuild  -o /dev/null HMM SEED;',
    SM    => 'hmmsearch -Z ' . $config->dbsize . ' -E 1000 HMM pfamseq',
    TP    => 'Domain'
  );

  my $io = Bio::Pfam::FamilyIO->new;
  my $desc = Bio::Pfam::Family::DESC->new(%desc);
  $io->writeDESC($desc);

  #Run pfjbuild
  system("pfjbuild -N 3 -fa seq.fa -gzip");

  chdir("../") or die "Couldn't chdir up from $dir_name, $!";
}

