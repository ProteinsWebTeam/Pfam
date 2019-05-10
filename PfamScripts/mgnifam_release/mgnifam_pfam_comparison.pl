#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Cwd;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my ($pfam_scan_dir, $mgnifam_scan_dir, $pfam_config, $mgnifam_config);
GetOptions( 'pfam_dir=s'   => \$pfam_scan_dir,
  'mgnifam_dir=s' => \$mgnifam_scan_dir,
  'pfam_config=s' => \$pfam_config,
  'mgnifam_config=s' => \$mgnifam_config);

unless(-d $pfam_scan_dir) {
  help();
}
unless(-s $pfam_config and -s $mgnifam_config) {
  help();
}


#Get database connections
$ENV{'PFAM_CONFIG'}=$pfam_config;
my $pfam_conf = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $pfam_conf->pfamliveAdmin } );

$ENV{'PFAM_CONFIG'}=$mgnifam_config;
my $mgnifam_conf=Bio::Pfam::Config->new;
my $mgnifamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $mgnifam_conf->pfamliveAdmin } );



#Get all the mgnifams
my @mgnifams = $mgnifamDB->getSchema->resultset('Mgnifam')->search({});


#Make a MGnifam hmm file
my $hmm_file = "MGnifam.hmm";
unless($mgnifam_scan_dir and -s "$mgnifam_scan_dir/$hmm_file") {
  print STDERR "Making $hmm_file\n";
  $mgnifam_scan_dir=".";
  open(HMM, ">$hmm_file") or die "Couldn't open fh tp $hmm_file, $!";
  foreach my $mgnifam (@mgnifams) {
    my $hmm = $mgnifamDB->getSchema->resultset('MgnifamHmm')->find({ mgnifam_acc => $mgnifam->mgnifam_acc });
    print HMM $hmm->hmm;
  }
  close HMM;
}

#Index the MGnifam hmm file 
my @index_files = qw(MGnifam.hmm.h3p  MGnifam.hmm.h3m  MGnifam.hmm.h3i  MGnifam.hmm.h3f);
foreach my $file (@index_files) {
  unless(-s "$mgnifam_scan_dir/$file") {
    print STDERR "Running hmmpress on $hmm_file\n";
    system("hmmpress $mgnifam_scan_dir/$hmm_file") and die "Problem running 'hmmpress $mgnifam_scan_dir/$hmm_file', $!";
    last;
  }
}

#Make hmm.dat file
my $hmm_dat_file="MGnifam.hmm.dat";
unless(-s "$mgnifam_scan_dir/$hmm_dat_file") {
  print STDERR "Making $hmm_dat_file file\n";
  open(HMMDAT, ">$hmm_dat_file") or die "Couldn't open fh to $hmm_dat_file file, $!";
  foreach my $mgnifam (@mgnifams) {
    print HMMDAT "# STOCKHOLM 1.0\n";
    print HMMDAT "#=GF ID   " . $mgnifam->mgnifam_id . "\n";
    print HMMDAT "#=GF AC   " . $mgnifam->mgnifam_acc . "\n";
    print HMMDAT "#=GF DE   " . $mgnifam->description . "\n";
    print HMMDAT "#=GF GA   " . $mgnifam->sequence_ga . "; " . $mgnifam->domain_ga . ";\n";
    print HMMDAT "#=GF TP   " . $mgnifam->type . "\n";
    print HMMDAT "#=GF ML   " . $mgnifam->model_length . "\n";
    print HMMDAT "//\n";
  }
  close(HMMDAT);
}


#Run hmmemit on each MGnifam family
my $mgnifam_fasta="mgnifam_consensus.fa";
print STDERR "Running hmmemit on each MGnifam family\n";
open(FA, ">$mgnifam_fasta") or die "Couldn't open fh to $mgnifam_fasta, $!";
foreach my $mgnifam (@mgnifams) {
  my $acc=$mgnifam->mgnifam_acc;

  my $hmm = $mgnifamDB->getSchema->resultset('MgnifamHmm')->find({ mgnifam_acc => $acc });
  open(HMM, ">$acc.hmm") or die "Couldn't open fh to $acc.hmm, $!";
  print HMM $hmm->hmm;
  close HMM; 

  open(H, "hmmemit -c $acc.hmm |") or die "Couldn't open fh to 'hmmemit -c $acc.hmm |', $!";
  while(<H>) {
    if(/^>/) {
      print FA ">$acc\n";
    }
    else {
      print FA $_;
    }
  }
  close H; 
  unlink("$acc.hmm");
}
close FA;


#Run mgnifam_scan on MGnifam consenus fasta seqs
print STDERR "Running mgnifam_scan on $mgnifam_fasta\n";
my $mgnifam_scan_out="mgnifam_scan_out";
system("mgnifam_scan.pl -dir $mgnifam_scan_dir -fasta $mgnifam_fasta > $mgnifam_scan_out");

open(MGNIFAM_SCAN, "$mgnifam_scan_out") or die "Couldn't open fh to $mgnifam_scan_out, $!";
my $mgnifam_error="";
while(<MGNIFAM_SCAN>) {
  if(/^#/) {
    next;
  }
  elsif(/^(\S+)\s+\d+\s+\d+\s+\d+\s+\d+\s+(\S+)/) {  # <seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc>
    my ($consensus_seq, $fam) = ($1, $2);

    unless($consensus_seq eq $fam) {
      print STDERR "Error: QC fail, the consensus sequence from $consensus_seq matches $fam\n";
      $mgnifam_error=1;
    }
  }
}
close MGNIFAM_SCAN;

unless($mgnifam_error) {
  print STDERR "MGnifams do not have matches to each other\n";
}


#Run pfam_scan on MGnifam consenus fasta seqs
my $pfam_scan_out="pfam_scan_out";
system("pfam_scan.pl -dir $pfam_scan_dir -fasta $mgnifam_fasta > $pfam_scan_out");
open(PFAM_SCAN, "pfam_scan_out") or die "Couldn't open fh to $pfam_scan_out, $!";
my $pfam_error="";
while(<PFAM_SCAN>) {
  if(/^#/) {
    next;
  }
  elsif(/^(\S+)\s+\d+\s+\d+\s+\d+\s+\d+\s+(\S+)/) {  # <seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc>
    my ($consensus_seq, $fam) = ($1, $2);
    print STDERR "Error: QC fail, the consensus sequence from $consensus_seq matches $fam\n";
    $pfam_error=1;
  }
}
close PFAM_SCAN;


#Get all the pfams
my @pfams = $pfamDB->getSchema->resultset('PfamA')->search({});


#Run hmmemit on each Pfam family
my $pfam_fasta="pfam_consensus.fa";
open(FA, ">$pfam_fasta") or die "Couldn't open fh to $pfam_fasta, $!";
print STDERR "Running hmmemit on each Pfam family\n";
foreach my $pfam (@pfams) {
  my $acc=$pfam->pfama_acc;

  my $hmm = $pfamDB->getSchema->resultset('PfamAHmm')->find({ pfama_acc => $acc });
  open(HMM, ">$acc.hmm") or die "Couldn't open fh to $acc.hmm, $!";
  print HMM $hmm->hmm;
  close HMM; 

  open(H, "hmmemit -c $acc.hmm |") or die "Couldn't open fh to 'hmmemit -c $acc.hmm |', $!";
  while(<H>) {
    if(/^>/) {
      print FA ">$acc\n";
    }
    else {
      print FA $_;
    }
  }
  close H; 
  unlink("$acc.hmm");
}
close FA;


#Run mgnifam_scan on Pfam consenus fasta seqs
print STDERR "Running mgnifam_scan on $pfam_fasta\n";
my $mgnifam_scan_out2="mgnifam_scan_out2";
system("mgnifam_scan.pl -dir $mgnifam_scan_dir -fasta $pfam_fasta > $mgnifam_scan_out2");

open(MGNIFAM_SCAN, "$mgnifam_scan_out2") or die "Couldn't open fh to $mgnifam_scan_out2, $!";
while(<MGNIFAM_SCAN>) {
  if(/^#/) {
    next;
  }
  elsif(/^(\S+)\s+\d+\s+\d+\s+\d+\s+\d+\s+(\S+)/) {  # <seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc>
    my ($consensus_seq, $fam) = ($1, $2);

    unless($consensus_seq eq $fam) {
      print STDERR "Error: QC fail, the consensus sequence from $consensus_seq matches $fam\n";
      $pfam_error=1;
    }   
  }
}
close MGNIFAM_SCAN;

unless($pfam_error) {
  print STDERR "MGnifams do not have matches to Pfam\n";
}

if(!$pfam_error and !$mgnifam_error) {
  print STDERR "MGnifam passes qc: families do not show matches to each other or to Pfam families\n";
}
else {
  die "MGnifam failed qc: families show matches to each other and/or to Pfam families\n";
}


sub help {
  print STDERR <<EOF;

  $0: Checks if families in MGnifam are related to each other, and if they are related
  to any of the families in Pfam. It does this by generating a consensus sequence for 
  each MGnifam using hmmemit, and running the MGnifam consenus sequences against the 
  MGnifam models and the Pfam models (Pfam-A.hmm). It also generates a consensus sequence
  for each Pfam in the database using hmmemit, and runs them against the MGnifam models.

  Usage: $0 -pfam_dir <directory location of Pfam-A.hmm> -pfam_config <pfam_config> -mgnifam_config <mgnifam_config>

  Options:
    -mgnifam_dir <directory location of MGnifam.hmm> 


EOF
  exit;
}
