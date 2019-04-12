#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::SeqFetch;
use Bio::Pfam::MGnifam;

my $family = shift;

unless(-d $family) {
  die "Need to specify family dir on the command line\n$0 <family_dir>\n";
}
if($family =~ /(\S+)\/$/) { #Remove trailing '/' if present
  $family = $1; 
}

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
unless($pfamDB->{database} eq "mgnifam") {
  die "Need to use the config for mgnifam\n";
}


#Check there is a family dir, and that we can write to it
my $cwd = getcwd;
unless(-d "$cwd/$family") { 
  die "$0: [$cwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}
unless(-w "$cwd/$family") { 
  die "$0: I can't write to directory [$cwd/$family].  Check the permissions.\n";
}


#Check family files are present and in correct timestamp order
unless(Bio::Pfam::PfamQC::checkFamilyFiles($family)) { 
  print "$family contains errors. You should rebuild this family.\n";
}


#Read in the family
my $familyIO = Bio::Pfam::FamilyIO->new;
my $famObj = $familyIO->loadPfamAFromLocalFile($family, $cwd);
print STDERR "Successfully loaded $family through middleware\n";


#A few quick checks
unless($famObj->DESC->AC) {
  die "Your DESC file does not have an accession. If this is a new family, use mfnew.pl\n";
}
my $rdb_family = $pfamDB->getSchema->resultset('Mgnifam')->find({ mgnifam_acc => $famObj->DESC->AC });
unless($rdb_family) {
  die "Your accession ".$famObj->DESC->AC." is not in the database. If this is a new family, use mfnew.pl\n";
}
unless($rdb_family->mgnifam_id eq $famObj->DESC->ID) {
  die "Your id (".$famObj->DESC->ID.") is different to what is in the database (".$rdb_family->pfama_id.")\n";
}


#Check that the current checkout is the latest checkout
my $last_modified;
open(LM, "$cwd/$family/.lastModified") or die "Couldn't open fh to .lastModified, $!";
while(<LM>) {
  if(/(.+)/) {
    $last_modified=$1;
    last;
  }
}
unless($last_modified) {
  die "Error: Cannot determine when the family was checked out as there is no .lastModified file";
}
unless($last_modified eq $rdb_family->updated) {
  die "The family has been updated in the database [".$rdb_family->updated."] since you checked this family out [$last_modified]. You will need to get a new checkout.\n";
}


#Some qc checks
my $error=0;
unless(Bio::Pfam::PfamQC::noFragsInSeed($family, $famObj)) { 
  $error=1;
}
unless(Bio::Pfam::PfamQC::nonRaggedSeed($family, $famObj)) { 
  $error=1;
  print STDERR "$0: SEED is not ragged\n";
}
unless(Bio::Pfam::PfamQC::nameFormatIsOK($famObj->DESC->ID)) {
  $error = 1; 
  warn "$family: your family identifier contains disallowed characters\n";
}
unless(Bio::Pfam::PfamQC::onlyASCII( $famObj->DESC, $family )) { 
  $error = 1; 
  warn "$family: desc file contains illegal characters\n";
}
my ($db_size) = $famObj->DESC->SM =~ m/-Z\s+(\d+)/;
unless($db_size eq $config->{mgnify}->{dbsize}) {
  $error=1;
  warn "$family: search was performed with a dbsize of [$db_size], expected [".$config->{mgnify}->{dbsize}."]\n";
}
if($error) {
  warn "$family contains errors\n";
  exit(1);
}
else {
  warn "$family: family $family passes qc checks \n";
}


#Check seed seqs are in the db
my %seed;
my $seed_total=0;
foreach my $seq ( $famObj->SEED->each_seq ) {
  push( @{$seed{$seq->id}}, { start => $seq->start, end => $seq->end } );
  $seed_total++;
}
open(FA, ">$family/FA") or die "Couldn't open fh to $family/FA, $!";
my $noSeqsFound = &Bio::Pfam::SeqFetch::fetchSeqs(\%seed, $config->{mgnify}->{location}, \*FA);
unless($seed_total == $noSeqsFound) {
  die "Did not find all the sequences from your seed alignment in the sequence database, only $seed_total/$noSeqsFound were found\n";
}


#Look for overlaps between seed/full and mgnifam_reg_seed
print STDERR "Looking for overlaps\n";
my %regions;
Bio::Pfam::MGnifam::store_regions($famObj, \%regions);

my %overlaps;
Bio::Pfam::MGnifam::getOverlapingSeedPfamRegions($pfamDB, \%regions, \%overlaps);
if (keys %overlaps) {
  print_overlaps(\%overlaps);
  exit(1);
}


#Everything looks good, so load family into the db
#Update mgnifam 
Bio::Pfam::MGnifam::update_mgnifam($famObj, $pfamDB);


#Update mgnifam_reg_seed
Bio::Pfam::MGnifam::update_mgnifam_reg_seed($famObj, $pfamDB);


#Update mgnifam_reg_full
Bio::Pfam::MGnifam::update_mgnifam_reg_full($famObj, $pfamDB);


#Upload SEED
Bio::Pfam::MGnifam::upload_seed($famObj, $family, $pfamDB);


#Upload HMM
Bio::Pfam::MGnifam::upload_HMM($famObj, $family, $pfamDB);
