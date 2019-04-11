#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::SeqFetch;
use Bio::Pfam::MPfam;

my $family = shift;

unless(-d $family) {
  die "Need to specify family dir on the command line\n$0 <family_dir>\n";
}
if($family =~ /(\S+)\/$/) { #Remove trailing '/' if present
  $family = $1; 
}

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
unless($pfamDB->{database} eq "mpfam") {
  die "Need to use the config for mpfam\n";
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

#Check user has filled in all the fields in DESC file
if($famObj->DESC->ID eq "ShortName") {
  die "Need to change the id in the DESC file (currently 'ShortName')\n";
} 
if($famObj->DESC->DE eq "Family description") {
  die "Need to change the description in the DESC file (currently 'Family description')\n";
} 
if($famObj->DESC->AU eq "Who RU") {
  die "Need to change the author name in the DESC file (currently 'Who RU')\n";
}
if($famObj->DESC->SE eq "Where did the seed come from") {
  die "Need to change the seed source in the DESC file (currently 'Where did the seed come from')\n";
}

#if ( $famObj->DESC->AC ) { 
#  die "Your family appears to have an accession, but you are using mfnew! Either remove and "
#    . "let the database automatically assign the accession or use mfci\n";
#}

unless ( $famObj->DESC->ID ) {
  die "Need to fill out the ID field in DESC file\n";
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

#Look for overlaps between seed/full and pfamA_reg_seed
print STDERR "Looking for overlaps\n";
my %regions;
Bio::Pfam::MPfam::store_regions($famObj, \%regions);

my %overlaps;
$pfamDB->getOverlapingSeedPfamRegions(\%regions, \%overlaps);
if (keys %overlaps) {
  print_overlaps(\%overlaps);
  exit(1);
}

#Everything looks good, so load family into the db
#Update pfamA 
Bio::Pfam::MPfam::update_pfamA($famObj, $pfamDB);

#Update pfamA_reg_seed
Bio::Pfam::MPfam::update_pfamA_reg_seed($famObj, $pfamDB);

#Update pfamA_reg_full
Bio::Pfam::MPfam::update_pfamA_reg_full($famObj, $pfamDB);

#Upload SEED
Bio::Pfam::MPfam::upload_seed($famObj, $family, $pfamDB);

#Upload HMM
Bio::Pfam::MPfam::upload_HMM($famObj, $family, $pfamDB);





