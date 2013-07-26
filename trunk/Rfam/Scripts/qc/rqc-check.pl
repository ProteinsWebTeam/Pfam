#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use File::Touch;

use Bio::Rfam::QC;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::SVN::Client;

my $config = Bio::Rfam::Config->new;
my $familyIO = Bio::Rfam::FamilyIO->new;
my $client = Bio::Rfam::SVN::Client->new({config => $config});

if($#ARGV == -1) {
  help();
}
my $family = shift;
my $pwd = getcwd;

#Check that $fam corresponds to a directory.
if(!-d $family){
  help();
}


my $familyObj;
eval{
    $familyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );
};
if($@){
   die "There was an issues loading the entry from file: $@\n";
}else{
  print STDERR "Successfully loaded local copy of $family through middleware\n";
}

if(-e "$family/check"){
  unlink("$family/check");
}

my $error = 0;
#See if all sequences are recovered.
$error = Bio::Rfam::QC::compareSeedAndScores($familyObj);

#Check that the family already exists.
eval{
  $client->checkFamilyExists($family); #TODO uncomment, exit if new.
};
if($@){
  warn "\nCan not find family, assuming it is new!\n\n";
  exit($error);
}

my ($oldFamilyObj);
eval {
  $oldFamilyObj = $familyIO->loadRfamFromSVN( $family, $client );
# This is how I tested the object.
# $oldFamilyObj = $familyIO->loadRfamFromLocalFile( 'RF00014', $pwd );
};
if($@){
  die "There was a problem fetching/loading the old family from SVN:$@\n";
}else{
  print STDERR "Successfully loaded (old) SVN copy of $family through middleware\n";
}

my($found, $missing) = 
  Bio::Rfam::QC::compareOldAndNew($oldFamilyObj, $familyObj, "$pwd/$family");

if($error){
  warn "There is a problem with your family. Please fix.\n";
  exit(1);
}else{
  touch("$family/check");
} 

sub help {
  
print "\n$0. Checks sequences of Rfam entry, warning of missing SEED sequneces and comparing old version to update version if applicable.".
      "\n\nUsage $0 <rfam-dir>\n";
  
  exit;
}