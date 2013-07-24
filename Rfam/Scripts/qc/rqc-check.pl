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

Bio::Rfam::QC::compareSeedAndScores($familyObj);

#Check that the family already exists.


#$client->checkFamilyExists($family); #TODO uncomment, exit if new.
my ($oldFamilyObj);
eval {
#This needs to be put back at a later date.
#my $oldFamily = $familyIO->loadRfamFromSVN( $family, $client );
  $oldFamilyObj = $familyIO->loadRfamFromLocalFile( 'RF00014', $pwd );
};
if($@){
  die "There was a problem fetching/loading the old family from SVN:$@\n";
}else{
  print STDERR "Successfully loaded (old) SVN copy of $family through middleware\n";
}

Bio::Rfam::QC::compareOldAndNew($oldFamilyObj, $familyObj, "$pwd/$family");
