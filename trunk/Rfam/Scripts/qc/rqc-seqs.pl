#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use File::Touch;

use Bio::Rfam::QC;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::SeqDB;

my $config = Bio::Rfam::Config->new;
my $familyIO = Bio::Rfam::FamilyIO->new;

if($#ARGV == -1) {
  help();
}
my $family = shift;
my $pwd = getcwd;

#Check that $fam corresponds to a directory.
if(!-d $family){
  help();
}

if(-e "$family/sequences"){
  unlink("$family/sequences");
}

#First check timestamps, because if they are wrong it is not worth doing anything
my $error = 0;

my $familyObj;
if(!$error){
  eval{
    $familyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );
  };
  if($@){
    warn "There was an issues loading the entry from file: $@\n";
    exit(1);
  }
}



my $rfamseqObj = $config->rfamseqObj;
$error = Bio::Rfam::QC::checkSEEDSeqs($familyObj, $rfamseqObj );
if($error){
  print STDERR "$family contains errors.  You should rebuild this family.\n";
  exit(1)
}
$error = Bio::Rfam::QC::checkScoresSeqs($familyObj, $rfamseqObj);
