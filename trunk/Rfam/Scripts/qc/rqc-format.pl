#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use File::Touch;

use Bio::Rfam::QC;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;

my $config = Bio::Rfam::Config->new;
my $familyIO = Bio::Rfam::FamilyIO->new;

if($#ARGV == -1) {
  help();
}
my $fam = shift;
my $pwd = getcwd;

#Check that $fam corresponds to a directory.
if(!-d $fam){
  help();
}

if(-e "$fam/format"){
  unlink("$fam/format");
}

#------------------------------------------------------------------------------
#First check timestamps, because if they are wrong it is not worth doing anything
my $error = 0;
$error =  Bio::Rfam::QC::checkTimestamps($fam, $config);
if($error){
  warn "The file timestamps look out of sync! Please rebuild this family.\n";
  exit(1);
}
#------------------------------------------------------------------------------
#Load the family.
my $familyObj;
if(!$error){
  eval{
    $familyObj = $familyIO->loadRfamFromLocalFile( $fam, $pwd );
  };
  if($@){
    warn "There was an issues loading the entry from file: $@\n";
    exit(1);
  }
}

#------------------------------------------------------------------------------
#Check the format of the files.
$error = Bio::Rfam::QC::checkFamilyFormat( $familyObj );

#------------------------------------------------------------------------------

if($error){
  warn "There is a format error with the family\n";
}

if( $error ) {
  warn "\nQC-FORMAT:$fam: Serious format errors: ".
  "YOU CAN NOT CHECK THIS IN-deal with listed the errors\n\n";
  exit(1);
}
print STDERR  "\nQC-FORMAT:$fam: No serious format errors found\n";
touch("$fam/format");

#------------------------------------------------------------------------------
sub help {
  print "$0. Checks format of Rfam entry.\nUsage $0 <rfam-dir>\n";
  exit;
}