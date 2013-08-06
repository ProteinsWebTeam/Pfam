#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use File::Touch;
use Getopt::Long;

use Bio::Rfam::QC;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;

#Get the inital set up
my $config   = Bio::Rfam::Config->new;
my $familyIO = Bio::Rfam::FamilyIO->new;

#Get the use options
my( @ignore);


&GetOptions("i=s@" => \@ignore ) or die "Unknown option passed in.";
            
if($#ARGV == -1) {
  help();
}

my $family = shift;
my $pwd = getcwd;

#Check that $fam corresponds to a directory.
if(!-d $family){
  help();
}

my $famPath = "$pwd/$family";

if(-e "$famPath/overlap"){
  unlink("$famPath/overlap");
}

#First check timestamps, because if they are wrong it is not worth doing anything
my $error = 0;
$error =  Bio::Rfam::QC::checkTimestamps($family, $config) if(!$error);
if($error){
  warn "The file timestamps look out of sync! Please rebuild this family.\n";
  exit(1);
}

#Now try and load the family object.
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

#------------------------------------------------------------------------------
#Now the QC bit.
my %ignore = map{$_ => 1}@ignore;
#This should really go in the query! Should be implicit that you ignore self.
$ignore{ $familyObj->DESC->AC }++ if($familyObj->DESC->AC);
#Now run both overlap checls, external and internal SEED overlaps.
$error = Bio::Rfam::QC::overlap($familyObj, $config, \%ignore, $famPath);

#------------------------------------------------------------------------------
#Handle the error.
if($error){
  warn "Your family contains overlaps.  Please see $family/overlap file.\n"; 
  exit(1);
}else{
  exit(0);
}

sub help {
  print<<EOF;

$0 - finds the overlap between a Rfam family and the current Rfamlive database

USAGE: $0 <model-directory>

If no families given, assummes all

OPTIONS
 -i <family> ignore this family (-i can occur multiple times)

-------------------------------------------------------------------------------

OLD OPTIONS (these were never implimented. If required they can be (rdf))  
<list of families to run against>
 -q quiet running
 -n  no log file
 -l  look in current directory for target families

EOF
  
exit;
}


