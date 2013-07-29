#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::QC;
use Cwd;

my $pwd = getcwd;

my $family = shift;
if($family eq '-h' or $family eq '--help'){
  help();
}
$family = '.' if(!$family);

#-------------------------------------------------------------------------------
# Get the config
my $config = Bio::Rfam::Config->new;
my $familyIO = Bio::Rfam::FamilyIO->new;
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware
my $familyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );

#Now the QC bit...
my $error = Bio::Rfam::QC::checkCMFormat($familyObj);

#Handle the error
if($error){
  warn "Format error with CMs!\n";
  exit(1);
}else{
  exit(0);
}
#------------------------------------------------------------------------------
sub help {

  print<<EOF;

$0 <family> - Checks the format of the CM.\n";

EOF
exit(1);
}