#!/usr/local/bin/perl
#
# Perform the basic format checks on a family.
#

use strict;
use warnings;
use Cwd;

use Bio::Pfam::PfamQC;
use Bio::Pfam::FamilyIO;

my $family = shift;
chomp($family);

unless($family){
  warn "\n***** No family passed  *****\n\n"; 
  help();
}


my $familyIO = Bio::Pfam::FamilyIO->new;

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if( !(-d "$pwd/$family") ) {
    die "$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if( !-w "$pwd/$family" ) {
    die "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

#-------------------------------------------------------------------------------

if( !Bio::Pfam::PfamQC::checkFamilyFiles( $family) ){
    print "pfci: $family contains errors.  You should rebuild this family.\n";
    exit(1);
}
  
my $famObj = $familyIO->loadPfamAFromLocalFile($family, $pwd);
print STDERR "Successfully loaded $family through middleware\n";

unless(Bio::Pfam::PfamQC::passesAllFormatChecks($famObj, $family)){
    exit(1); 
}
