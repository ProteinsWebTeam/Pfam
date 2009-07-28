#!/usr/local/bin/perl
#
# Script to run all QC checks at a given location.
#

use strict;
use warnings;
use Cwd;
use Getopt::Long;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

my ( $verbose, $help );

&GetOptions(
  "verbose" => \$verbose,
  "help"    => \$help,
);

my $family = shift;

unless($family){
  print STDERR "\n*** No family specified ***\n\n";
  help(); 
}

$verbose and print STDERR "Going to run PfamQC methods on $family\n";

my $config = Bio::Pfam::Config->new;

#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;
my $pwd = getcwd;

#-------------------------------------------------------------------------------
#First QC step is to check the timestamps on the files

if ( !Bio::Pfam::PfamQC::checkFamilyFiles($family) ) {
  print STDERR "$0: $family contains errors.  You should rebuild this family.\n";
  exit(1);
}

$verbose and print STDERR "$0: All family files are present\n";

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware

my $familyIO = Bio::Pfam::FamilyIO->new;
my $famObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
$verbose and print STDERR "Successfully loaded $family through middleware\n";

#-------------------------------------------------------------------------------
#Check DESC file for ID/AC and that if we have a CL line that we really meant it

my ($isNew, $svnFamObj);
if ( $famObj->DESC->AC ) {
  $isNew = 0;
  
  $verbose and  print STDERR "$0: Found an accession, assming the family already exists in the respository\n";
  
  #Check that the family exists in the repository
  $client->checkFamilyExists($famObj->DESC->AC); 
  
  $svnFamObj= $familyIO->loadPfamAFromSVN($famObj->DESC->AC, $client);
  
  unless(defined($svnFamObj) and $svnFamObj->isa('Bio::Pfam::Family::PfamA')){
    die "$0: Failed to load family from SVN $svnFamObj\n";  
  }
  
  #Check that the ID/ACs are the same
   #Check the desc accessions are the same
  if ( $svnFamObj->DESC->AC ne $famObj->DESC->AC ) {
    die "$0: Accession error, your local copy does not match the repository\n";
  }

  #Check the desc accessions are the same
  if ( $svnFamObj->DESC->ID ne $famObj->DESC->ID ) {
    die "$0: Identifier error, your local copy does not match the repository\n";
  }


}
else {
  $isNew = 1;

  #-------------------------------------------------------------------------------
  #Check that the pending family does ot already exist. Parnoid check as new families
  #should be removed immediately into the main respository.

  $client->checkNewFamilyDoesNotExists( $famObj->DESC->ID );
  
}

unless ( $famObj->DESC->ID ) {
  die
"$0: Your family does not appear have an identifier!  The name of the family is now "
    . "supplied in the DESC file and families are stored under their accession\n."
    . "The check-in process will automatically assign the accession and position it in the
  repository for you!\n";
}

if ( $famObj->DESC->CL ) {
  #Check clan is in the SVN repository.
  $verbose and print STDERR "$0: Found a clan cross references in you DESC file, checking\n";
  my $clanIO = Bio::Pfam::ClanIO->new;
  my $clanObj = $clanIO->loadClanFromSVN($famObj->DESC->CL, $client);
  
  unless(defined($clanObj) and $clanObj->isa('Bio::Pfam::Clan::Clan')){
    die "$0: Found a clan cross reference, but failed to load the Clan from the SVN repository\n";
  }
  
}

#If we are at sanger, perform an overlap check against the database.
if ( $config->location eq "WTSI" ) {
  my $connect = $config->pfamlive;
  my $pfamDB  = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );

  #Find out if family is in rdb
  my $rdb_family = $pfamDB->getPfamData($family);
  my %ignore;

  #Need to populate the ignore hash with clan and nesting data......

  my $overlaps =
    &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \%ignore, undef,
    $pfamDB, $famObj );
  if ($overlaps) {
    print "$0: Looks like your family contains overlaps.\n";
    exit(1);
  }
  $verbose and print STDERR "$0: No overlaps found\n";
  
}

Bio::Pfam::PfamQC::checkDESCSpell( $family, $familyIO );

unless ( Bio::Pfam::PfamQC::sequenceChecker( $family, $famObj ) ) {
  print "$0: $family contains errors.  You should rebuild this family.\n";
  exit(1);
}

$verbose and  print STDERR "Looks like you sequences will be okay\n";

#pqc-check $family
unless ( Bio::Pfam::PfamQC::noFragsInSeed( $family, $famObj ) ) {
  exit(1);
}

$verbose and print STDERR "$0: Did not find any fragment in the SEED\n";

unless ( Bio::Pfam::PfamQC::nonRaggedSeed( $family, $famObj ) ) {
  exit;
}

$verbose and print STDERR "$0: SEED does not apeear to be ragged\n";

#NEED TO CHECK THAT ASSURTIONS COVER ALL FORMAT CHECKS.....
unless ( Bio::Pfam::PfamQC::passesAllFormatChecks( $famObj, $family ) ) {
  exit(1);
}


sub help {

print<<EOF;

usage: $0 [options] <PFAM ACCESSION>

Performs all QC checks on the family.

-help    : print this help message
-verbose : print extra statements as to what checks have been passeed.

EOF
 

exit;
  
}
