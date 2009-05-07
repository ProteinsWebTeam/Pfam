#!/usr/local/bin/perl

use strict;
use warnings;

#
# This script allows one to check in a family to the SVN repository containing Pfam families.
# The family that you wish to add to the database should be passed in as a parameter
#

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, $ignore, $addToClan, $help );

&GetOptions(
  "m=s"              => \$message,
  "i"                => \$ignore,
  "add_to_clan"      => \$addToClan,
  "help"             => \$help
);

my $family = shift;
chomp($family);

unless ($family) {
  warn "\n***** No family passed  *****\n\n";
  help();
}

if (@ARGV) {
  warn "\n***** $0 no longer supports multiple family check-ins *****\n\n";
  help();
}

help() if ($help);

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if ( !( -d "$pwd/$family" ) ) {
  die
"$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if ( !-w "$pwd/$family" ) {
  die
    "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

#-------------------------------------------------------------------------------
# If a message is supplied, then write it to file such that the code reference
# that deals with the SVN log message can grab it.

if ( -s ".defaultpfnew" ) {
  unlink(".defaultpfnew")
    or die "Could not remove old default check-in message\n";
}

#-------------------------------------------------------------------------------
#Initial SVN stuff

my $config = Bio::Pfam::Config->new;
#Check that family exists in svn
my $client = Bio::Pfam::SVN::Client->new;


#-------------------------------------------------------------------------------
#First QC step is to check the timestamps on the files

if ( !Bio::Pfam::PfamQC::checkFamilyFiles($family) ) {
  print "pfnew: $family contains errors.  You should rebuild this family.\n";
  exit(1);
}

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware

my $familyIO = Bio::Pfam::FamilyIO->new;
my $newFamObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded $family through middleware\n";

#-------------------------------------------------------------------------------
#Check DESC file for ID/AC and that if we have a CL line that we really meant it

if($newFamObj->DESC->AC){
  die "Your family appears to have an accession, but you are using pfnew! Either remove and ".
  "let the database automatically assign the accession or use pfci\n";
}

unless($newFamObj->DESC->ID){
  die "Your family does not appear have an identifier!  The name of the family is now ".
  "supplied in the DESC file and families are stored under their accession\n.". 
  "The check-in process will automatically assign the accession and position it in the
  repository for you!\n";
}

if($addToClan){
  unless($newFamObj->DESC->CL){
    die "You need to add the clan accession to the DESC file to add it to the clan\n";  
  }
#TODO - Remove this die line when add to clan is working!
  die "Not supported at the moment......\n";
}else{
  if($newFamObj->DESC->CL){
    die "Found a clan cross-reference in the DESC file, but you have not asked ".
    "for this new family to be added to the clan.\n  If you want to add it to the clan, ".
    "please use the appropriate flag (-add_to_clan) or remove the CL line!\n"  
  }  
}

#-------------------------------------------------------------------------------
#Check that the pending family does ot already exist. Parnoid check as new families
#should be removed immediately into the main respository.

$client->checkNewFamilyDoesNotExists($newFamObj->DESC->ID);

#-------------------------------------------------------------------------------

#These are more sanity checks
unless ($ignore) {

  #If we are at sanger, perform an overlap check against the database.
  if ( $config->location eq "WTSI" ) {
    my $connect = $config->pfamlive;
    my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );

    #Find out if family is in rdb
    my $rdb_family = $pfamDB->getPfamData($family); 
    my %ignore;
    #Need to populate the ignore hash with clan and nesting data......
    
    my $overlaps =
      &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \%ignore, undef,
      $pfamDB, $newFamObj );
    if ($overlaps) {
      print "Looks like your family contains overlaps.\n";
      exit(1);
    }
  }

  unless ( Bio::Pfam::PfamQC::sequenceChecker( $family, $newFamObj ) ) {
    print "pfnew: $family contains errors.  You should rebuild this family.\n";
    exit(1);
  }

  #pqc-check $family
  unless ( Bio::Pfam::PfamQC::noFragsInSeed( $family, $newFamObj ) ) {
    exit(1);
  }

  unless ( Bio::Pfam::PfamQC::nonRaggedSeed( $family, $newFamObj ) ) {
    exit;
  }
}

#NEED TO CHECK THAT ASSURTIONS COVER ALL FORMAT CHECKS.....
unless ( Bio::Pfam::PfamQC::passesAllFormatChecks( $newFamObj, $family ) ) {
  exit(1);
}

#Automatically write the 'new' message and add it the binding.
open(M, ">.defaultpfnew") or die "Could not open .defaultpfnew:[$!]\n";
print M $newFamObj->DESC->ID." deposited\n";
close M;
$client->addPFNEWLog();
   
#-------------------------------------------------------------------------------
#If we get here, then great! We can now add the family!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!
 
$client->addFamily($family, $newFamObj->DESC->ID);
#$client->commitFamily($family, $newFamilyObj->DESC->ID);
#Remove any file containing the check-in message
if ( -s ".defaultpfnew" ) {
  unlink(".defaultpfnew")
    or die "Could not remove old default check-in message\n";
}

#
if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}

#TODO - Usability issues....
#It may be nice to report the accession of the new family.....


exit(0);
