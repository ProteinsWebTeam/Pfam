#!/usr/bin/env perl
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

#------------------------------------------------------------------------------------
#Check user has filled in all the fields in DESC file
#Could do this bit just for new families, but doing it for all families just in case
my $user_fields;
if($famObj->DESC->ID eq "ShortName") {
  print STDERR "Need to change the id in the DESC file (currently 'ShortName')\n";
  $user_fields=1;
} 
if($famObj->DESC->DE eq "Family description") {
  print STDERR "Need to change the description in the DESC file (currently 'Family description')\n";
  $user_fields=1;
} 
if($famObj->DESC->AU eq "Who RU") {
  print STDERR "Need to change the author name in the DESC file (currently 'Who RU')\n";
  $user_fields=1;
}
if($famObj->DESC->SE eq "Where did the seed come from") {
  print STDERR "Need to change the seed source in the DESC file (currently 'Where did the seed come from')\n";
  $user_fields=1;
}
print "\n" if($user_fields);


#-------------------------------------------------------------------------------
#Check DESC file for ID/AC and that if we have a CL line that we really meant it

my ($isNew, $svnFamObj);
unless ( $famObj->DESC->ID  and $famObj->DESC->ID =~ /\S+/   ) {
  die
"$0: Your family does not appear have an identifier!  The name of the family is now "
    . "supplied in the DESC file and families are stored under their accession\n."
    . "The check-in process will automatically assign the accession and position it in the
  repository for you!\n";

}

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

unless ( $famObj->DESC->ID  and $famObj->DESC->ID =~ /\S+/   ) {
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
if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) {
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
  
  my $signal_peptide_overlap = &Bio::Pfam::PfamQC::family_overlaps_with_signal_peptide($family, $famObj, $pfamDB);
  if(defined($signal_peptide_overlap) and exists($signal_peptide_overlap->{total}) and $signal_peptide_overlap->{total}>0) {
    print "$0: There are $signal_peptide_overlap->{total} signal peptide overlaps, $signal_peptide_overlap->{seed} in SEED and $signal_peptide_overlap->{align} in ALIGN\n";
    exit(1);
  }
  
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

unless($isNew){
  unless(Bio::Pfam::PfamQC::noMissing($famObj, $svnFamObj, $family )){
    print STDERR "$0: It appears you are missing sequences compared to the SVN copy\n";  
  }
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
