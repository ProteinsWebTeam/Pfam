#!/usr/bin/env perl
#
# This script should not be used unless something wrong happened when pfnew.
# If on pfnew a family failed to commit to svn but was added to the database,
# this script allows the killing of that family (database-only removal of family)
#

use strict;
use warnings;

use Cwd;
use Data::Dumper;
use Getopt::Long;
use File::Temp;
use Try::Tiny;

use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

#-------------------------------------------------------------------------------
# Deal with all of the options

my $user = $ENV{USER};
my ( $family, $help );

&GetOptions(
  "f=s"              => \$family,
  "help"             => \$help
) or die "Unrecognised option passed in to the script.\n";

help() if ($help);

unless ($family) {
  warn "\n***** No family provided  *****\n\n";
  help();
}
chomp($family);

if($family =~ /(\S+)\/$/) { #Remove trailing '/' if present
  $family = $1;
}

if (@ARGV) {
  warn "\n***** $0 unexpected arguments *****\n\n";
  help();
}


my $config = Bio::Pfam::Config->new;


# Connecting to pfam_live database
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh    = $pfamDB->getSchema->storage->dbh;

my $pwd = getcwd;

if ( !( -d "$pwd/$family" ) ) {
  die "$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if ( !-w "$pwd/$family" ) {
  die "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

my $db = $config->pfamlive->{database};
print "Going to update $family on $db...\n";

my $familyIO = Bio::Pfam::FamilyIO->new;
my $famObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );

my $fam_clan = $famObj->DESC->CL // '';
my $fam_clan_db = $pfamDB->getClanDataByPfam($family) // '';

if ($fam_clan_db) {
  $fam_clan_db = $fam_clan_db->clan_acc->clan_acc;
}



# $famObj->seedcheck('ignore'); #Set to ignore so this field doesn't get updated
Bio::Pfam::PfamQC::sequenceChecker( $family, $famObj, $pfamDB, 1 );

# try {
  print "Doing updatePfamAInRDB\n";
  $familyIO->updatePfamAInRDB($famObj, $pfamDB, 0);

  print "Doing updatePfamARegions\n";
  $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
  $familyIO->updatePfamARegions($famObj, $pfamDB);

  print "Doing uploadPfamAHMM\n";
  $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
  $familyIO->uploadPfamAHMM($famObj, $pfamDB, $pwd, 0);

  print "Doing uploadPfamAAligns\n";
  $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
  $familyIO->uploadPfamAAligns($famObj, $pfamDB, $pwd, 0);

  print "Doing create_or_update_author\n";
  $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
  $familyIO->create_or_update_author($pfamDB, $famObj);

  if ($fam_clan ne $fam_clan_db) {
    print "Updating clan membership... \n";
    if ($fam_clan_db) {
      print "   remove $fam_clan_db clan membership\n";
      $pfamDB->removeFamilyFromClanMembership($fam_clan_db, $family);
    }
    if ($fam_clan) {
      print "   add $fam_clan clan membership\n";
      $pfamDB->updateClanMembership($fam_clan, $family);
    }
  }

  $pfamDB->getSchema->resultset('AlignmentAndTree')->search({
      pfama_acc => $family
    })->delete;





# } catch {
#   die "Could not update $family on the database.\nERROR: $!";
# };




print "Done.\n";




sub help {

print<<EOF;

  usage: $0 -f <PfamA_acc>

  Aim: Update locally on the database the HMM, regions and aligns.

  
  -f    - Family accession to remove from the database.

EOF

exit(1);

}
