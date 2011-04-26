#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Dfam::LiveDBManager;
use Bio::Dfam::FamilyIO;
use Bio::Dfam::QC;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, $ignore, $addToClan, $help );

&GetOptions(
  "m=s"         => \$message,
  "i"           => \$ignore,
  "help"        => \$help
) or die "Error fetching options\n";

my $family = shift;

unless ($family) {
  warn "\n***** No family passed  *****\n\n";
  help();
}
chomp($family);

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

my $config = Bio::Dfam::Config->new;

#-------------------------------------------------------------------------------
#First QC step is to check the timestamps on the files

#if ( !Bio::Dfam::QC::checkFamilyFiles($family) ) {
#  print "$0: $family contains errors.  You should rebuild this family.\n";
#  exit(1);
#}

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware

my $familyIO = Bio::Dfam::FamilyIO->new;
my $newFamObj = $familyIO->loadDfamFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded $family through middleware\n";
print $newFamObj;
my $dfamDB = Bio::Dfam::LiveDBManager->new;
$dfamDB->createDfam($newFamObj, 'finnr');
if ( $newFamObj->DESC->WIKI ) {
  $dfamDB->updateDfamWikipedia($newFamObj);
}
if ( $newFamObj->DESC->REFS ) {
  $dfamDB->updateDfamLitRefs($newFamObj);
}

if ( $newFamObj->DESC->DBREFS ) {
  $dfamDB->updateDfamDbXrefs($newFamObj);
}
$dfamDB->updateDfamRegSeed($newFamObj);
$dfamDB->updateDfamRegFull($newFamObj);

exit;
