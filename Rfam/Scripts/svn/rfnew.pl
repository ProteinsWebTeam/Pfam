#!/usr/bin/env perl
#
# rfnew.pl - This script allows one to check in a family to the SVN repository
# containing rfam families. The family that you wish to add to the database
# should be passed in as a parameter.
#

use strict;
use warnings;
use Cwd;
use Data::Printer;
use Getopt::Long;

use Bio::Rfam::SVN::Client;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::QC;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, @ignore, $addToClan, $help );

&GetOptions(
  "m=s"         => \$message,
  "i=s"         => \@ignore,
  "help"        => \$help,
  "add_to_clan" => \$addToClan,
) or die "Unrecognised option passed in to the script.\n";

my $family = shift;

unless ($family) {
  warn "\n***** No family passed  *****\n\n";
  help();
}
chomp($family);

if (@ARGV) {
  warn "\n***** $0 does not support multiple family check-ins *****\n\n";
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

if ( -s ".default" . $$ . "rfnew" ) {
  unlink( ".default" . $$ . "rfnew" )
    or die "Could not remove old default check-in message\n";
}

#-------------------------------------------------------------------------------
#Initial SVN stuff

my $config = Bio::Rfam::Config->new;

#Check that family exists in svn
my $client = Bio::Rfam::SVN::Client->new;

#-------------------------------------------------------------------------------
#First QC step is to check the timestamps on the files

if ( Bio::Rfam::QC::checkFamilyFiles($family, $config) ) {
  print "$0: $family contains errors.  You should rebuild this family.\n";
  exit(1);
}

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware

my $familyIO = Bio::Rfam::FamilyIO->new;
my $newFamObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded $family through middleware\n";


#-------------------------------------------------------------------------------
#Check DESC file for ID/AC 

if ( $newFamObj->DESC->AC ) {
  die
"Your family appears to have an accession, but you are using rfnew! Either remove and "
    . "let the database automatically assign the accession or use rfci\n";
}

unless ( $newFamObj->DESC->ID ) {
  die "Your family does not appear have an identifier!"
    . "The check-in process will automatically assign the accession "
    . " and position it in the repository for you!\n";
}

#-------------------------------------------------------------------------------
if($newFamObj->DESC->CL){
  #Okay, we have a clan added to the DESC file.
  unless($addToClan){
    die "Found a CL line in the DESC file and you have not explicitly said you".
        " are going to add to a clan. See $0 -help!\n";  
  }
  #Check that the clan accession is valid.
  eval{
    $client->checkClanExists($newFamObj->DESC->CL);  
  };
  if($@){
    die "\nERROR:There was an issue finding the clan referenced in the DESC file, ".
         $newFamObj->DESC->CL."\n\nSee:$@\n";
  }
  #If we get to here, then the clan is okay and the users has appropriately triggered.
}

 exit;

#-------------------------------------------------------------------------------
#Check that the pending model does ot already exist. Parnoid check as new models
#should be removed immediately into the main respository.

$client->checkNewFamilyDoesNotExist($newFamObj->DESC->ID );

#-------------------------------------------------------------------------------
#Perform the QC on the family

my $overrideHashRef = Bio::Rfam::QC::processIgnoreOpt(\@ignore, $config);


my $error = 0;
my $overlapIgnore = {};

#Okay, this a full check-in, perform whole QC repetoire.
$error = Bio::Rfam::QC::essential($newFamObj, "$pwd/$family", undef, $config);
  die "Failed essential QC step.\n" if($error);
$error = Bio::Rfam::QC::optional( $newFamObj, "$pwd/$family", undef, 
                                    $config, $overrideHashRef, $overlapIgnore );
die "Failed QC step.\n" if($error);


#Automatically write the 'new' message and add it the binding.
open( M, ">.default" . $$ . "rfnew" )
  or die "Could not open .default" . $$ . "rfnew:[$!]\n";
print M $newFamObj->DESC->ID . " deposited\n";
close M;
$client->addRFNEWLog();

#-------------------------------------------------------------------------------
#If we get here, then great! We can now add the family!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->addFamily( $family, $newFamObj->DESC->ID );

#Remove any file containing the check-in message
if ( -s ".default" . $$ . "rfnew" ) {
  unlink( ".default" . $$ . "rfnew" )
    or die "Could not remove old default check-in message\n";
}

#
if ($caught_cntrl_c) {
  print STDERR "\n** You hit cntrl-c while the operation was in progress.".
               "\n** The script has tried to ignore this and recover".
               "\n** but this could be very bad.  You really must tell ".
               "\n** someone about this so they can check everything is okay!\n";
}


#It may be nice to report the accession of the new family.....
#This will only work where the database is based, as this is the only place 
#where the database sits. Could replace with a webservice!

if ( $config->location eq "EBI" ) {
  my $rfamdb   = $config->rfamlive;
  my $rfamEntry = $rfamdb->resultset('Family')->find({rfam_id => $newFamObj->DESC->ID });
  print STDERR "This family has been asssigned the accession:"
    . $rfamEntry->rfam_acc . "\n"
    if ( $rfamEntry->rfam_acc );
}

exit(0);

sub help {
  
print<<EOF;

  usage: $0 <directory>
  
  Where the directory contains the files that consitute a Rfam entry.
  
  Aim: To perform quality control checks on a new family and add it to the SVN repository.
  
  -i                - Ignore some of the QC steps to speed up check-in.
  -m                - Specify the message that describes the changes you have made to this family 
                      on the command line, avoid being prompted for it at a later satge.
  -add_to_clan      - Add this family to a clan.                                       
                      
EOF

exit(1);

}
