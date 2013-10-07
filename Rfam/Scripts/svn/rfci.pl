#!/usr/bin/env perl
#
# This script allows one to check in a family to the SVN repository containing Rfam 
# entries. The family that you wish to commit should be passed in as a parameter
#

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Rfam::SVN::Client;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::QC;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, @ignore, $onlydesc, $help );

&GetOptions(
  "m=s"              => \$message,
  "i=s"              => \@ignore,
  "onlydesc"         => \$onlydesc,
  "help"             => \$help
) or die "Unrecognised option passed in to the script.\n";

my $family = shift;
unless ($family) {
  warn "\n***** No family dir name passed in *****\n\n";
  help();
}
chomp($family);

help() if ($help);

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if ( !( -d "$pwd/$family" ) ) {
  die
"$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the model to check in\n";
}

if ( !-w "$pwd/$family" ) {
  die
    "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

if ( !( -d "$pwd/$family/.svn" ) ) {
  die
"$0: [$pwd/$family] does not look like that it is from the Rfam subversion repository";
}

#-------------------------------------------------------------------------------
# If a message is supplied, then write it to file such that the code reference
# that deals with the SVN log message can grab it.

if ( -s ".default" . $$ . "rfci" ) {
  unlink( ".default" . $$ . "rfci" )
    or die "Could not remove old default check-in message\n";
}

if ($message) {
  open( M, ">.default" . $$ . "rfci" ) or die "Could not open message file\n";
  print M $message;
  close(M);
}


#-------------------------------------------------------------------------------
#Initial SVN stuff

#Check that family exists in svn

my $config = Bio::Rfam::Config->new;
my $client = Bio::Rfam::SVN::Client->new({config => $config});
$client->checkFamilyExists($family);

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware
my ( $oldFamilyObj, $upFamilyObj );
my $familyIO = Bio::Rfam::FamilyIO->new;
$upFamilyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded local copy $family through middleware\n";

#Now load the remote family
$oldFamilyObj = $familyIO->loadRfamFromSVN( $family, $client );
print STDERR "Successfully loaded SVN copy of $family through middleware\n";

#-------------------------------------------------------------------------------
#Add the appropriate callback to the client. This adds the message if not supplied.
$client->addRFCILog();

#These two checks are also performed elsewhere, but this message is more informative.
#Check the desc accessions are the same
if ( $upFamilyObj->DESC->AC ne $oldFamilyObj->DESC->AC ) {
  die
"\n***** Accession error, your local copy does not match the SVN repository *****\n\n";
}

#Check the desc accessions are the same
if ( $upFamilyObj->DESC->ID ne $oldFamilyObj->DESC->ID ) {
  die
"\n***** Identifier error, your local copy does not match the SVN repository. "
    . " Do you mean to change the name? If so use rfmove. *****\n\n";
}

#-------------------------------------------------------------------------------
#Perform the QC on the family

my $acc = defined($upFamilyObj->DESC->AC) ? $upFamilyObj->DESC->AC : '';
my $overrideHashRef = Bio::Rfam::QC::processIgnoreOpt(\@ignore, $config, $acc);


my $error = 0;
if ($onlydesc) {
  #If people are only making an annotation change....We just need to check this.
  $error = Bio::Rfam::QC::checkNonFreeText($upFamilyObj, $oldFamilyObj);
  $error = Bio::Rfam::QC::checkDESCFormat( $upFamilyObj ) if(!$error);
}else{
  my $overlapIgnore = {};
  $overlapIgnore->{$acc} = 1;
  #Okay, this a full check-in, perform whole QC repetoire.
  $error = Bio::Rfam::QC::essential($upFamilyObj, "$pwd/$family", $oldFamilyObj, $config);
  $error = Bio::Rfam::QC::optional( $upFamilyObj, "$pwd/$family", $oldFamilyObj, 
                                    $config, $overrideHashRef, $overlapIgnore );
}

#-------------------------------------------------------------------------------
#If we get here, then great! We can now check the family in!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

if ($onlydesc) {
  $client->commitFamilyDESC($family);
}
else {
  $client->commitFamily($family);
}

#------------------------------------------------------------------------------
#Clean-up
#Remove any file containing the check-in message
if ( -s ".default" . $$ . "rfci" ) {
  unlink( ".default" . $$ . "rfci" )
    or die "Could not remove old default check-in message\n";
}

if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}
exit(0);


sub help {
  
print<<EOF;

USAGE: $0 <directory>

  Where the directory contains the files that consitute a Rfam entry.

AIM: To perform quality control checks on an existing family and commit to the SVN repository.
    
OPTIONS:

  -onlydesc         - Speeds up check-ins a bit by avoiding the QC and onyl updating the contents
                      in the DESC file.  Do not change DESC contents that affect the other files,
                      particularly cut-offs.  Useful for when making updates to annotation or 
                      adding a family to a clan. If you are uncertain of what you are doing, do 
                      not use this option.
  -i <option>       - Ignore some of the QC steps to speed up check-in/get family through.
  -m                - Specify the message that describes the changes you have made to this family 
                      on the command line, avoid being prompted for it at a later satge.                   
                      
EOF

exit(1);

}
