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

my ( $message, $ignore, $onlydesc, $help );

&GetOptions(
  "m=s"              => \$message,
  "i"                => \$ignore,
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
my $familyIO = Bio::Rfam::FamilyIO->new;

my ( $oldFamilyObj, $upFamilyObj );



$upFamilyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded local copy $family through middleware\n";

#Split QC into two parts. Basic new familiy checks, such as format
#sequences and overlap.

unless($onlydesc){
  #TODO - put in lots ot QC here.
  if ( !Bio::Rfam::QC::checkFamilyFiles($family, $upFamilyObj) ) {
    print "$0: $family contains errors.  You should rebuild this model.\n";
    exit(1);
  } 
}

$oldFamilyObj = $familyIO->loadRfamFromSVN( $family, $client );
print STDERR "Successfully loaded SVN copy of $family through middleware\n";


#Add secondary checks here, comparing old and new families.

#-------------------------------------------------------------------------------
#Add the appropriate callback to the client.
$client->addRFCILog();


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

if ($onlydesc) {

  #Check that none of the lines that should not be touch are not

  #The list of fields  that cannot be alter are:
  # NC, TC, GA,
  foreach my $tag (qw(NC TC GA)) {
    my $ftag = 'CUT'.$tag;
    unless ( $oldFamilyObj->DESC->$ftag eq $upFamilyObj->DESC->$ftag )
    {
      die
"There is a differnce in your $tag lines between what is in the SVN repository and this local copy.".
        " You can not do this when only commint a DESC file!\n";
    }
  }

  #ID, AC, PI, SE, SS, BM, SM
  foreach my $tag (qw(ID AC PI SE BM SM FR)) {
    next unless(defined($oldFamilyObj->DESC->$tag));
    unless ( $oldFamilyObj->DESC->$tag eq $upFamilyObj->DESC->$tag ) {
      die
"You are only checking in the DESC file, yet the $tag line has change. You can not do this!\n";
    }
  }
}

#These are more sanity checks
unless ($ignore) {

#Regardless of whether we are just checking in a DESC or the complete set of family files.
  #TODO - bring this online.
  #Bio::Rfam::QC::checkDESCSpell( $family, $familyIO );

  unless ($onlydesc) {

    #TODO - Add QC here
    
    #unless ( Bio::Rfam::QC::sequenceChecker( $family, $upFamilyObj ) ) {
    #  print "%0: $family contains errors.  You should rebuild this family.\n";
    #  exit(1);
    #}

#TOD - Add QC in here!
#    unless ( Bio::Rfam::RfamQC::noMissing( $upFamilyObj, $oldFamilyObj, $family ) ) {
#      print(
#"$0: your family seems to be missing members compared to the DBN copy\n(see $family/missing). Please inspect loss of members.\n"
#      );
#      print("Do you want to continue regardless? [y/n]  ");
#      my $reply = <STDIN>;
#      chomp $reply;
#      if ( $reply ne "y" ) {
#        exit(1);
#      }
#    }
#MORE QC TO GO IN HERE

#    
#    unless ( Bio::Rfam::QC::passesAllFormatChecks( $upFamilyObj, $family ) ) {
#      exit(1);
#    }
  }
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

  usage: $0 <directory>
  
  Where the directory contains the files that consitute a Rfam entry.
  
  Aim: To perform quality control checks on an existing family and commit to the SVN repository.
  
  -onlydesc         - Speeds up check-ins a bit by avoiding the QC and onyl updating the contents
                      in the DESC file.  Do not change DESC contents that affect the other files,
                      particularly cut-offs.  Useful for when making updates to annotation or 
                      adding a family to a clan. If you are uncertain of what you are doing, do 
                      not use this option.
  -i                - Ignore some of the QC steps to speed up check-in.
  -m                - Specify the message that describes the changes you have made to this family 
                      on the command line, avoid being prompted for it at a later satge.                   
                      
EOF

exit(1);

}
