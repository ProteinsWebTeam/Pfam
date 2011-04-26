#!/usr/bin/env perl
#
# dfnew.pl - This script allows one to check in a family to the SVN repository
# containing Dfam families. The family that you wish to add to the database
# should be passed in as a parameter.
#
#
# Author:        rdf
# Maintainer:    $Author$
# Version:       $Revision$
# Created:       Feb 18, 2010
# Last Modified: $Date$

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Dfam::SVN::Client;
use Bio::Dfam::DBManager;
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

#-------------------------------------------------------------------------------
# If a message is supplied, then write it to file such that the code reference
# that deals with the SVN log message can grab it.

if ( -s ".default" . $$ . "dfnew" ) {
  unlink( ".default" . $$ . "dfnew" )
    or die "Could not remove old default check-in message\n";
}

#-------------------------------------------------------------------------------
#Initial SVN stuff

my $config = Bio::Dfam::Config->new;

#Check that family exists in svn
my $client = Bio::Dfam::SVN::Client->new;

#-------------------------------------------------------------------------------
#First QC step is to check the timestamps on the files

if ( !Bio::Dfam::QC::checkFamilyFiles($family) ) {
  print "$0: $family contains errors.  You should rebuild this family.\n";
  exit(1);
}

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware

my $familyIO = Bio::Dfam::FamilyIO->new;
my $newFamObj = $familyIO->loadDfamFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded $family through middleware\n";

#-------------------------------------------------------------------------------
#Check DESC file for ID/AC 

if ( $newFamObj->DESC->AC ) {
  die
"Your family appears to have an accession, but you are using dfnew! Either remove and "
    . "let the database automatically assign the accession or use dfci\n";
}

unless ( $newFamObj->DESC->ID ) {
  die
"Your family does not appear have an identifier!  The name of the family is now "
    . "supplied in the DESC file and families are stored under their accession\n."
    . "The check-in process will automatically assign the accession and position it in the
  repository for you!\n";
}

#-------------------------------------------------------------------------------
#Check that the pending family does ot already exist. Parnoid check as new families
#should be removed immediately into the main respository.

$client->checkNewFamilyDoesNotExists( $newFamObj->DESC->ID );

#-------------------------------------------------------------------------------

#These are more sanity checks
unless ($ignore) {

  #If we are at sanger, perform an overlap check against the database.
  if ( $config->location eq "JFRC" ) {
    my $connect = $config->dfamlive;
    my $dfamDB  = Bio::Dfam::LiveDBManager->new( %{$connect} );

    #Find out if family is in rdb
    my $rdb_family = $dfamDB->getDfamData($family);
    my %ignore;

    #Need to populate the ignore hash with clan and nesting data......

    my $overlaps =
      &Bio::Dfam::QC::family_overlaps_with_db( $family, \%ignore, undef,
      $dfamDB, $newFamObj );
    if ($overlaps) {
      print "Looks like your family contains overlaps.\n";
      exit(1);
    }
  }

  Bio::Dfam::QC::checkDESCSpell( $family, $familyIO );

  unless ( Bio::Dfam::QC::sequenceChecker( $family, $newFamObj ) ) {
    print "dfnew: $family contains errors.  You should rebuild this family.\n";
    exit(1);
  }

  #pqc-check $family
  unless ( Bio::Dfam::QC::noFragsInSeed( $family, $newFamObj ) ) {
    exit(1);
  }

  unless ( Bio::Dfam::QC::nonRaggedSeed( $family, $newFamObj ) ) {
    exit;
  }
}

#NEED TO CHECK THAT ASSURTIONS COVER ALL FORMAT CHECKS.....
unless ( Bio::Dfam::QC::passesAllFormatChecks( $newFamObj, $family ) ) {
  exit(1);
}

#Automatically write the 'new' message and add it the binding.
open( M, ">.default" . $$ . "dfnew" )
  or die "Could not open .default" . $$ . "dfnew:[$!]\n";
print M $newFamObj->DESC->ID . " deposited\n";
close M;
$client->addDFNEWLog();

#-------------------------------------------------------------------------------
#If we get here, then great! We can now add the family!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->addFamily( $family, $newFamObj->DESC->ID );

#Remove any file containing the check-in message
if ( -s ".default" . $$ . "dfnew" ) {
  unlink( ".default" . $$ . "dfnew" )
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

if ( $config->location eq "JFRC" ) {
  my $connect   = $config->pdfamlive;
  my $dfamDB    = Bio::Dfam::LiveDBManager->new( %{$connect} );
  my $dfamEntry = $dfamDB->getDfamData( $newFamObj->DESC->ID );
  print STDERR "This family has been asssigned the accession:"
    . $dfamEntry->dfam_acc . "\n"
    if ( $dfamEntry->dfam_acc );
}

exit(0);

sub help {
  
print<<EOF;

  usage: $0 <directory>
  
  Where the directory contains the files that consitute a Dfam entry.
  
  Aim: To perform quality control checks on a new family and add it to the SVN repository.
  
  -i                - Ignore some of the QC steps to speed up check-in.
  -m                - Specify the message that describes the changes you have made to this family 
                      on the command line, avoid being prompted for it at a later satge.                   
                      
EOF

exit(1);

}


=head1 NAME

 pdfnew.pl <dir> - add new families to dfam

=cut

=head1 DESCRIPTION


 This script allows one to check in a family to the SVN repository containing Dfam families.
 The family that you wish to add to the database should be passed in as a parameter


$Author$

=head1 COPYRIGHT

File: dfnew.pl

=cut
