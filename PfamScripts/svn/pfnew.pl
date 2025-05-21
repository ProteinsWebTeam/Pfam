#!/usr/bin/env perl
#
# pfnew.pl - This script allows one to check in a family to the SVN repository
# containing Pfam families. The family that you wish to add to the database
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

use Bio::Pfam::SVN::Client;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, $ignore, $addToClan, $help );

&GetOptions(
  "m=s"         => \$message,
  "i"           => \$ignore,
  "add_to_clan" => \$addToClan,
  "help"        => \$help
) or die "Error fetching options\n";

help() if ($help);

my $family = shift;
chomp($family);

unless ($family) {
  warn "\n***** No family passed  *****\n\n";
  help();
}

if($family =~ /(\S+)\/$/) { #Remove trailing '/' if present
  $family = $1;
}

if (@ARGV) {
  warn "\n***** $0 no longer supports multiple family check-ins *****\n\n";
  help();
}


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

if ( -s ".default" . $$ . "pfnew" ) {
  unlink( ".default" . $$ . "pfnew" )
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


#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware

my $familyIO = Bio::Pfam::FamilyIO->new;
my $newFamObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );
print STDERR "Successfully loaded $family through middleware\n";


#-------------------------------------------------------------------------------
#Check ALIGN files has size
#If not, ask user to confirm they still want to check it in
unless(-s "$family/ALIGN") {
  print "$0: Your family has an empty ALIGN file?\n";
  print "Do you still want to check it in? [y/n]  ";
  my $reply = <STDIN>;
  chomp $reply;
  if ( $reply ne "y" ) { 
    exit(1);
  }   
}  

#------------------------------------------------------------------------------------
#Check user has filled in all the fields in DESC file
if($newFamObj->DESC->ID eq "ShortName") {
  die "Need to change the id in the DESC file (currently 'ShortName')\n";
} 
if($newFamObj->DESC->DE eq "Family description") {
  die "Need to change the description in the DESC file (currently 'Family description')\n";
} 
if($newFamObj->DESC->AU eq "Who RU") {
  die "Need to change the author name in the DESC file (currently 'Who RU')\n";
}
if($newFamObj->DESC->SE eq "Where did the seed come from") {
  die "Need to change the seed source in the DESC file (currently 'Where did the seed come from')\n";
}


#-------------------------------------------------------------------------------
#Check DESC file for ID/AC and that if we have a CL line that we really meant it

if ( $newFamObj->DESC->AC ) {
  die
"Your family appears to have an accession, but you are using pfnew! Either remove and "
    . "let the database automatically assign the accession or use pfci\n";
}

unless ( $newFamObj->DESC->ID ) {
  die
"Your family does not appear have an identifier!  The name of the family is now "
    . "supplied in the DESC file and families are stored under their accession\n."
    . "The check-in process will automatically assign the accession and position it in the
  repository for you!\n";
}

if ($addToClan) {
  unless ( $newFamObj->DESC->CL ) {
    die
"You need to add the clan accession to the DESC file to add it to the clan\n";
  }
}
else {
  if ( $newFamObj->DESC->CL ) {
    die "Found a clan cross-reference in the DESC file, but you have not asked "
      . "for this new family to be added to the clan.\n  If you want to add it to the clan, "
      . "please use the appropriate flag (-add_to_clan) or remove the CL line!\n";
  }
}

#-------------------------------------------------------------------------------
#Check that the pending family does ot already exist. Parnoid check as new families
#should be removed immediately into the main respository.

$client->checkNewFamilyDoesNotExists( $newFamObj->DESC->ID );

#-------------------------------------------------------------------------------

#Get pfamDB object, use this for various qc checks
my $pfamDB; 
if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) { 
  my $connect = $config->pfamlive;
  $pfamDB  = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );
}

#NEED TO CHECK THAT ASSURTIONS COVER ALL FORMAT CHECKS.....
unless ( Bio::Pfam::PfamQC::passesAllFormatChecks( $newFamObj, $family, undef, undef, $pfamDB ) ) {
  exit(1);
}

#These are more sanity checks
unless ($ignore) {
  
  #If we are at sanger, perform an overlap check against the database.
  if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) {

    #Find out if family is in rdb
    my $rdb_family = $pfamDB->getPfamData($family);

    unless ( Bio::Pfam::PfamQC::sequenceChecker( $family, $newFamObj, $pfamDB ) ) {
      print "pfnew: $family contains errors.  You should rebuild this family.\n";
      exit(1);
    }

    #Need to populate the ignore hash with clan and nesting data......
    my %ignore;

    my $overlaps =
      &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \%ignore, $pfamDB, $newFamObj, undef, undef); 

    if ($overlaps) {
      print "Looks like your family contains overlaps.\n";
      exit(1);
    }

    #Check if the pfamA id is already being used
    my $row = $pfamDB->getSchema->resultset('PfamA')->find({ pfamA_id => $newFamObj->DESC->ID });
    if($row) {
      print "pfnew: Unable to commit your family as there is already a family in the database called ".$newFamObj->DESC->ID." (".$row->pfama_acc.")\n";
      exit;
    }
  }

  Bio::Pfam::PfamQC::checkDESCSpell( $family, $familyIO );


  #pqc-check $family
  unless ( Bio::Pfam::PfamQC::noFragsInSeed( $family, $newFamObj ) ) {
    exit(1);
  }

  if ( Bio::Pfam::PfamQC::nonRaggedSeed( $family, $newFamObj)){
      print "SEED alignment is not ragged\n";
  } else {
      print "SEED alignment is ragged\n";
  }

}


#Automatically write the 'new' message and add it the binding.
open( M, ">.default" . $$ . "pfnew" )
  or die "Could not open .default" . $$ . "pfnew:[$!]\n";
print M $newFamObj->DESC->ID . " deposited\n";
if ($message) {
  print M $message;
}
close M;
$client->addPFNEWLog();

#-------------------------------------------------------------------------------
#If we get here, then great! We can now add the family!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->addFamily( $family, $newFamObj->DESC->ID );

# submit to apicuron
my $api_run = `perl submit_apicuron.pl $family create_family`;
if ($api_run ne 'Success') {
  print "Could not submit curation event to apicuron...\n$api_run\n";
}

#Remove any file containing the check-in message
if ( -s ".default" . $$ . "pfnew" ) {
  unlink( ".default" . $$ . "pfnew" )
    or die "Could not remove old default check-in message\n";
}

#Now update the DESC file!
#$client->update($family); - this does not work as it has already moved in the repos!

#
if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}


#It may be nice to report the accession of the new family.....This will only
#work at WTSI/EBI, as these are the only places where the database sits. Could replace
#with a webservice!

if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) {
  my $connect   = $config->pfamlive;
  my $pfamDB    = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );
  my $pfamEntry = $pfamDB->getPfamData( $newFamObj->DESC->ID );
  print STDERR 'This family has been asssigned the accession: '
    . $pfamEntry->pfama_acc . "\n"
    if ( $pfamEntry->pfama_acc );
}

exit(0);

sub help {
  
print<<EOF;

  usage: $0 <directory>
  
  Where the directory contains the files that consitute a Pfam-A entry.
  
  Aim: To perform quality control checks on a new family and add it to the SVN repository.
  
  -add_to_clan      - Add the family to the clan specified on the CL line of the DESC file. This 
                      flag/option only needs to be added the first time the CL line is added.
  -i                - Ignore some of the QC steps to speed up check-in.
  -m                - Specify the message that describes the changes you have made to this family 
                      on the command line, avoid being prompted for it at a later satge.                   
                      
EOF

exit(1);

}


=head1 NAME

 pfnew.pl <dir> - add new families to pfam

=cut

=head1 DESCRIPTION


 This script allows one to check in a family to the SVN repository containing Pfam families.
 The family that you wish to add to the database should be passed in as a parameter


$Author$

=head1 COPYRIGHT

File: pfnew.pl

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut
