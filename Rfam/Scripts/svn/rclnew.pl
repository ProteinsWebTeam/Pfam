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
use Bio::Rfam::ClanIO;
use Bio::Rfam::QC;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, $addToClan, $help );

&GetOptions(
  "m=s"         => \$message,
  "help"        => \$help
) or die "Unrecognised option passed in to the script.\n";

my $clan = shift;

unless ($clan) {
  warn "\n***** No clan dir name passed in *****\n\n";
  help();
}
chomp($clan);

if (@ARGV) {
  warn "\n***** $0 does not support multiple family check-ins *****\n\n";
  help();
}

help() if ($help);

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if ( !( -d "$pwd/$clan" ) ) {
  die
"$0: [$pwd/$clan] is not a current directory.\nMust be in the parent directory of the clan to check in\n";
}

if ( !-w "$pwd/$clan" ) {
  die
    "$0: I can't write to directory [$pwd/$clan].  Check the permissions.\n";
}

#-------------------------------------------------------------------------------
# If a message is supplied, then write it to file such that the code reference
# that deals with the SVN log message can grab it.

if ( -s ".default" . $$ . "rclnew" ) {
  unlink( ".default" . $$ . "rclnew" )
    or die "Could not remove old default check-in message\n";
}

#-------------------------------------------------------------------------------
#Initial SVN stuff

my $config = Bio::Rfam::Config->new;

#Check that family exists in svn
my $client = Bio::Rfam::SVN::Client->new;

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware
my ( $newClanObj );
my $clanIO = Bio::Rfam::ClanIO->new;

$newClanObj = $clanIO->loadClanFromLocalFile( $clan, $pwd );
print STDERR "Successfully loaded local copy $clan through middleware\n";


#-------------------------------------------------------------------------------
#Check DESC file for ID/AC 

if ( $newClanObj->DESC->AC ) {
  die
"Your clan appears to have an accession, but you are using $0! Either remove and "
    . "let the database automatically assign the accession or use rclci\n";
}

unless ( $newClanObj->DESC->ID ) {
  die "Your clan does not appear have an identifier!"
    . "The check-in process will automatically assign the accession "
    . " and position it in the repository for you!\n";
}

#-------------------------------------------------------------------------------
#Check that the pending model does ot already exist. Parnoid check as new models
#should be removed immediately into the main respository.

$client->checkNewClanDoesNotExist($newClanObj->DESC->ID );

#-------------------------------------------------------------------------------
#Perform the QC on the clan

#Map the ignore flags into a hash:
my $error = 0;
$error = Bio::Rfam::QC::essentialClan($newClanObj, undef, $config);
die "Failed essential QC step.\n" if($error);

#-------------------------------------------------------------------------------
#Automatically write the 'new' message and add it the binding.
open( M, ">.default" . $$ . "rclnew" )
  or die "Could not open .default" . $$ . "rclnew:[$!]\n";
print M $newClanObj->DESC->ID . " deposited\n";
close M;
$client->addRCLNEWLog();

#-------------------------------------------------------------------------------
#If we get here, then great! We can now add the family!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->addClan( $clan, $newClanObj->DESC->ID );

#Remove any file containing the check-in message
if ( -s ".default" . $$ . "rclnew" ) {
  unlink( ".default" . $$ . "rclnew" )
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
  my $clanEntry = $rfamdb->resultset('Clan')->find({id => $newClanObj->DESC->ID });
  print STDERR "This clan has been asssigned the accession:"
    . $clanEntry->clan_acc . "\n"
    if ( $clanEntry->clan_acc );
}

exit(0);

sub help {
  
print<<EOF;

  usage: $0 <directory>
  
  Where the directory contains the files that consitute a Rfam clan are.
  
  Aim: To perform quality control checks on a new clan and add it to the SVN repository.
  
  -i                - Ignore some of the QC steps to speed up check-in.
  -m                - Specify the message that describes the changes you have made to this family 
                      on the command line, avoid being prompted for it at a later satge.                   
                      
EOF

exit(1);

}
