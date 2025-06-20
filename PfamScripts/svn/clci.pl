#!/usr/bin/env perl
#
# This script allows one to check in a clan in to the SVN repository.
# The clan that you wish to check in should be passed in as a parameter
#

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::SVN::Client;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, $help );

&GetOptions(
  "m=s"  => \$message,
  "help" => \$help
) or die "Problem with options passed in, exiting.\n";

my $clan = shift;
unless ($clan) {
  warn "\n***** No clan passed  *****\n\n";
  help();
}
chomp($clan);

if($clan =~ /(\S+)\/$/) { #Remove trailing '/' if present
  $clan = $1; 
}

if (@ARGV) {
  warn "\n***** $0 no longer supports multiple clan check-ins *****\n\n";
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
  die "$0: I can't write to directory [$pwd/$clan].  Check the permissions.\n";
}

if ( !( -d "$pwd/$clan/.svn" ) ) {
  die
"$0: [$pwd/$clan] does not look like that it is from the Pfam subversion repository";
}

#-------------------------------------------------------------------------------
# If a message is supplied, then write it to file such that the code reference
# that deals with the SVN log message can grab it.

if ( -s ".default".$$."clci" ) {
  unlink(".default".$$."clci")
    or die "Could not remove old default check-in message\n";
}

if ($message) {
  open( M, ">.default".$$."clci" ) or die "Could not open message file\n";
  print M $message;
  close(M);
}

#-------------------------------------------------------------------------------
#Initial SVN stuff

#Check that family exists in svn

my $config = Bio::Pfam::Config->new;
my $client = Bio::Pfam::SVN::Client->new;
$client->checkClanExists($clan);

#Need to check that the family is up-to-date

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware
my $clanIO = Bio::Pfam::ClanIO->new;

# Check spelling!
&Bio::Pfam::PfamQC::checkCLANDESCSpell( $clan, $clanIO );

# Clan QC - need to make sue the clan membership has not been altered.
my $clanObj = $clanIO->loadClanFromLocalFile( $clan, $pwd, 'file' );
my $clanSVNObj = $clanIO->loadClanFromSVN( $clan, $client );

#Now make sure that the clan membership is unaltered.
unless( &Bio::Pfam::PfamQC::checkClanMembership( $clanObj->DESC->MEMB, $clanSVNObj->DESC->MEMB )){
  print STDERR "Miss-match between the clan membership! You can not modify the MB lines\n";
  exit(1);  
}

#Make sure that the clan accession and id are the same between the two versions.
unless($clanObj->DESC->AC eq $clanSVNObj->DESC->AC){
  die "The clan accession has been changed between the SVN copy and your local copy!:".
    "From:".$clanSVNObj->DESC->AC." to ".$clanObj->DESC->AC."\n";
}

unless($clanObj->DESC->ID eq $clanSVNObj->DESC->ID){
  die "The clan accession has been changed between the SVN copy and your local copy!:".
    "From:".$clanSVNObj->DESC->ID." to ".$clanObj->DESC->ID.".  Use clmove.pl to change the name\n";
}

$client->addCLCILog();  
 
#-------------------------------------------------------------------------------
#If we get here, then great! We can now check the family in!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->commitClan($clan);

# submit to apicuron
my $api_run = `submit_apicuron.pl $clan update_clan`;
if ($api_run ne 'Success') {
  print "Could not submit curation event to apicuron...\n$api_run\n";
}

#Remove any file containing the check-in message
if ( -s ".default".$$."clci" ) {
  unlink(".default".$$."clci")
    or die "Could not remove old default check-in message\n";
}

#
if ($caught_cntrl_c) {
  print STDERR
  "\n** You hit cntrl-c while the operation was in progress.\n**". 
  "The script has tried to ignore this and recover\n**".
  "but this could be very bad.  You really must tell someone about this!\n";
}
exit(0);

sub help {

print<<EOF;

  usage: $0 <directory>

  Where the directory contains the files that constitute a Pfam Clan.

  Aim: To perform quality control checks on an existing clan and commit to the SVN repository.

  -m       - Specify the message that describes the changes you have made to this clan
             on the command line, avoid being prompted for it at a later stage.
  -help    - Prints this message

EOF

exit(1);

}