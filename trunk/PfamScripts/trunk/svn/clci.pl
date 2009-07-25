#!/software/bin/perl
#
# This script allows one to check in a clan in to the SVN repository.
# The clan that you wish to check in should be passed in as a parameter
#

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

#-------------------------------------------------------------------------------
# Deal with all of the options

my ( $message, $help );

&GetOptions(
  "m=s"  => \$message,
  "help" => \$help
);

my $clan = shift;
unless ($clan) {
  warn "\n***** No family passed  *****\n\n";
  help();
}
chomp($clan);

if (@ARGV) {
  warn "\n***** $0 no longer supports multiple family check-ins *****\n\n";
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

if ( -s ".defaultclci" ) {
  unlink(".defaultclci")
    or die "Could not remove old default check-in message\n";
}

if ($message) {
  open( M, ">.defaultclci" ) or die "Could not open message file\n";
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
#&Bio::Pfam::PfamQC::checkClanMembership( $clanObj, $clanSVNObj );



 $client->addCLCILog();  

#-------------------------------------------------------------------------------
#If we get here, then great! We can now check the family in!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->commitClan($clan);

#Remove any file containing the check-in message
if ( -s ".defaultclci" ) {
  unlink(".defaultclci")
    or die "Could not remove old default check-in message\n";
}

#
if ($caught_cntrl_c) {
  print STDERR
"\n** You hit cntrl-c while the operation was in progress.\n** The script has tried to ignore this and recover\n** but this could be very bad.  You really must tell someone about this!\n";
}
exit(0);

#-------------------------------------------------------------------------------
# SUBROUTINES ------------------------------------------------------------------
#-------------------------------------------------------------------------------
