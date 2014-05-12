#!/usr/bin/env perl
#
# rmfam_motif_ci.pl - This script allows a motif to be checked into RfamLive.
#

use strict;
use warnings;
use Cwd;
use Data::Dumper;
use Getopt::Long;

use Bio::Rfam::MotifIO;
use Bio::Rfam::QC;
use Bio::Rfam::SVN::Commit;
use RfamLive::ResultSet::Motif;

#------------------------------------------------------------------------------
# Deal with the options
#

my (@ignore,$help);

&GetOptions(
  "i=s"              => \@ignore,
  "help"             => \$help
) or die "Unrecognised option passed in to the script.\n";

my $motif = shift;
unless ($motif) {
  warn "\n***** No motif dir name passed in *****\n\n";
  help();
}
chomp($motif);

help() if ($help);

#----------------------------------------------------------------------------
# Get the working directory and check the motif folder exists
#
my $pwd = getcwd;

if ( !( -d "$pwd/$motif" ) ) {
  die
"$0: [$pwd/$motif] is not a current directory.\nMust be in the parent directory of the model to check in\n";
}

if ( !-w "$pwd/$motif" ) {
  die
    "$0: Can't write to directory [$pwd/$motif].  Check the permissions.\n";
}

#----------------------------------------------------------------------------
# Load the motif from disk
my ( $upMotifObj );
my $motifIO = Bio::Rfam::MotifIO->new;

$upMotifObj = $motifIO->loadRfamMotifFromLocalFile( $motif, $pwd );
print STDERR "Successfully  loaded $motif from local files\n";

#---------------------------------------------------------------------------
# Insert  or updat the motif into the database
my $commit = Bio::Rfam::SVN::Commit->new;
$commit->commitMotif($upMotifObj);

#---------------------------------------------------------------------------


