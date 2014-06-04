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
use Bio::Rfam::ClanIO;
use Bio::Rfam::QC;

#-------------------------------------------------------------------------------
# Deal with all of the options


my ( $message, $help );

&GetOptions(
  "m=s"              => \$message,
  "help"             => \$help
) or die "Unrecognised option passed in to the script.\n";

my $clan = shift;
unless ($clan) {
  warn "\n***** No clan dir name passed in *****\n\n";
  help();
}
chomp($clan);

help() if ($help);

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if ( !( -d "$pwd/$clan" ) ) {
  die
"$0: [$pwd/$clan] is not a current directory.\nMust be in the parent directory of the model to check in\n";
}

if ( !-w "$pwd/$clan" ) {
  die
    "$0: I can't write to directory [$pwd/$clan].  Check the permissions.\n";
}

if ( !( -d "$pwd/$clan/.svn" ) ) {
  die
"$0: [$pwd/$clan] does not look like that it is from the Rfam subversion repository";
}

#-------------------------------------------------------------------------------
# If a message is supplied, then write it to file such that the code reference
# that deals with the SVN log message can grab it.

if ( -s ".default" . $$ . "clci" ) {
  unlink( ".default" . $$ . "clci" )
    or die "Could not remove old default check-in message\n";
}

if ($message) {
  open( M, ">.default" . $$ . "clci" ) or die "Could not open message file\n";
  print M $message;
  close(M);
}


#-------------------------------------------------------------------------------
#Initial SVN stuff

#Check that family exists in svn

my $config = Bio::Rfam::Config->new;
my $client = Bio::Rfam::SVN::Client->new({config => $config});
$client->checkClanExists($clan);

#
#-------------------------------------------------------------------------------
# Load the family from disk and svn through the middleware
my ( $oldClanObj, $upClanObj );
my $clanIO = Bio::Rfam::ClanIO->new;

$upClanObj = $clanIO->loadClanFromLocalFile( $clan, $pwd );
print STDERR "Successfully loaded local copy $clan through middleware\n";

#Now load the remote family
$oldClanObj = $clanIO->loadClanFromSVN( $clan, $client );
print STDERR "Successfully loaded SVN copy of $clan through middleware\n";

#-------------------------------------------------------------------------------
#Add the appropriate callback to the client. This adds the message if not supplied.
$client->addCLCILog();

#These two checks are also performed elsewhere, but this message is more informative.
#Check the desc accessions are the same
if ( $upClanObj->DESC->AC ne $oldClanObj->DESC->AC ) {
  die
"\n***** Accession error, your local copy does not match the SVN repository *****\n\n";
}

#Check the desc accessions are the same
if ( $upClanObj->DESC->ID ne $oldClanObj->DESC->ID ) {
  die
"\n***** Identifier error, your local copy does not match the SVN repository. "
    . " Do you mean to change the name? If so use rfmove. *****\n\n";
}

#-------------------------------------------------------------------------------
#Perform the QC on the clan

#Map the ignore flags into a hash:
my $error = 0;
$error = Bio::Rfam::QC::essentialClan($upClanObj, $oldClanObj, $config);
die "Failed essential QC step.\n" if($error);
#TODO put spell check in!
#$error = Bio::Rfam::QC::optionalClan();


#-------------------------------------------------------------------------------
#If we get here, then great! We can now check the family in!
my $caught_cntrl_c;
$SIG{INT} = sub { $caught_cntrl_c = 1; };    # don't allow control C for a bit!

$client->commitClan($clan);
print STDERR "Successfully checked family in\n";
#------------------------------------------------------------------------------
#Clean-up
#Remove any file containing the check-in message
if ( -s ".default" . $$ . "clci" ) {
  unlink( ".default" . $$ . "clci" )
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

AIM: To perform quality control checks on an existing clan and commit to the SVN repository.
    
EOF

exit(1);

}
