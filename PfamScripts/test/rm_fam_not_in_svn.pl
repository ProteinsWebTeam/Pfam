#!/usr/bin/env perl
#
# This script should not be used unless something wrong happened when pfnew.
# If on pfnew a family failed to commit to svn but was added to the database,
# this script allows the killing of that family (database-only removal of family)
#

use strict;
use warnings;

use Cwd;
use Data::Dumper;
use Getopt::Long;
use File::Temp;
use Try::Tiny;

use Bio::Pfam::SVN::Client;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

#-------------------------------------------------------------------------------
# Deal with all of the options

my $user = $ENV{USER};
my ( $message, $family, $help );

&GetOptions(
  "m=s"              => \$message,
  "f=s"              => \$family,
  "help"             => \$help
) or die "Unrecognised option passed in to the script.\n";

help() if ($help);

unless ($family) {
  warn "\n***** No family provided  *****\n\n";
  help();
}
chomp($family);

if($family =~ /(\S+)\/$/) { #Remove trailing '/' if present
  $family = $1;
}

if (@ARGV) {
  warn "\n***** $0 unexpected arguments *****\n\n";
  help();
}

if ( !$message ) {
  warn "\n***** Message is mandatory *****\n\n";
  help();
}


my $config = Bio::Pfam::Config->new;


# Connecting to pfam_live database
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh    = $pfamDB->getSchema->storage->dbh;



my $client = Bio::Pfam::SVN::Client->new;



#Check that family does not exist in svn

my $fam_in_svn;
try {
  $client->checkFamilyExists( $family );
  $fam_in_svn = "Family $family seems to exist in the SVN repository. Abort.\n"
} catch {
  print "Family $family does not exist in the SVN repository. Continue.\n"; # not $@
};

if ($fam_in_svn) {
  die $fam_in_svn;
}


#Now delete from database
try {
  $pfamDB->deletePfamA( $family, $message, '' , $user );
} catch {
  die "Could not delete $family from the database. Does it exist in the database?\nERROR: $!";
};



print "Done.\n";




sub help {
  
print<<EOF;

  usage: $0 -f <PfamA_acc> -m <message>
  
  Aim: Remove a family from the mysql database that for some reason does not exist in the SVN server.

  
  -f    - Family accession to remove from the database.
  -m    - Specify the message that describes the changes you have made to this family 
          on the command line, avoid being prompted for it at a later stage.                   
          
EOF

exit(1);

}
