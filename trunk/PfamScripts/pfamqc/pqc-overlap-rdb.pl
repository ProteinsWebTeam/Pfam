#!/usr/local/bin/perl
#
# Script to find overlaps between a family (on disk) and the live curation MySQL database!
#
use strict;
use warnings;
use Getopt::Long;
use Cwd;

use Bio::Pfam::Config;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

my ( @ignore, $endpoints_opt, $help, $add_to_clan, $remove_from_clan, $compete );

&GetOptions(
  "i=s@"               => \@ignore,
  "help"               => \$help,
  "compete"            => \$compete
) or die "Incorrect option passed in\n"; 

my $family = shift;

if ( !$family ) {
  print STDERR "ERROR:You have not specified the family\n";
  help();
}

if ( $family =~ /^(\S+)\/$/ ) {
  chop $family;
}

help() if ($help);

my %ignore;    #Hash used to ignore duplicating entries in @ignore
  foreach my $fam (@ignore) {
  $ignore{$fam} = 1;
}


my $config = Bio::Pfam::Config->new;
my $connect = $config->pfamlive;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{ $connect }
);

print STDERR "Going to try to resolve overlaps via competing\n" if($compete); 

#Find out if family is in rdb
my $rdb_family = $pfamDB->getPfamData($family);

my $familyIO = Bio::Pfam::FamilyIO->new;

#-------------------------------------------------------------------------------
my $pwd = getcwd;

if( !(-d "$pwd/$family") ) {
    die "$0: [$pwd/$family] is not a current directory.\nMust be in the parent directory of the family to check in\n";
}

if( !-w "$pwd/$family" ) {
    die "$0: I can't write to directory [$pwd/$family].  Check the permissions.\n";
}

#-------------------------------------------------------------------------------
  
my $famObj = $familyIO->loadPfamAFromLocalFile($family, $pwd);
print STDERR "Successfully loaded $family through middleware\n";


my $overlaps =
  &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \%ignore, undef, $pfamDB, $famObj, $compete );
  warn "$family: found $overlaps external overlaps\n";

if ($overlaps) {
  exit(1);
}else {
  exit(0);
}

sub help {
  print STDERR << "EOF";

This script runs checks for overlaps between a Pfam family and the
current Pfam database (pfamlive).


Usage:

    $0 <family>


Addional options:

  -add_to_clan <clan_accession>       :Adds family to clan
  -remove_from_clan <clan_accession>  :Removes family from clan
  -i <family_name>                    :Ignore this family (-i can occur multiple times)
  -e                                  :Find end points of extensions


  

Example:

    $0 AAA -remove_from_clan CL0023

EOF

  exit(0);
}
