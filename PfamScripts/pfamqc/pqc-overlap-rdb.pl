#!/usr/bin/env perl
#
# Script to find overlaps between a family (on disk) and the live curation MySQL database!
#
use strict;
use warnings;
use Getopt::Long;
use Cwd;

use Bio::Pfam::Config;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

my ( @ignore, $help, $compete, $no_sigP, $all, $noFilter );

&GetOptions( "i=s@"     => \@ignore,
             "help"     => \$help,
             "compete"  => \$compete,
             "no_sigP"  => \$no_sigP,
             "all"      => \$all,
             "no_filter" => \$noFilter
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

#Need admin access to db (for creating temporary table) if 
my $pfamDBAdmin;
unless($noFilter) {
  my $connectParams = $config->pfamliveAdmin;
  $pfamDBAdmin   = Bio::Pfam::PfamLiveDBManager->new( %{$connectParams} );
}

my $overlaps =
  &Bio::Pfam::PfamQC::family_overlaps_with_db( $family, \%ignore, undef, $pfamDB, $famObj, $compete, $all, $noFilter, $pfamDBAdmin );
  warn "$family: found $overlaps external overlaps\n";

unless($no_sigP) {
  my $signal_peptide_overlap = &Bio::Pfam::PfamQC::family_overlaps_with_signal_peptide($family, $famObj, $pfamDB);
  if($signal_peptide_overlap->{total}>0) {
    warn "$family: there are $signal_peptide_overlap->{total} signal peptide overlaps, $signal_peptide_overlap->{seed} in SEED and $signal_peptide_overlap->{align} in ALIGN\n";
    exit(1);
  }
  else {
    warn "$family: found 0 signal peptide overlaps\n";
  }
}


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

  -i <family_name>       :Ignore this family (-i can occur multiple times)
  -compete               :Compete family before checking for overlaps
  -no_sigP               :Do not check whether family overlaps with signal peptide
  -all                   :Report overlaps for all sequences and not reference proteomes only
  -noFilter              :Do not filter overlaps


EOF

  exit(0);
}
