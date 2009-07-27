#!/usr/local/bin/perl

use strict;
use warnings;

use Bio::Pfam::PfamQC;

my $family = shift;
my $pwd = getcwd;

my $familyIO = Bio::Pfam::FamilyIO->new;
my $famObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );

unless ( Bio::Pfam::PfamQC::sequenceChecker( $family, $famObj ) ) {
  print "$family contains sequence errors.  You should rebuild this family.\n";
  exit(1);
}