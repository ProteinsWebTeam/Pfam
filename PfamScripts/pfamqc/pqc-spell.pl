#!/software/bin/perl

# A script to enter an i-spell session for the free text of a family
# Make a file of free text only. Keep line numbers for eachline.
# Free text lines will be those beginning with RT or CC.
# Not a lot has change to this script as part of the HMMER3 migration (rdf).


use strict;
use File::Copy;

use Bio::Pfam::Config;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::PfamQC;

my $config = Bio::Pfam::Config->new;
my $dictionary = $config->dictionary;

if( $#ARGV == -1 ) {
    print "pqc-spell.pl. Checks spelling of free text in DESC files.\nUsage pqc-spell.pl <pfam-directories>\n";
    exit(1);
}

my $familyIO = Bio::Pfam::FamilyIO->new;

foreach my $family (@ARGV) {
  Bio::Pfam::PfamQC::checkDESCSpell( $family, $familyIO );
}

  