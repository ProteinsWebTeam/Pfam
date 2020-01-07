#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use File::Copy;
use Cwd;

my $dir=getcwd;

unless(-d "Uniprot") {
  print STDERR "Making Uniprot directory\n";
  mkdir("Uniprot", 0755);
}

opendir(DIR, $dir) or die "Can't open dir $dir, $!";
my @fams = grep { /^MGYP/ } readdir DIR;
closedir DIR;

foreach my $fam (@fams) {
  if(-s "$fam/ALIGN") {
    print STDERR "Moving $fam to Uniprot dir\n";
    move("$fam", "Uniprot/$fam") or die "Couldn't move $fam to Uniprot dir, $!";
  }
}
