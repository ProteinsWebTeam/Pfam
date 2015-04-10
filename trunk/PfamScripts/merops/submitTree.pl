#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;

my $family;
GetOptions("family=s" => \$family);

my @families;
if($family) {
  push(@families, $family);
}
else {
  my $dir = ".";
  opendir(DIR, $dir) or die "Couldn't open directory '$dir', $!";
  @families = sort grep { ! /^\./ } readdir(DIR);
}

my $memory_mb=12000;

foreach my $fam (@families) {
  print STDERR "$fam\n";
  chdir($fam) or die "Couldn't chdir into $fam, $!";
  system("bsub -q production-rh6 -R \"rusage[mem=$memory_mb]\" -M $memory_mb -o tree.log -J$fam 'raxmlHPC-PTHREADS-SSE3 -T 4 -p12345 -m PROTGAMMAAUTO -s homologues.afa -n AUTO'");
  chdir("../") or die "Couldn't chdir up from $fam, $!";
}
