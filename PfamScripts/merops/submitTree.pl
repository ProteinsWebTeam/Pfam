#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;

my $family;
GetOptions("family=s" => \$family);

my $memory_mb=5000;
my $fastTree="/nfs/production/xfam/users/jaina/merops/bin/FastTree";

my @families;
if($family) {
  push(@families, $family);
}
else {
  my $dir = ".";
  opendir(DIR, $dir) or die "Couldn't open directory '$dir', $!";
  @families = sort grep { ! /^\./ } readdir(DIR);
}


foreach my $fam (@families) {
  if(-d $fam) {
    print STDERR "$fam\n";
    chdir($fam) or die "Couldn't chdir into $fam, $!";
    system("bsub -q production-rh6 -R \"rusage[mem=$memory_mb]\" -M $memory_mb -o fastTree.log -J$fam '$fastTree -out fastTree.tree homologues.afa'");
    chdir("../") or die "Couldn't chdir up from $fam, $!";
  }
} 
