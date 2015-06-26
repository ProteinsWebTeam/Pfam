#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;

my $family;

GetOptions("family=s" => \$family);
die "No family" unless($family);

my $raxml="raxmlHPC-PTHREADS-SSE3";
my $hmmerBin="/nfs/production/xfam/users/jaina/merops/bin";
unless(-d $hmmerBin) {
  die "No $hmmerBin\n";
}

#Reformat seed into aligned fasta
unless(-s "SEED") {
  die "No SEED in cwd\n";
}
system("$hmmerBin/esl-reformat -o seed.afa afa SEED");
unless(-s "seed.afa") {
  die "Failed to reformat SEED into aligned fasta\n";
}

my @fastTree= qw(A01A C01A C19 C26 M12B S01A S08A S09C S12 S33 S54); #These families take too long to run using RAxML
my %fastTree = map {($_, 1)} @fastTree;

my $seedTree;
if(exists($fastTree{$family})) {
  #Make initial Fasttree tree
  my $fastTreeCmd="FastTree seed.afa > FastTree.seed";
  system("$fastTreeCmd") and die "Failed to run '$fastTreeCmd', $!";

  $seedTree="FastTree.seed";
}
else {
  #Make initial RAxML tree
  my $raxmlCmd="$raxml -T 4 -p12345 -m PROTGAMMAAUTO -s seed.afa -n seed";
  system("$raxmlCmd") and die "Failed to run '$raxmlCmd', $!";

  $seedTree="RAxML_bestTree.seed";
}

#Insert homologues into tree using  RAxML
my $raxmlInsertCmd="$raxml -T 4 -p12345 -m PROTGAMMAAUTO -n insert -s homologues.afa -f v -t $seedTree";
system("$raxmlInsertCmd") and die "Failed to run '$raxmlInsertCmd', $!";

