#!/usr/bin/env perl

use strict;
use warnings;

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

#Make initial tree
my $raxmlCmd="$raxml -T 4 -p12345 -m PROTGAMMAAUTO -s seed.afa -n seed";
system("$raxmlCmd") and die "Failed to run '$raxmlCmd', $!";

#Insert homologues into tree
my $raxmlInsertCmd="$raxml -T 4 -p12345 -m PROTGAMMAAUTO -n insert -s homologues.afa -f v -t RAxML_bestTree.seed";
system("$raxmlInsertCmd") and die "Failed to run '$raxmlInsertCmd', $!";
