#!/usr/bin/env perl

use strict;
use warnings;

my $hmmerTbl = "hmmsearchTbl";

open(FA, ">dom_se") or die "Couldn't open fh to dom_se, $!";
open(HMMERTBL, $hmmerTbl) or die "Couldn't open fh to $hmmerTbl, $!";
while(<HMMERTBL>) {
  next if(/^#/);
  my @line = split(/\s+/, $_);
  my ($hmmer_acc, $envFrom, $envTo) = ($line[0], $line[19], $line[20]);
  print FA "$hmmer_acc/$envFrom-$envTo $envFrom $envTo $hmmer_acc\n"; #This is the format required by esl-sfetch
}
close HMMERTBL;
close FA;
