#!/usr/bin/env perl

#Script to print files containing the domains for each proteome in the completed_proteomes tables
#Output: one file foreach proteome in cwd
#Format of file is <seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc> <hmm name> <type> <hmm start> <hmm end> <hmm length> <bit score> <E-value> <clan>

use strict;
use warnings;
use Bio::Pfam::ViewProcess::Proteome;

my $view = Bio::Pfam::ViewProcess::Proteome->new;

if($view->options->{species}){
  $view->speciesToTaxId;
  $view->makeTSV($view->options->{taxid});
}#searching each in turn.
elsif($view->options->{chunk} and $view->options->{chunkSize}){
  $view->proteomeRange;
}
#Perform a full database search
elsif($view->options->{allVsall}){
  $view->submitToFarm(100);
}else{
  die "Incorrect options passed in, try running with --help"; 
}
