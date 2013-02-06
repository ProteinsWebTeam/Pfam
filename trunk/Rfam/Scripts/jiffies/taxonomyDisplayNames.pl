#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Rfam::Config;
use Data::Printer;

my $config = Bio::Rfam::Config->new;
my $rfamdb = $config->rfamlive;

my $upSth = $rfamdb->prepare_updateTaxonomy;

my $rs = $rfamdb->resultset('Taxonomy')->search({});
my $tax = $rs->cursor;
while (my @t = $tax->next){
  my $speciesOri = $t[1];
  my $ncbi = $t[0];
  my $treeDN = $speciesOri;
  $treeDN =~ s/\s+/\_/g;
  
  my $alignDN = $speciesOri;
  if($alignDN =~ /(.*)\s+(sp|subsp)\./){
    $alignDN = $1;
  }
  $alignDN =~ s/\s+/\_/g;
  if(length($alignDN) > 25){
    $alignDN = substr($alignDN, 0,23);
    $alignDN =~ s/(.*?)[\_|\.]+^/$1/g;
    $alignDN .= '..';
  }
  $alignDN .= '['.$ncbi.']';
  #$upSth->execute($treeDN, $alignDN, $ncbi);
  print "O $speciesOri\nT $treeDN\nA $alignDN\n\n"; 
}
