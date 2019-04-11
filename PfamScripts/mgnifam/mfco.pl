#!/usr/bin/env perl

use strict;
use warnings;
use Text::Wrap;


use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;

my $family = shift;

unless($family) {
  die "Need to specify family dir on the command line\n$0 <family_dir>\n";
}
if($family =~ /(\S+)\/$/) { #Remove trailing '/' if present
  $family = $1; 
}

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );


#Check correct config is being used
unless($pfamDB->{database} eq "mgnifam") {
  die "Need to use the config for mgnifam\n";
}


#Look for family in db
my $mgnifam = $pfamDB->getSchema->resultset('Mgnifam')->find({ mgnifam_acc => $family }); 
unless($mgnifam) {
  $mgnifam = $pfamDB->getSchema->resultset('Mgnifam')->find({ mgnifam_id => $family });
}
unless($mgnifam) {
  die "Couldn't find a family in the database called $family";
}


#Create dir
mkdir($family, 0775) or die "Couldn't mkdir $family, $!";
chdir($family) or die "Couldn't chdir into $family, $!";


#Get seed alignment
my $seed = $pfamDB->getSchema->resultset('MgnifamSeed')->find({ mgnifam_acc => $mgnifam->mgnifam_acc });
unless($seed) {
  die "Error: Couldn't find a seed alignment for ".$mgnifam->pfama_acc." in the database\n";
}
open(SEED, ">SEED") or die "Couldn't open fh to SEED, $!";
print SEED $seed->seed;
close SEED;


#Get HMM
my $hmm = $pfamDB->getSchema->resultset('MgnifamHmm')->find({ mgnifam_acc => $mgnifam->mgnifam_acc });
open(HMM, ">HMM") or die "Couldn't open fh to HMM, $!";
print HMM $hmm->hmm;
close HMM;


#Create DESC
open(DESC, ">DESC") or die "Couldn't open fh to DESC, $!";
print DESC "AC   ".$mgnifam->mgnifam_acc."\n";
print DESC "ID   ".$mgnifam->mgnifam_id."\n";
print DESC "DE   ".$mgnifam->description."\n";
my @authors = split(/, /, $mgnifam->author);
foreach my $author (@authors) { 
  print DESC "AU   $author\n";
}    
print DESC "SE   ".$mgnifam->seed_source."\n";
print DESC "GA   ".$mgnifam->sequence_ga." ".$mgnifam->domain_ga.";\n";
print DESC "TC   ".$mgnifam->sequence_tc." ".$mgnifam->domain_tc.";\n";
print DESC "NC   ".$mgnifam->sequence_nc." ".$mgnifam->domain_nc.";\n";
print DESC "BM   ".$mgnifam->buildmethod."\n";
print DESC "SM   ".$mgnifam->searchmethod."\n";
print DESC "TP   ".$mgnifam->type."\n";
if($mgnifam->comment) {
  $Text::Wrap::columns = 75;
  print DESC wrap('CC   ', 'CC   ', $mgnifam->comment);
}
close DESC;


#Create the last modified hidden file
open(LM, ">.lastModified") or die "Couldn't open fh to .lastModified, $!";
print LM $mgnifam->updated;
close LM;
