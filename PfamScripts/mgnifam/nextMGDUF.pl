#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new(%{$config->pfamliveAdmin});

unless($pfamDB and $pfamDB->{database} eq "mgnifam") {
  die "Need to use the config for mgnifam\n";
}


#Get a list of dufs from the database
my %duf_numbers;
my @fams = $pfamDB->getSchema->resultset("Mgnifam")->search();
my @dead_fams = $pfamDB->getSchema->resultset("DeadMgnifam")->search();

foreach my $mgnifam (@fams, @dead_fams){
  if($mgnifam->mgnifam_id =~ /^MGDUF(\d+)$/){
    $duf_numbers{$1}=1;
  }
}


#Find the biggest duf number, and increment by one
foreach my $duf (sort { $b <=> $a } keys %duf_numbers) {
  my $num = $duf + 1;
  my $duf_num = "MGDUF".$num;
  print "\nThe next available MGDUF number is: $duf_num\n\n";
  last;
}
