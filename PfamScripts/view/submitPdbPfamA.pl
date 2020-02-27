#!/usr/bin/env perl

#Script to submit pdbPfamAReg.pl jobs to farm
#Can be run on all families (-all), or on a single family (-pfamA <pfamA_acc>)
#The script estimates how much memory to request on the farm (currently 3gb)
#A handful (<10 for uniprot29) will fail because insufficient memory was requested
#Re-run these using the -M option


use strict;
use warnings;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;

#Get user options and check they are sensible
my ($all, $acc, $memory_gb);
GetOptions('all' => \$all,
  'pfamA=s'  => \$acc,
  'M=i'  => \$memory_gb);

unless($all or $acc) {
  die "Need to specify whether to do all families (-all) or a single family (-pfamA <pfamA_acc>)\n";
}
if($all and $acc) {
  die "Can't use -all and -acc option together\n";
}
if($all and $memory_gb) {
  die "Can't use -all and -M option together\n";
}


#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

#Create array of families to work on
my @pfamA;
if($all) {
  @pfamA=$pfamDB->getSchema->resultset('PfamA')->search();
}
else {
  @pfamA=$pfamDB->getSchema->resultset('PfamA')->find({pfama_acc => $acc});
}

#Create group pdbPfamA and use this to limit number of running jobs to 100
system("bgadd -L 100 /pdbPfamA");
my $group='/pdbPfamA';

#3gb is enough for 99% of the families
unless($memory_gb) { 
  $memory_gb=3;
}
my $memory_mb=$memory_gb*1000;

#Loop through each pfamA and run searches
my $queue = $config->{farm}->{lsf}->{queue};
foreach my $pfamA (@pfamA) {
  my $pfamA_acc=$pfamA->pfama_acc;

  #Submit to farm
  print STDERR "$pfamA_acc\n";
  system("bsub -q queue -J$pfamA_acc -o $pfamA_acc.log -M $memory_mb -R \"rusage[mem=$memory_mb]\" -g $group 'pdbPfamAReg.pl -pfamA $pfamA_acc'");
}
