#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;

my $cluster_name;
GetOptions( "cluster=s" => \$cluster_name);

#Check cluster name seems sensible
unless($cluster_name and $cluster_name =~ /^MGYP\d{12}$/) {
  print STDERR "Need to specify a valid cluster name on the command line\n";
  print STDERR "E.g. $0 -cluster MGYP000001829837\n";
  exit;
}

#Create dir
mkdir($cluster_name, 0775) or die "Couldn't mkdir $cluster_name, $!";
chdir($cluster_name) or die "Couldn't chdir into $cluster_name, $!";

#Set memory for farm
my $memory_gb = 4;
my $memory_mb = $memory_gb*1000;

#Submit job to farm
my $group = '/mgnifam';
system("bsub -q production-rh7 -g $group -o seed.log -J$cluster_name -M $memory_mb -R \"rusage[mem=$memory_mb]\" create_seed_from_cluster.pl -cluster $cluster_name");
chdir("../") or die "Couldn't changde dir up from $cluster_name, $!";
