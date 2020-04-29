#!/usr/bin/env perl

#Script to run ncbi searches for families that do not have an ncbi alignment in the database
#Searches are submitted to the farm and uploaded to the database
#Can be used after the Ancillary view for searches that failed to complete
#Default uses 16gb memory, but can request more memory via -M option

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;


my $memory_gb;
GetOptions('M=i'  => \$memory_gb);

unless($memory_gb) {
  $memory_gb = 16;
}
my $memory_mb=$memory_gb*1000;

#Get database connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#Get a list of all pfamA_acc that have an ncbi alignment
print STDERR "Querying database to find out which families have an ncbi alignment\n";
my %done;
my $sth=$dbh->prepare("select pfamA_acc from alignment_and_tree where type = 'ncbi'");
$sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
my ($acc);
$sth->bind_columns(\$acc);

while ($sth->fetch()) {
  $done{$acc}=1;
}


#Submit searches to the farm for families that do not have an ncbi alignment in the database
my $queue = $config->{farm}->{lsf}->{queue};
my $group = "/NcbiSearch";
my $count=0;

print STDERR "Submitting ncbi searches to the farm for families that do not have an ncbi alignment in the database\n"; 

my @pfamA=$pfamDB->getSchema->resultset('PfamA')->search();
foreach my $pfamA (@pfamA) {
  my $pfamA_acc= $pfamA->pfama_acc;

  next if(exists($done{$pfamA_acc}));

  print "Submitting $pfamA_acc\n";
  system("bsub -q $queue -J$pfamA_acc -o $pfamA_acc.log -M $memory_mb -R \"rusage[mem=$memory_mb]\" -g $group 'performOtherSeqDBSearch.pl -acc $pfamA_acc -db ncbi -upload'");
  $count++;
}
print STDERR "Submitted $count searches\n";
