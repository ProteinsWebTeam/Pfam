#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::SeqFetch;
use Bio::Pfam::FamilyIO;
use Bio::Pfam::Family::DESC;

my $cluster_name;
GetOptions( "cluster=s" => \$cluster_name);

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $cluster_file = $config->{mgnify}->{clusterseq};
my $mgnify_fasta = $config->{mgnify}->{location};

unless($cluster_name and $cluster_name =~ /^MGYP\d{12}$/) {
  print STDERR "Need to specify a valid cluster name on the command line\n";
  print STDERR "E.g. $0 -cluster MGYP000001829837\n";
  exit;
}

print STDERR "Getting $cluster_name cluster members\n";
my $cluster_seqs;
my $total_seqs=0;
open(CLUSTER, "gunzip -c $cluster_file |") or die "Couldn't open fh to 'gunzip -c $cluster_file', $!";
while(<CLUSTER>) {
  if(/^(\S+)\s+(\S+)/) {
    my ($name, $seq) = ($1, $2);
    if($name eq $cluster_name) {
      push( @{ $cluster_seqs->{$seq} }, { whole => 1 } );
      $total_seqs++;
    }
  }
}
close CLUSTER;

if($total_seqs == 0 ) {
  die "$cluster_name not found in $cluster_file\n";
}
else {
  print STDERR "$cluster_name contains $total_seqs sequences\n";
}

print STDERR "Retrieving sequences\n";
my $fasta_file = "seqs.fa";
open(F, ">$fasta_file") or die "Couldn't open fh to $fasta_file, $!"; 
my $noSeqsFound = &Bio::Pfam::SeqFetch::fetchSeqs($cluster_seqs, $mgnify_fasta, \*F);

unless($noSeqsFound == $total_seqs) {
  die "Did not find all the sequences you requested: Requested $total_seqs; Got $noSeqsFound\n";
}

print STDERR "Running create_alignment.pl\n";
system("create_alignment.pl -fasta $fasta_file -mu > SEED") and die "Couldn't run 'create_alignment.pl -fasta $fasta_file -mu > SEED', $!";

#Create a DESC file
my $io = Bio::Pfam::FamilyIO->new;
my %desc = ( 
  ID    => 'ShortName',
  DE    => 'Family description',
  AU    => [ { name => 'Who RU' } ],
  SE    => "$cluster_name (release ".$config->{mgnify}->{release}.")",
  CUTGA => { seq => '27.00', dom => '27.00' },
  CUTNC => { seq => '27.00', dom => '27.00' },
  CUTTC => { seq => '27.00', dom => '27.00' },
  BM    => 'hmmbuild  -o /dev/null HMM SEED;',
  SM    => 'hmmsearch -Z ' . $config->{mgnify}->{dbsize} . ' -E 1000 HMM mgnify',
  TP    => 'Family'
);  

my $desc = Bio::Pfam::Family::DESC->new(%desc);
$io->writeDESC($desc);

