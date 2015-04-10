#!/usr/bin/env perl

use strict;
use warnings;
use IO::File;
use Cwd;

#Set up the parameters
my $hmmerBin="/nfs/production/xfam/pfam/software/bin";
my $cpu = 4;
my $seqDB="/nfs/production/xfam/users/jaina/merops/db/uniprot_trembl_sprot.fasta";
my $queue="production-rh6";
my $memory_mb=4000;
my $evalue=0.1;


#Run hmmbuild
my $hmmbuildCmd="$hmmerBin/hmmbuild -o /dev/null HMM SEED";
system("$hmmbuildCmd") and die "Couldn't run '$hmmbuildCmd', $!";
unless(-s "HMM") {
  die "Failed to build HMM";
}

#Open connection to farm
my $cwd=getcwd;
my $fh = IO::File->new();
$fh->open( "| bsub -q $queue -n $cpu -R \"rusage[mem=$memory_mb]\" -M $memory_mb -o hmmer.log -Jhmmsearch$$");
$fh->print( "cd $cwd\n" );

#Run hmmsearch
my $hmmsearchCmd="$hmmerBin/hmmsearch -E $evalue --domtblout hmmsearchTbl --cpu 4 -o hmmsearchOut -A hmmsearchAln HMM $seqDB";
$fh->print("$hmmsearchCmd\n");
  
#Make fasta file of sequences in hmmsearchAln    
my $eslCmd="$hmmerBin/esl-reformat -o FA fasta hmmsearchAln";
$fh->print("$eslCmd\n");

#Run hmmalign on sequnces and include SEED alignment
my $hmmalignCmd="$hmmerBin/hmmalign -o tmpAln --outformat pfam --mapali SEED HMM FA";
$fh->print("$hmmalignCmd\n");

#Remove lines that start with # and blank lines
$fh->print("grep -v \'^#\\|^\$\' tmpAln > ALIGN\n");
$fh->print("rm -fr tmpAln\n");

#Finish
$fh->close();
