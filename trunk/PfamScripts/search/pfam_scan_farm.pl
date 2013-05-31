#!/usr/bin/env perl

#Script to split a fasta file into smaller chunks, and run pfam_scan.pl on each chunk
#Input is fasta file
#Optional: Location of HMMs to use, and the number of sequences to spilt this file into
#Output is in cwd

use strict;
use warnings;
use Getopt::Long;
use IO::File;

#Get user options
my ($help, $fasta, $split_number, $hmm_dir);
&GetOptions ("fasta=s" => \$fasta,
             "split=i" => \$split_number,
	     "hmm_dir=s" => \$hmm_dir );

#Does fasta file exist
unless($fasta and -s $fasta) {
  help();
}

#Check split number is sensible, if not specified use default
if($split_number) {
  unless($split_number >= 50 and $split_number <= 10000) {
    die "Split number must be > 50 and <= 10000";
  }
}
else {
  $split_number = 1000;
}

#Check $hmm_dir contains the right files, if $hmm_dir not specified, use HMMs from current release
if($hmm_dir) {
  my $error;
  foreach my $file (qw(Pfam-A.hmm  Pfam-A.hmm.dat  Pfam-A.hmm.h3f  Pfam-A.hmm.h3i  Pfam-A.hmm.h3m  Pfam-A.hmm.h3p)) {
    unless(-s "$hmm_dir/$file") {
      $error=1;
      print STDERR "$hmm_dir/$file does not exist\n";
    }
  }
  if($error) {
    die "Files required to run pfam_scan are missing";
  }
}
else {
  $hmm_dir="/lustre/scratch110/blastdb/Supported";
  print STDERR "Using HMMs in /lustre/scratch110/blastdb/Supported\n"; 
}

#Set up counters
my $i=0;
my $j=1;

#Obtain name of fasta file if not in cwd
my $name;
if($fasta =~ /\S+\/(\S+)$/) {
  $name = $1;
}
else {
  $name = $fasta;
}

#Name of first file handle
my $filename = $name . $j;

#Split fasta file
print STDERR "Splitting $name into files of $split_number sequences\n";
open(FH, ">$filename") or die "Couldn't open fh to $filename, $!";
open(FASTA, $fasta) or die "Couldn't open $fasta, $!";  
while(<FASTA>) {
  if(/^>/){
    $i++;
    if($i > $split_number) {
      close FH; #Close file handle
      $j++; 
      $filename = $name . $j; #Name of next file handle
      open(FH, ">$filename") or die "couldn't open fh to $filename, $!";
      $i=1;
    }
  }
  print FH $_;
}
close FASTA;
close FH;

#Run pfam_scan.pl on farm using job arrays, submit to long queue, select 2Gb memory
print STDERR "Submitting $j pfam_scan.pl jobs to farm\n";
my $num_files = "1-$j";
my $fh = IO::File->new();

$fh->open( "| bsub -q normal -o $name.err -J$name\"[$num_files]\" -R \"select[type==X86_64 && mem>2000] rusage[mem=2000]\" -M 2000000") or die "Couldn't open file handle\n";
$fh->print( "pfam_scan.pl -dir $hmm_dir -fasta $name\$\{LSB_JOBINDEX} > $name\$\{LSB_JOBINDEX}.out\n"); 
$fh->close;


sub help {
print STDERR << "EOF";

This program splits a fasta file into smaller chunks named <fasta>1,
<fasta>2 etc. It runs pfam_scan.pl using Pfam GA thresholds on each
chunk on the Sanger farm. The output will be pfam_scan.pl output files
named <fasta>1.out, <fasta>2.out etc. in the cwd.

EXAMPLE:

$0 -fasta NR_PDB.fasta

OPTIONS:
  -split <integer>  Number of sequences to put in each smaller file 
                    (value must be >=50 and <=10000, default is 1000)

  -hmm_dir <dir>    Directory location of HMMs 
                    (default is HMMs from current release)
 
EOF
 
exit (0);
}
