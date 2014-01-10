#!/usr/bin/env perl

#Script to split a fasta file into smaller chunks, and run pfam_scan.pl on each chunk
#Input: Fasta file and directory location of Pfam-A.hmm files
#Optional: the number of sequences to spilt this file into, name of ouput file
#Output is in cwd

use strict;
use warnings;
use Getopt::Long;
use IO::File;
use Bio::Pfam::Config;

#Get user options
my ($help, $fasta, $split_number, $hmm_dir, $output);
&GetOptions ("fasta=s" => \$fasta,
             "split=i" => \$split_number,
	     "hmm_dir=s" => \$hmm_dir, 
	     "output=s"  => \$output);

#Does fasta file exist
unless($fasta and -s $fasta) {
  help();
}

#Check can get pfam config okay
my $config = Bio::Pfam::Config->new;
die "No pfam config file" unless($config);

unless($config->location eq "EBI") {
  die "This script currently only works at EBI";
}

#Define output file if not defined already
$output="pfam_scan_result" unless($output);

#Check split number is sensible, if not specified use default
if($split_number) {
  unless($split_number >= 50 and $split_number <= 10000) {
    die "Split number must be > 50 and <= 10000";
  }
}
else {
  $split_number = 1000;
}

#Check $hmm_dir contains the right files
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
  die "Need to specify hmm_dir\n";
  help();
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

#Run pfam_scan.pl on farm using job arrays, select 2Gb memory
print STDERR "Submitting $j+1 pfam_scan.pl jobs to farm\n";
my $num_files = "1-$j";
my $fh = IO::File->new();


$fh->open( "| bsub -q ". $config->farm->{lsf}->{queue}." -o $name.err -J$name\"[$num_files]\" -R \"rusage[mem=2000]\" -M 2000") or die "Couldn't open file handle\n";
$fh->print( "pfam_scan.pl -dir $hmm_dir -fasta $name\$\{LSB_JOBINDEX} -cpu 4 > $name\$\{LSB_JOBINDEX}.out\n"); 
$fh->close;


#Concatenate output files into one
#Remove intermediate files
my $fh2 = IO::File->new();

$fh2->open( "| bsub -q ". $config->farm->{lsf}->{queue}." -o $name.log -Jcat_$name -w 'done($name)' " ) or die "Couldn't open file handle\n";
$fh2->print("cat $name*out > $output\n");
$fh2->print("rm -fr $name*\[0-9\]\n");
$fh2->print("rm -fr $name*out\n");
$fh2->close;


sub help {
print STDERR << "EOF";

This program splits a fasta file into smaller chunks named <fasta>1,
<fasta>2 etc. It runs pfam_scan.pl using Pfam GA thresholds on each
chunk on the farm. If all chunks run on the farm successfully,
the script will concantenate all output files into a single file
called 'pfam_scan_result', and delete the intermediate files.

EXAMPLE:

$0 -fasta NR_PDB.fasta -hmm_dir <directory location of HMMs>

OPTIONS:
  -split <integer>    Number of sequences to put in each smaller file 
                      (value must be >=50 and <=10000, default: 1000)

  -output <filename>  Name of output file (default: pfam_scan_result)
 
EOF
 
exit (0);
}
