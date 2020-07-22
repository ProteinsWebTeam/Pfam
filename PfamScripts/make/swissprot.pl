#! /usr/bin/perl -w

use strict;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Cwd;
use Getopt::Long;

# Jiffy script that attempts to identify Swiss-Prot sequences from a PFAMOUT file
# You run it in the Pfam family directory itself and it will look for the PFAMOUT file
# It also createa a file called arch, with either swiss-prot protein Pfam matches or
# otherwise top n scoring proteins in it (default n=5).

# To do
# Fetch regions for each protein and display in sp file and in STDOUT
# Add option to fetch all architectures


my $help;
my $n = 5;
GetOptions('num=i' => \$n,
           'help'  => \$help);

if($help) {
  help();
}
unless(-s "PFAMOUT" and -s "scores"){
  warn "You need to run this script in a Pfam family directory with a PFAMOUT and scores file\n";
  help();
}

unless($n>0) {
  warn "The num specified on the command line [$n] needs to be greater than 0\n";
  exit;
}

# Read scores file and only report matches!
my %id;
open (SCORES, "scores") or die "Cannot open scores file, $!";
while(<SCORES>){
  if (/^\S+\s+(\S+)\.\d+/){
    $id{$1}=1;
  }
}
close SCORES;


# Find if any swiss-prots are found in PFAMOUT
my %accmap;
my %sp; # Stores sps found
my %top; # Stores accs of top n scoring proteins.  Will give architecture of these if no swiss-prot are available
my $i=0;
unlink "arch";
open (FH, "PFAMOUT") or die "Cannot open fh to PFAMOUT file, $!";
open (OUT, ">sp") or die "Cannot write to sp file, $!";
while(<FH>){
  if (/^(\S+)\.\d+\s+(\S+)_\S+\s+(.*)/){
    my $acc=$1;
    my $id=$2;
    my $desc=$3;

    if ($i<$n){
      $top{$acc}=$desc;
    }
    if ($acc ne $id and $id{$acc}){
      $sp{$acc}=$desc;
      print;
      print OUT;
    }
    $i++;
  }
}
close FH;
close OUT;

# Get live mapping of Pfam ids and accs data
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my @pfamA=$pfamDB->getSchema->resultset('PfamA')->search();
foreach my $pfamA (@pfamA) {
  $accmap{$pfamA->pfama_acc}=$pfamA->pfama_id;
}

#Get current dir name
my $cwd = getcwd;
my $dir = pop @{[split m|/|, $cwd]};
$dir = sprintf("%-26s", $dir); #To make it line up with the other text

# Loop over swiss-Prots and write to arch file
open (ARCH, "> arch") or die "Cannot write to file arch, $!";

if (! %sp){
  %sp=%top;
}
foreach my $sp (sort keys %sp){

  print ARCH "$sp\t$sp{$sp}\n";

  # Get regions from PFAMOUT and add to arch file
  open (PFAMOUT, "PFAMOUT") or die "Cannot read PFAMOUT file, $!";
  while(<PFAMOUT>){
    if (/^$sp\.\d+\s+(\d+)\s+(\d+)\s+\d+\s+\d+\s+\d+\s+\d+\s+(\S+)/){
      my ($st, $en, $bits) = ($1, $2, $3);
      $st = sprintf("%6s", $st);
      $en = sprintf("%6s", $en);
      $bits = sprintf("%8s", $bits);

      print ARCH "\t$dir\t$st\t$en\t$bits\n";
    }
  }
  close PFAMOUT;

  #Get regions from pfam_live
  my @reg_full=$pfamDB->getSchema->resultset('PfamARegFullSignificant')->search({ pfamseq_acc => $sp }, {order_by => 'seq_start ASC'});
  foreach my $row (@reg_full) {
    my $pfamA_id = $accmap{$row->pfama_acc->pfama_acc};
    $pfamA_id = sprintf("%-16s", $pfamA_id);

    my ($st, $en, $bits) = ($row->seq_start, $row->seq_end, $row->domain_bits_score);

    $st = sprintf("%6s", $st); #Allow for the maximum start/end in the db
    $en = sprintf("%6s", $en);
    $bits = sprintf("%8s", $bits); #Allows for the maximum bits score in the db

    print ARCH "\t".$row->pfama_acc->pfama_acc."\t$pfamA_id\t$st\t$en\t$bits\n";
  }
  print ARCH "\n";
}
close ARCH;


sub help {
  print<<EOF;

This script finds Swiss-Prot sequences in a PFAMOUT file. It requires a 
PFAMOUT and scores files in the current working directory. The script 
creates two file: sp and arch. sp contains the Swiss-Prot matches, while 
arch contains the architectures of the Swiss-Prot Pfam matches or, if 
there aren't any, the architectures of the top n scoring proteins 
(default n=5). The Swiss-Prot matches are also printed to STDOUT.

usage:

$0

Options:
  -num <integer>: The number of top scoring proteins to add to the arch file
                  if no Swiss-Prot Pfam matches are found (default=5)
  -help         : Shows this help
EOF

  exit;
}
