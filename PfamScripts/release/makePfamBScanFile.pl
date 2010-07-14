#!/usr/local/bin/perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $config = Bio::Pfam::Config->new;
my $dir = shift;

unless($dir and -d $dir){
  $logger->logdie("Could not find the releases directory");  
}

$logger->debug("Making Pfam-B.hmm");

unless($dir."/Pfam-B"){
  $logger->logdie("Could not find the Pfam-B hmms");  
}

open(B, $dir."/Pfam-B") or 
  $logger->logdie("Could not open Pfam-B:[$!]");
open(B2, ">".$dir."/Pfam-B.top20000") or 
  $logger->logdie("Could not open Pfam-B.top20000:[$!]");
  
$/="//";
while(<B>){
  print B2 $_;  
  if(/#=GF ID   Pfam-B_(\d+)/){
    if($1 == 20000){
      print B2 "\n";
      close(B2);  
      last;
    }
  }  
}
close(B);
$/="\n";


#system($config->hmmer3bin."/hmmbuild $dir/Pfam-B.hmm.a $dir/Pfam-B.top20000") 
 # and $logger->logdie("Failed to run hmmbuild:[$!]");

$logger->debug("Making Pfam-B.hmm.dat");

unless($dir."/Pfam-B.hmm"){
  $logger->logdie("Could not find the Pfam-B hmms");  
}

open(S, ">".$dir."/Pfam-B.hmm.dat") or 
  $logger->logdie("Could not open the scan data dir in the release directory:[$!]");

open(H, $dir."/Pfam-B.hmm") or 
  $logger->logdie("Could not open the hmm file in the release directory:[$!]");
 
my @data;
$/="//";
while(<H>){
  
  $/= "\n";
  my @f = split(/\n/, $_);
  #NAME  Pfam-B_1
  #ACC   PB000001
  #LENG  251
  my($name, $acc, $length);
  foreach my $l (@f){
    if($l =~ /^NAME\s+(\S+)/){
      $name = $1;
    }elsif($l =~/^ACC\s+(\S+)/){
      $acc = $1;
    }elsif($l =~ /LENG\s+(\d+)/){
      $length = $1;
      last;  
    }  
  }
  if($name){
    push(@data, { name => $name, acc => $acc, leng => $length });
  }
  $/="//";
} 

$/="\n";
foreach my $d (@data){
  print S "# STOCKHOLM 1.0\n";
  print S "#=GF ID   ".$d->{name}."\n";
  print S "#=GF AC   ".$d->{acc}."\n";
  print S "#=GF ML   ".$d->{leng}."\n";
  print S "//\n";
}  
  
close(S);

#Press and convert the files.
$logger->debug("Making Pfam-B.hmm.bin");

system($config->hmmer3bin."/hmmpress -f $dir/Pfam-B.hmm")
  and $logger->logdie("Failed to run hmmpress:[$!]");

system($config->hmmer3bin."/hmmconvert -b $dir/Pfam-B.hmm > $dir/Pfam-B.hmm.bin ")
  and $logger->logdie("Failed to run hmmconvert:[$!]");
