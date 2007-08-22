#!/usr/bin/perl

use strict;
use warnings;
use Getopt::Long;
use File::Temp qw/ tempfile /;

my $DEBUG = 0;

my ($faFile, $tmpDir, $dataFileDir, $cpus, $pfAcc, $seed, $help);

GetOptions( "in=s"   => \$faFile,
		    "tmp=s"  => \$tmpDir,
		    "data=s" => \$dataFileDir,
		    "cpu=s"  => \$cpus,
		    "acc=s" => \$pfAcc,
		    "seed"  => \$seed,
		    "h"      => \$help );
		   
if(!$faFile or !$tmpDir or !$dataFileDir or !$pfAcc){
 	&usage;
 	exit(1);
 }

$cpus = 1 if(!$cpus); #If not specified, assuming that we are running on a single core machine.

if($help){
	&usage;
}

#Check the the fast file exists and the the files we need to use are present.
unless (-d $tmpDir){
	die "$tmpDir does not exist:[$!]\n";
}
unless (-s "$tmpDir/$faFile"){
	die "Could not find fasta file ($faFile) in the tmp dir ($tmpDir):[$!]\n";
}

#Now get the HMMs
my( $tmpHMMFh, $tmpHMMFile ) = tempfile( DIR => $tmpDir );
open(HMM,"hmmfetch $dataFileDir/Pfam_ls.bin $pfAcc |") ||
  die "Failed to get open hmmfetch pipe, hmmfetch $dataFileDir/Pfam_ls.bin $pfAcc:[$!] \n";
while(<HMM>) {
  print $tmpHMMFh $_;
}
close(HMM) || die "Could not clode hmmfetch pipe:[$!]\n";;
close $tmpHMMFh;

#Now get the SEED alignment, if desired
my( $tmpSeedFh, $tmpSeedFile );
if($seed){
  ( $tmpSeedFh, $tmpSeedFile ) = tempfile( DIR => $tmpDir );  
  open(SEED,"$dataFileDir/Pfam-A.seed") ||
   die "Failed to open $dataFileDir/Pfam-A.seed:[$!] \n";
  my $read = 0;
  while(<SEED>) {
    last if($read == 1 and $_ =~ /\/\//);
    if($read && $_ !~ /^#/){
      print $tmpSeedFh $_;
    }
    $read = 1 if($_ =~ /AC\s+($pfAcc)/);
  }
  close(SEED) || die "Could not close Pfam-A.seed:[$!]\n";;
  close $tmpSeedFh;
}

my  $tmpAliFile = "/tmp/$$.ali"; 
#Okay, if we have the HMM, fa (and seed)
my $cmd = "hmmalign --oneline -q";
$cmd .= " --withali $tmpSeedFile" if($seed);
$cmd .= " $tmpHMMFile $tmpDir/$faFile";


$DEBUG && print STDERR "Going to run:$cmd\n"; 
open( OUT, "$cmd |") or die "failed to open pipe on $cmd:[$!]\n";
open( OUT2, "> $tmpAliFile") or die "failed to open $tmpAliFile:[$1]\n";
my $displayLength;
while(<OUT>){
  if( $_ !~ m{^[#|//]} and m{^\S+} ){
    if(!$displayLength) {
      if( my ($d) = $_ =~ /^(\S+\s+)\S+$/){
        $displayLength = length($d); 
      }
    }
   print $_;
   print OUT2 $_; 
  }
}
close(OUT);
close(OUT2);

$DEBUG && print STDERR "Going to run:consensus.pl\n"; 
open(CONS, "consensus.pl $tmpAliFile 60 |") or die "Failed to get consensus string\n";
my $con;
while(<CONS>){
  $con = $1 if(/consensus\/60%\s+(\S+)/);
}
close(CONS);
print sprintf('%-'.$displayLength.'s%s', "ConSeq", "$con");

unlink($tmpAliFile);
unlink($tmpHMMFile);
unlink($tmpSeedFile) if ($tmpSeedFile);

sub usage {

print STDERR <<EOF;
Usage:$0 -in <fasta-file> -tmp <directory> -data <directory> -acc <pfam acc> -seed -cpu <#>
-h    : shows this help statement.
-in   : The name of the file containing the input DNA sequence in fasta format.
      : This file is expected to be in the tmp directory.
-tmp  : A directory where file generated as part of this program can be written.
-acc  : The accession of the family which the sequences are to be aligned against.
-cpu  : Number of cpus to use, default is 1.Currently, only the wublast is the multithread program run.
-seed : Include the seed sequences in the alignment. Optional.
-data : The data directory containing the Pfam-A.fasta file (wu-blast indexed) and the Pfam_ls.bin file.

EOF

}

exit(0);
