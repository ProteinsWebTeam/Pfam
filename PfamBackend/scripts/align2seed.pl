#!/usr/bin/perl

# Copyright (c) 2007: Genome Research Ltd.
#
# Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)
#
# This is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
# 
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <http://www.gnu.org/licenses/>.

use strict;
use warnings;
use Getopt::Long;
use File::Temp qw/ tempfile /;

my $DEBUG = 0;

my ($faFile, $tmpDir, $dataFileDir, $cpus, $pfAcc, $seed, $help);

GetOptions( "in=s"   => \$faFile,
		    "tmp=s"  => \$tmpDir,
		    "data=s" => \$dataFileDir,
		    "acc=s" => \$pfAcc,
		    "seed"  => \$seed,
		    "h"      => \$help );
		   
if(!$faFile or !$tmpDir or !$dataFileDir or !$pfAcc){
 	&usage;
 	exit(1);
 }


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
open(HMM,"hmmfetch $dataFileDir/Pfam-A.hmm $pfAcc |") ||
  die "Failed to get open hmmfetch pipe, hmmfetch $dataFileDir/Pfam-A.hmm $pfAcc:[$!] \n";
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
  print $tmpSeedFh "# STOCKHOLM 1.0\n";
  while(<SEED>) {
    last if($read == 1 and $_ =~ /\/\//);
    if($read && $_ !~ /^#/){
      print $tmpSeedFh $_;
    }
    $read = 1 if($_ =~ /AC\s+($pfAcc)/);
  }
  print $tmpSeedFh "//\n";
  close(SEED) || die "Could not close Pfam-A.seed:[$!]\n";;
  close $tmpSeedFh;
}

my  $tmpAliFile = "/tmp/$$.ali"; 
#Okay, if we have the HMM, fa (and seed)
my $cmd = "hmmalign -q --outformat Pfam";
$cmd .= " --mapali $tmpSeedFile" if($seed);
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
open(CONS, "consensus.pl -method pfam -file $tmpAliFile |") or die "Failed to get consensus string\n";
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
