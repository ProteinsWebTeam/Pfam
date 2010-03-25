#!/usr/bin/perl -w

#Originally written by mm1.  Now completely re-written by rdf. 
# The aim of this script is to identify the presence of Pfam domains on 
# a piece of DNA sequence.  The is achieved by first running wublastx against
# the dna sequence and the Pfam-A.fasta file.  The gives a list of potential
# families that are hitting the sequence (rather than running all models through
# genewise which would take ages).  We then fetch the HMMs of potential families
# and make a mini HMM database file. Finally, genewise is called to predicted the
# precise region(s) of DNA that corresponds to a Pfam family or families.

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
use File::Temp qw/ tempfile /;
use Bio::SearchIO;
use Bio::SearchIO::blast;
use Getopt::Long;

my $DEBUG = 1;

my ($faFile, $tmpDir, $dataFileDir, $cpus, $help);

GetOptions( "in=s"   => \$faFile,
		    "tmp=s"  => \$tmpDir,
		    "data=s" => \$dataFileDir,
		    "cpu=s"  => \$cpus,
		    "h"      => \$help );
		   
if(!$faFile or !$tmpDir or !$dataFileDir){
 	&usage;
 	exit(1);
 }

$cpus ||=1; #If not specified, assuming that we are running on a single core machine.

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

foreach my $dataFile (qw/Pfam-A.fasta Pfam-A.fasta.phr  Pfam-A.fasta.pin  Pfam-A.fasta.psq Pfam-A.hmm/){
  unless (-s "$dataFileDir/$dataFile"){
	die "Could not find $dataFile in the data dir ($dataFileDir):[$!]\n";
  }
}

unless($ENV{'WISECONFIGDIR'}){
	die "The environment variable WISECONFIGDIR is not set\n";
}
unless (-d $ENV{'WISECONFIGDIR'}){
	die "Can not find the wiseconfig directory\n";
}

#Build the wublast command to try and find Pfam regions on the DNA sequence
my $command = "blastx -db $dataFileDir/Pfam-A.fasta -query $tmpDir/$faFile";

$DEBUG && print STDERR "Going to run $command\n";

#Now run the blast command
open (STDIN, "$command  |") or die "Failed to open pipe on the following command\n";

#Parse the results. Note, I have put a wait limit of 10 minutes on the blast search.
#This is because bioperl only waits for 30secs by default.  This is an over-estimate, but I just
#want the searches to run.


#my $blastio; 
#eval {
#  $blastio = Bio::SearchIO::blast->new( -format => 'blast',
#  										-wait => 600,
#  										-fh   => \*STDIN,
#				  						-signif => 0.001);
#};


#Check that this is true in the sense that zero hits dies..........!
#if( $@ ) {
#  die "Error in parsing blast, please report this bug to Pfam and include your query sequence\n";
#}
#$DEBUG && print STDERR "Finished wublast and parsed results\n";

$DEBUG && print STDERR "Building mini db\n";
#Now store the list of Pfam matches
my %pfamHits;
#while( my $result = $blastio->next_result ) {
 #   while( my $hit = $result->next_hit ) {
 #   	if($hit->description){
 #   		my ($acc) = $hit->description =~ /(PF\d{5}\.\d+)/;
 #    		$pfamHits{$acc} = 1;		
 #    	}
 #    }
#}

while(<STDIN>){
  next unless( $_ =~ /^\s{2}\w+[\/]\d+\-\d+\s+(PF\d+\.\d+)[;][A-Za-z0-9-_]+[;]\s+( ( \d+[.]\d+)|(\d+) )\s+(.*)$/ );
   if( $5 <= 0.01 ){
        $pfamHits{ $1 }++;
   }
}


#Now make a mini database of for the matching family HMMs.....
if(keys %pfamHits){
	my( $tmpFh, $tmpFile ) = tempfile( DIR => $tmpDir );
  	foreach my $pfamA (keys %pfamHits){
		#Get the HMM from the flatfile
    print STDERR $pfamA;
		#my ($tmpPfamA) = $pfamA =~ /(PF\d+)\.\d+/;
		open(HMM,"hmmfetch $dataFileDir/Pfam-A.hmm $pfamA | hmmconvert -2 - |") ||
			die "Failed to get open hmmfetch pipe, hmmfetch $dataFileDir/Pfam_ls.bin $pfamA:[$!] \n";
  		while(<HMM>) {
    		print $tmpFh $_;
  		}
  		close(HMM) || 
  			die "Could not close hmmfetch pipe:[$!]\n";;
	}
	close $tmpFh;
	$DEBUG && print STDERR "Going to run: genewise -pretty -hmmer $tmpFile $tmpDir/$faFile\n";	
	eval{
		open(GENE,"genewise -pretty -hmmer $tmpFile $tmpDir/$faFile | ") || 
			die "Problem opening pipe on genewise output:[$!]\n";
		while(<GENE>){
			print $_;
		}
		close(GENE) || die "Problem closing genewise filehandle:[$!]\n";
	};
	
	if ($@) {
	  print "Problem running genewise: $@ \n";
	}
	unless ($DEBUG){
	    #Now clean up after ourselves
		unlink($tmpFile);
		unlink("$tmpDir/$faFile");
	}
}else {
	print "No Pfam matches found to your DNA sequence\n";
}


sub usage {

print STDERR <<EOF;
Usage:$0 -in <fasta-file> -tmp <directory> -data <directory> -cpu <#>

-h    : shows this help statement.
-in   : The name of the file containing the input DNA sequence in fasta format.
      : This file is expected to be in the tmp directory.
-tmp  : A directory where file generated as part of this program can be written.
-cpu  : Number of cpus to use, default is 1.Currently, only the wublast is the multithread program run.
-data : The data directory containing the Pfam-A.fasta file (wu-blast indexed) and the Pfam_ls.bin file.

EOF

}

exit(0);
