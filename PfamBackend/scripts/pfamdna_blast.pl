#!/usr/bin/perl -w

#Originally written by mm1.  Now completely re-written by rdf. 


use lib '/data/bin/perl/bioperl-live';
use strict;
use warnings; 
use File::Temp qw/ tempfile /;
use Bio::SearchIO;
use Bio::SearchIO::blast;

#Set up some global parameters. If anything goes wrong, please check these first.
my $td = "/tmp"; 
my $hmmdb = "/data/blastdb/Pfam/data/Pfam_ls.bin";
my $hmmfetch = "/data/bin/hmmfetch";
my $genewise = "/data/bin/genewisedb";
$ENV{'WISECONFIGDIR'} = '/data/bin/wise2/wisecfg/';
my $DEBUG = 0;
#Check that the input file exists!
my $file = shift;
if(!-s "$td/$file"){
  die "$td/$file does not exist!\n";	
}

#Build the command to try and find Pfam regions on the DNA sequence
my $command = "/data/bin/wublastx /data/blastdb/Pfam/data/Pfam-A.fasta $td/$file -cpus 2 -V=10000 -B=10000";

#Now run the blast command
open (STDIN, "$command  |");

#Parse the results. Note, I have put a wait limit of 10 minutes on the blast search.
#This is because bioperl only waits for 30secs.  This is an over-estimate, but I just
#want the searches to run.

my $blastio; 
eval {
  $blastio = Bio::SearchIO::blast->new( -format => 'blast',
  										-wait => 600,
  										-fh   => \*STDIN,
				  						-signif => 0.001);
};


#Check that this is true in the sense that zero hits dies..........!
if( $@ ) {
  print "Error in parsing blast, please report this bug to Pfam and include your query sequence\n";
  exit;
}


#Now store the list of Pfam matches
my %pfamHits;
while( my $result = $blastio->next_result ) {
    while( my $hit = $result->next_hit ) {
    	if($hit->description){
    		my ($acc) = $hit->description =~ /(PF\d{5}\.\d+)/;
     		$pfamHits{$acc} = 1;		
     	}
     }
}


#Now make a mini database of for the matching HMMs.....
if(%pfamHits){
	my( $tmpFh, $tmpFile ) = tempfile( DIR => $td );
  	foreach my $pfamA (keys %pfamHits){
		#Get the HMM from the flatfile
		open(HMM,"$hmmfetch $hmmdb $pfamA |") ||
			die "Failed to get open hmmfetch pipe, $hmmfetch $hmmdb $pfamA:[$!] \n";
  		while(<HMM>) {
    		print $tmpFh $_;
  		}
  		close(HMM) || 
  			die "Could not clode hmmfetch pipe:[$!]\n";;
	}
	close $tmpFh;

	eval{
		open(GENE,"$genewise -pretty -pfam $tmpFile $td/$file | ") || 
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
		unlink("$td/$file");
	}
}else {
	print "No Pfam matches found\n";
	exit;
}

