#!/software/bin/perl

use lib ".";
use strict;
use warnings;
use Getopt::Long;

use HMMResultsIO;
use Bio::Pfam::SeqFetch;

my( $seqThrs, $domThrs, $evalue, $seqDB);

GetOptions (
  "e=s" => \$evalue,
  "T=s" => \$seqThrs,
  "t=s" => \$domThrs,
  "d=s" => \$seqDB
);

my $HMMResultsIO = HMMResultsIO->new;
my $HMMResults = $HMMResultsIO->parseHMMSearch( "PFAMOUT" );

if($evalue){
 $domThrs = $HMMResults->domain_bits_cutoff_from_evalue($evalue);
 $seqThrs = $domThrs if($domThrs); 
 print STDERR "Calculated bits thres = $domThrs\n";
}

open(SCORES, ">scores") or die "Could not open file GDF:[$!]\n";
if($domThrs and $seqThrs){
  $HMMResults->domThr($domThrs);
  $HMMResults->seqThr($seqThrs);
}else{
  $HMMResults->seqThr( 25.0 );
  $HMMResults->domThr( 25.0 );
}
$HMMResults->write_scores_file(\*SCORES);
close(SCORES);

my $fasta = "fa";
open(SC, "scores") || die "Error opening scores: [$!]\n";
open(FA,">$fasta") || die "Error opening fasta file $fasta: [$!]\n";

my %seqList;
my $noSeqsRequested = Bio::Pfam::SeqFetch::parseScores(\*SC, \%seqList);
close (SC) || die "Error closing gdf file:[$!]\n";

my $dbname = "/Users/rdf/Work/HMMER3/Data/23.0/pfamseq";
my $noSeqsFound = &Bio::Pfam::SeqFetch::fetchSeqs(\%seqList, $dbname, \*FA);
close(FA) || die "Error closing fasta file:[$!]\n";  

if($noSeqsFound != $noSeqsRequested){
  die "Did not find all the sequences you requested: Requested $noSeqsRequested; Got $noSeqsFound\n";
}  
                
#hack until we get H3 hmmalign
system("hmm2build -F HMM2 SEED");
system("hmm2align -q HMM2 fa > ALIGN");
