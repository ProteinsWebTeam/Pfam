#!/software/bin/perl

# $0 -d <seq database index file>  [-g <gdf file>  or -l <list file> or -s <seq name> ]
# Fetch a sequence from a wu-blast indexed sequence database.
# This script has had various incarnations written by Alex and Sam.  This 
# has been optomised for speed (rdf).

use strict;
use warnings;
use Getopt::Long;

use Bio::Pfam::Config;
use Bio::Pfam::SeqFetch;

my($gdffile, $help, $listfile, $seq_name, $seq_beg, $seq_end, $index, $debug);

GetOptions (
	'g=s'  => \$gdffile,
	'h'    => \$help,
	'l=s'  => \$listfile,
	's=s'  => \$seq_name,
	'b=s'  => \$seq_beg,
	'e=s'  => \$seq_end,
	'd=s'  => \$index,
	'debug' => \$debug);

if($index && $debug){
  print STDERR "Using index $index\n";
}

my $pfamseqinx;
if (defined $index){
  $pfamseqinx = $index;
}else{
  my $config = Bio::Pfam::Config->new; 
  $pfamseqinx = $config->pfamseqLoc."/pfamseq"
  
}

$debug && print STDERR $pfamseqinx."\n";

if ($help){
  &help;
}

my (%seqList, $noSeqsRequested, $noSeqsFound); 


if ($seq_end and not $seq_beg){ # Set up start point is end point is specified
  $seq_beg=1;
}

if (defined $seq_name){
  #Catch someone giving it ids
  die "Sequence name contains invalid characters" if ($seq_name =~ /\_/);  
  $debug && print STDERR "Adding $seq_name to list of sequences to be fetched\n";
  Bio::Pfam::SeqFetch::addSeq($seq_name,$seq_beg,$seq_end, \%seqList);
  $noSeqsRequested = 1;
}elsif (defined $listfile){
  $debug && print STDERR "Adding list of sequences to be fetched from $listfile\n";
  open (LIST, "$listfile")||die "Can't open $listfile list file\n";
  $noSeqsRequested = Bio::Pfam::SeqFetch::parseList(\*LIST, \%seqList);
  close (LIST);
}elsif($gdffile){
  $debug && print STDERR "Adding list of sequences to be fetched from $gdffile\n";
  open(GDFFILE,$gdffile) || die "Error opening gdf file $gdffile: [$!]\n";
  $noSeqsRequested = Bio::Pfam::SeqFetch::parseGDF(\*GDFFILE, \%seqList);
  close (GDFFILE) || die "Error closing gdf file:[$!]\n";
}

$debug && print STDERR "Fetching sequences\n";
$noSeqsFound = &Bio::Pfam::SeqFetch::fetchSeqs(\%seqList, $pfamseqinx);

if($noSeqsFound != $noSeqsRequested){
  die "Did not find all the sequences you requested: Requested $noSeqsRequested; Got $noSeqsFound\n";
} 

sub help {
    print STDERR << "EOF";
Sequence retreival tool

seq_get.pl -d <sequence database>
  -h         for help
  -d file    database in fasta format. Must be wu-blast indexed!
  -debug     Switches on comments
  
  Then one of the following:
   
  -g file       gdf format file
  
  or
  
  -l file       list of sequence names
  
  or
  
  -s accession  retrieve single sequence
 
  and (optionally, or it will get the whole sequence) 
  
  -b number  retrieve single sequence from this residue. Must specify -e also!
  -e number  retrieve single sequence to this residue. Must specify -b also!
  
  to get a region.

Example 1:
 To get a sequence using a Swiss-Prot accession
 seq_get.pl -d sw33 -s P00789 
 seq_get.pl -d sw33 -s P00789 -b 23 -e 196

Example 2:
  To get subsequences with a gdf file
  
  seq_get.pl -d pfamseq -g test.gdf
  
Example 3:
  To get subsequences with a list file (Format <seqAcc>/<start>-<end> or <seqAcc>)
  
  seq_get.pl -d pfamseq -g test.gdf

EOF

    exit 0;
}


