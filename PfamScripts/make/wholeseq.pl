#!/usr/bin/env perl 

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::AlignMethods;
use Bio::Pfam::Config;

my $prog = "wholeseq.pl";

my ($ali_file, $mafft, $muscle, $musclep, $method, $mcount);


&GetOptions("align=s" => \$ali_file,
	    "m!" => \$mafft,	    
	    "mu!" => \$muscle,
	    "mup!"=> \$musclep);





if (! $ali_file){
  &Bio::Pfam::AlignMethods::help($prog);
}


if($mafft) {
  $method = "mafft";
  $mcount++;
}
if($muscle) {
  $method = "muscle";
  $mcount++;
}
if($musclep) {
  $method = "musclep";
  $mcount++;
}

if($mcount == 0) {
  &iBio::Pfam::AlignMethods::help($prog);
}


if($mcount !=1) {
    die "More than one alignment method selected!\n";
}


# Make list file containing accession numbers
my %name;
open(ALI, "$ali_file")||die "Can't open $ali_file\n";
while(<ALI>){
    next if(/^\/\//);
    if (/^(\S+)\//){
	    $name{$1}=1;  # Assures seq is only used once!
    }
}


my ($original_seqno, $retrieved_seqno, %seqs);

foreach my $element (sort keys %name){ 
    $original_seqno++;
    #unless($element =~ /\d/) {
      #$element = &Bio::Pfam::AlignMethods::id2acc($element)   # If id number is present change to accession number
    #}
    if($element) {
      push(@{ $seqs{$element} }, { whole => 1 });
      $retrieved_seqno++;
    }
}


# Get sequences in fasta format
use Bio::Pfam::Config;
use Bio::Pfam::SeqFetch;

my $config = Bio::Pfam::Config->new;
open(F, ">FA.whole") or die "Could not open FA.whole for writing :[$!]\n";
Bio::Pfam::SeqFetch::fetchSeqs(\%seqs,$config->pfamseqLoc."/uniprot", \*F); 
close(F);


my $diff = $original_seqno - $retrieved_seqno;
print STDERR "Extending $retrieved_seqno out the $original_seqno sequences in alignment.\n";
if($diff != 0) { 
  print STDERR "Warning - $diff of the $original_seqno sequences in the original alignment were not retrieved\n";
} 

my $fasta = "FA.whole";

# Read fasta file and put ref into scalars
my ($sequence, $description) = &Bio::Pfam::AlignMethods::read_fasta($fasta);


# Create alignment 
my %hash=&Bio::Pfam::AlignMethods::create_alignment($sequence,$description,$method,$fasta);


#Print alignment
&Bio::Pfam::AlignMethods::print_alignment(\%hash, $method);


