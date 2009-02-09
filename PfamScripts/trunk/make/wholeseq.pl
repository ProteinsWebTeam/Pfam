#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use Bio::Pfam::AlignMethods;

my $prog = "wholeseq.pl";

my ($ali_file, $t_coffee, $mafft, $clustalw, $probcons, $muscle, $musclep, $method, $mcount, $mask, $pdb, $chain);


&GetOptions("align=s" => \$ali_file,
            "t!" => \$t_coffee,
	    "m!" => \$mafft,	    
	    "cl!" => \$clustalw,
	    "p!"  => \$probcons,
	    "mu!" => \$muscle,
	    "mup!"=> \$musclep,
            "ma!" => \$mask,
            'pdb=s' => \$pdb,
            'chain=s' => \$chain, );





if (! $ali_file){
  &Bio::Pfam::AlignMethods::help($prog);
}


if($t_coffee) {
  $method = "t_coffee";
  $mcount++;
}
if($mafft) {
  $method = "mafft";
  $mcount++;
}
if($clustalw) {
  $method = "clustalw";
  $mcount++;
}
if($probcons) {
  $method = "probcons";
  $mcount++;
}
if($mask) {
  $method = "mask";
  $mcount++;
  if(!$pdb) {
      &iBio::Pfam::AlignPfam::help($prog);
  }
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
    if (/^(\S+)\//){
	    $name{$1}=1;  # Assures seq is only used once!
    }
}


my ($original_seqno, $retrieved_seqno, %seqs);

foreach my $element (sort keys %name){ 
    print STDERR $element;
    $original_seqno++;
    unless($element =~ /\d/) {
      #$element = &Bio::Pfam::AlignMethods::id2acc($element)   # If id number is present change to accession number
    }
    if($element) {
      push(@{ $seqs{$element} }, { whole => 1 });
      $retrieved_seqno++;
    }
}


# Get sequences in fasta format
use Bio::Pfam::Config;
use Bio::Pfam::SeqFetch;

my $config = Bio::Pfam::Config->new;
#system ("seq_get.pl -l list -d ".$config->pfamseqLoc."/pfamseq -nodesc > FA") and die "Failed to run seq_get.pl";        
open(F, ">FA.whole") or die "Could not open FA.whole for writing :[$!]\n";

Bio::Pfam::SeqFetch::fetchSeqs(\%seqs,$config->pfamseqLoc."/pfamseq", \*F); 
close(F);


my $diff = $original_seqno - $retrieved_seqno;
print STDERR "Extending $retrieved_seqno out the $original_seqno sequences in alignment.\n";
if($diff != 0) { 
  print STDERR "Warning - $diff of the $original_seqno sequences in the original alignment were not retrieved\n";
} 



my $fasta = "FA.whole";

# Read fasta file and put ref into scalars
my @foo = &AlignMethods::read_fasta($fasta);
my @sequence    = @{shift @foo};
my @description = @{shift @foo};


# Create alignment 
my %hash=&AlignMethods::create_alignment(\@sequence,\@description,$method,$fasta, $pdb, $chain);


#Print alignment
&AlignMethods::print_alignment(\%hash, $method);


