#!/usr/bin/env perl

#create alignments from fasta file
#from Pfam software now adapted for Rfam use

use IO::File;
use Getopt::Long;
use strict;
use warnings;
use Bio::Rfam::AlignMethods;
use Bio::Rfam::Config;

my $prog = $0;

my ($fasta_file, $t_coffee, $mafft, $clustalw, $muscle, $musclep, $method, $mcount);

&GetOptions('fasta=s' => \$fasta_file,
            't!' => \$t_coffee,
	    'm!' => \$mafft,	    
	    'cl!' => \$clustalw,
 	    'mu!' => \$muscle,
	    'mup!'=> \$musclep,        
            );

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
if($muscle) {
  $method = "muscle";  
  $mcount++;
}
if($musclep) {
  $method = "musclep";
  $mcount++;
}
if($mcount == 0) {
  &Bio::Rfam::AlignMethods::help($prog);
}


if($mcount !=1) {
    die "More than one alignment method selected!\n";
}



if (!$fasta_file) {
  &Bio::Rfam::AlignMethods::help($prog);
}
if(! -s $fasta_file) {
  die "Your fasta file either does not exist or is of zero size\n";
}

#Remove // from end of file if it exists
system("grep -v \"^//\" $fasta_file > tmp.$$");
if(-s "tmp.$$") {
  unlink $fasta_file;
  rename ("tmp.$$", $fasta_file );
}
else {
  die "Could not remove // from end of file\n";
} 


my $config = Bio::Rfam::Config->new;     

# Read fasta file and put ref into scalars
my ($sequence, $description) = &Bio::Rfam::AlignMethods::read_fasta($fasta_file, $config->binLocation);


# Create alignment 

my %hash=&Bio::Rfam::AlignMethods::create_alignment($config->binLocation, $sequence, $description,$method,$fasta_file);

#Print alignment
&Bio::Rfam::AlignMethods::print_alignment(\%hash, $method,$config->binLocation);



 
