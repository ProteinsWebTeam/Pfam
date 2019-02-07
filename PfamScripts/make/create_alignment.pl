#!/usr/bin/env perl

use IO::File;
use Getopt::Long;
use strict;
use Bio::Pfam::AlignMethods;
use Bio::Pfam::Config;

my $prog = $0;

my ($fasta_file, $mafft, $muscle, $musclep, $method, $mcount);

&GetOptions('fasta=s' => \$fasta_file,
	    'm!' => \$mafft,	    
	    'mu!' => \$muscle,
	    'mup!'=> \$musclep,        
     );

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
  &Bio::Pfam::AlignMethods::help($prog);
}

if($mcount !=1) {
    die "More than one alignment method selected!\n";
}



if (!$fasta_file) {
  &Bio::Pfam::AlignMethods::help($prog);
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


my $config = Bio::Pfam::Config->new;      

# Read fasta file and put ref into scalars
my ($sequence, $description) = &Bio::Pfam::AlignMethods::read_fasta($fasta_file);


# Create alignment 
my %hash=&Bio::Pfam::AlignMethods::create_alignment($sequence, $description,$method,$fasta_file);


#Print alignment
&Bio::Pfam::AlignMethods::print_alignment(\%hash, $method);



 
