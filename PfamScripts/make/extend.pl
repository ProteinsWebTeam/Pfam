#!/usr/bin/env perl

unlink ("FA");
use strict;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::AlignMethods;

my $prog = $0;

my ($opt_align,$opt_n,$opt_c,$length, $mafft, $musclep, $muscle, $mcount);

# Get options
&GetOptions('align=s'=> \$opt_align,
	    'n=i' => \$opt_n,
	    'c=i' => \$opt_c,
	    'm!'  => \$mafft,
	    'mu!' => \$muscle,
	    'mup!'=> \$musclep,
	    );


my $method;

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



if(! $opt_align){
    warn "Don't know what to extend!\n";
    &Bio::Pfam::AlignMethods::help($prog);
}

if (! $opt_n && ! $opt_c){
    warn "Don't know how far to extend!\n";
    &Bio::Pfam::AlignMethods::help(@_);
}


# If options not set use defaults
if (! $opt_n){
    $opt_n=0;
}
if (! $opt_c){
    $opt_c=0;
}

print STDERR "\nExtending $opt_align by:\n\n";
print STDERR "$opt_n residues at the N terminus\n";
print STDERR "$opt_c residues at the C terminus\n";


my $config = Bio::Pfam::Config->new;      
my $sequence_db;
my $uniprot = $config->{uniprot}->{location}."/uniprot";
if(-s $uniprot) {
  $sequence_db=$uniprot;
}
else {
  $sequence_db=$config->{pfamseq}->{location}."/pfamseq";
}
&Bio::Pfam::AlignMethods::extend_alignment($opt_align, $sequence_db, $opt_n, $opt_c) ; # prints extended sequences in fasta format to a file called FA


my $fasta = "FA";

# Read fasta file and put ref into scalars
my ($sequence, $description) = &Bio::Pfam::AlignMethods::read_fasta($fasta);


# Create alignment 
my %hash=&Bio::Pfam::AlignMethods::create_alignment($sequence,$description,$method,$fasta);


#Print alignment
&Bio::Pfam::AlignMethods::print_alignment(\%hash, $method, $description);
