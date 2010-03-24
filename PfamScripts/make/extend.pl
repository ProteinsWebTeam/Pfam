#!/software/bin/perl

unlink ("FA");
use strict;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::AlignMethods;

my $prog = $0;

my ($opt_align,$opt_n,$opt_c,$length, $t_coffee, $mafft, $clustalw, $probcons, $musclep, $muscle, $mcount, $mask, $pdb, $chain);

# Get options
&GetOptions('align=s'=> \$opt_align,
	    'n=i' => \$opt_n,
	    'c=i' => \$opt_c,
	    't!'  => \$t_coffee,
	    'm!'  => \$mafft,
	    'cl!' => \$clustalw,
	    'p!'  => \$probcons,
	    'mu!' => \$muscle,
	    'mup!'=> \$musclep,
            "ma!" => \$mask,
            'pdb=s' => \$pdb,
            'chain=s' => \$chain,
	    );


my $method;

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
      &AlignPfam::help($prog);
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

&Bio::Pfam::AlignMethods::extend_alignment($opt_align, $config->pfamseqLoc."/pfamseq", $opt_n, $opt_c) ; # prints extended sequences in fasta format to a file called FA


my $fasta = "FA";

# Read fasta file and put ref into scalars
my ($sequence, $description) = &Bio::Pfam::AlignMethods::read_fasta($fasta, $config->binLocation);


# Create alignment 
my %hash=&Bio::Pfam::AlignMethods::create_alignment($config->binLocation, $sequence,$description,$method,$fasta, $pdb, $chain);


#Print alignment
&Bio::Pfam::AlignMethods::print_alignment(\%hash, $method);
