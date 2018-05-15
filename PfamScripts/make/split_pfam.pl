#! /usr/bin/perl -w 

use strict;
use Getopt::Long;

# This script can help to split a Pfam family based on structural domain information from
# ECOD. Could probably make into a productivity script where user can check alignments.

# To do:
# 1) Check for each domain whether it is hit by the current HMM.
# 2) Add an option to extend domains as well.
# 3) Attempt an automatic splitting of the alignments
# 4) Allow user to select a domain which keeps the original accession.

# Usage $0: <pfam acc> <pdb acc:chain> -wholeseq

my $wholeseq;
&GetOptions('wholeseq!' => \$wholeseq, # This option will do a wholeseq on SEED alignment
    );

my $pfam_acc = shift @ARGV;
my $pdb_acc_chain= shift @ARGV;
my ($pdb_acc,$chain);
if ($pdb_acc_chain=~ /(\S{4}):(\S)/){
    $pdb_acc=$1;
    $chain=$2;
} else {
    die "Cannot parse pdb identifier and chain $pdb_acc_chain";
}

# Get accessions from ecod analysis output file
#my $file='/homes/agb/ECOD/output10';
#open (FH, $file) or die "cannot open $file";
#while(<FH>){
#    if (/OVERLAP for \S{4}:\S.*$pfam_acc/){#

#    }
#}
#close FH;

# Ask user for a PDB accession to try - when I move to being a productivity script. Otherwise users provides on command line

# Fetch sequences from ecod fasta file

my $file='/homes/agb/Work/ECOD/ecod.develop210.fasta.txt';
my $go;
open (FH, $file) or die "Cannot open $file"; 
my $test=$pdb_acc.$chain;
print STDERR "Attempting to split $pfam_acc using PDB $pdb_acc chain $chain\n";
system ("pfco $pfam_acc");

open (FASTA, "> $pfam_acc/$$.fasta") or die "Cannot write to $pfam_acc/$$.fasta";
my $n=0; # Counts number of domains
while(<FH>){
    if (/^>.*$test/i){
	print FASTA;
	$go=1;
	$n++;
	print STDERR "Domain $n ",$_;
    } elsif (/>/){
	$go=0;
    } else {
	if ($go){
	    print FASTA;
	}
    }
}
close FH;
close FASTA;

print STDERR "Found $n domains\n";

# Align them to SEED alignment of Pfam family
chdir "$pfam_acc";

# Wholeseq SEED of -wholeseq option was used
if ($wholeseq){
    print STDERR "Remaking SEED using wholeseq\n";
    system ("wholeseq.pl -align SEED -m > SEED.whole");
    system ("cp SEED.whole SEED");

    print STDERR "Overwriting old HMM with new one based on new SEED\n";
    system ("hmmbuild HMM SEED");

}

system ("hmmalign --mapali SEED -o SEED.sto HMM $$.fasta");
system("grep -v '#=G' SEED.sto > SEED.new");

# No create dirs for each domains
for (my $i=1; $i<=$n; $i++){
    mkdir "$i";
    system ("cp DESC $i/");
    system ("cp SEED.new $i/SEEDNEW");
    print STDERR "Please view alignment for domain $i and save as SEED\n";
    system ("belvu $i/SEEDNEW");
}


# Run pfbuilds
for (my $i=1; $i<=$n; $i++){
    print STDERR "Now running pfbuilds\n";
    if (-s "$i/SEED"){
	chdir "$i";
	print STDERR "Running pfbuild on domain $i\n";
	system ("pfbuild -local -withpfmake");
	chdir "..";
    }
}

chdir "..";
# Ask user to generate new SEED alignments. and select which domain will retain existing name. Perhaps do that by saving that alignment as SEED

# Copy DESC files into new directories and run pfbuilds.
