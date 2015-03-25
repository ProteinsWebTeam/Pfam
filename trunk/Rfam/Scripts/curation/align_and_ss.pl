#!/usr/bin/env perl

#runs alignment methods using create_alignment.pl and SS prediction methods using predict_ss.pl

use Getopt::Long;
use strict;
use warnings;
use File::Copy::Recursive qw(fcopy rcopy dircopy fmove rmove dirmove);
use Bio::Rfam::AlignMethods;
use Bio::Rfam::Config;
use Data::Dumper;

my $infile;
my $config = Bio::Rfam::Config->new;  

my $dir = "SS_pred/";;
if (!-d "SS_pred"){
#make directory for ss prediction
    mkdir $dir;
} else {
    print STDERR "Directory SS_pred already exists. Moving to SS_pred_$$\n";
    dirmove("SS_pred", "SS_pred_$$") or die "Cannot move directory $!\n";
    mkdir $dir;
}

&GetOptions('infile=s' => \$infile,
            );

if (!$infile){
    die "Must specify an input file in fasta format\n";
}

#run alignment methods
my %align_meth = (
    'm' => 'mafft',
    'mu' => 'muscle',
    't' => 't_coffee',
    'cl' => 'clustalw',
    );

#run ss methods
my %ss_meth = (
    'ra' => 'RNAalifold',
    'p' => 'PPfold',
    'c' => 'cmfinder',
    );

#check input is fasta format and contains 2 or more seqs (use read_fasta here)
my ($sequence, $description) = &Bio::Rfam::AlignMethods::read_fasta($infile, $config->binLocation);
my $count = @$sequence;
if ($count < 2) {
    die "ERROR: Please select a fasta file containing a minimum of two sequences\n";
}


#align sequences with each method - this was duplicates read_alignment - would add a save_alignment method to AlignMethods.pm and call read, align and save methods seperately instead of using create_alignment.pl here

foreach my $alimeth (keys %align_meth){
    my $outfile_ali = $dir . $infile . "_" . $align_meth{$alimeth} . ".sto";
    print STDERR "Aligning with $align_meth{$alimeth}......\n";
# Create alignment 

    my %hash=&Bio::Rfam::AlignMethods::create_alignment($config->binLocation, $sequence, $description, $align_meth{$alimeth}, $infile);

#print alignment to file
    &Bio::Rfam::AlignMethods::save_alignment(\%hash, $alimeth,$config->binLocation, $outfile_ali);

#for each alignment predict ss with each method
    foreach my $ssmeth (keys %ss_meth){
	my $outfile_ss = $dir . $infile . "_" . $align_meth{$alimeth} . "_" . $ss_meth{$ssmeth} . ".sto";
	print STDERR "\tPredicting structure with $ss_meth{$ssmeth}....\n";
	system ("/nfs/production/xfam/rfam/production_software/rfam_production/Rfam/Scripts/curation/predict_ss.pl -infile $outfile_ali -outfile $outfile_ss -$ssmeth") and die "Cannot predict secondary structure for $outfile_ali with $ssmeth$!\n";
    }

}
