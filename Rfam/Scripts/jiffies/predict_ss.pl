#!/usr/bin/env perl

#predict SS of a sequence file giving a choice of methods 
#and return output as a stockholm file.
#sequence file can be a single sequence (fasta) or an alignment (fasta or stockholm).

use IO::File;
use Getopt::Long;
use strict;
use warnings;
use Bio::Rfam::PredictSS;
use Bio::Rfam::Config;
use File::Slurp;
use File::Copy;

my $config = Bio::Rfam::Config->new;

my ($infile, $intype, $outfile, $ppfold, $rnafold, $rnaalifold, $cmfinder, $method, $mcount, $seqcount, $help);
&GetOptions('infile=s' => \$infile,
	    'outfile=s' => \$outfile,
            'p!' => \$ppfold,
	    'r!' => \$rnafold,
	    'ra!' => \$rnaalifold,
	    'c!' => \$cmfinder,
	    'h!' => \$help,
            );

if ($help){
#help
    &help;
}
if ($ppfold){
    $method = "ppfold";
    $mcount ++;
}
if ($rnafold){
    $method = "rnafold";
    $mcount++;
}
if ($rnaalifold){
    $method = "rnaalifold";
    $mcount ++;
}
if ($cmfinder){
    $method = "cmfinder";
    $mcount ++;
}

if ($mcount != 1){
    print "ERROR one method must be specified\n\n";
    &help;
}
if (!$infile){
    print "ERROR an infile must be specified\n\n";
    &help;
}
if (!$outfile){
    print "ERROR an outfile must be specified\n\n";
    &help;
}

#what format is the input file? - could also attempt a reformat to sto here
my @infile = read_file($infile);
if ($infile[0] =~ m/STOCKHOLM/){
    $intype = "stockholm";
    print "Input file $infile recognised as stockholm format......\n";
} elsif ($infile[0] =~ m/^>/){
    $intype = "fasta";
    print "Input file $infile recognised as fasta format........\n";
} else {
    print "ERROR: Input file type not recognised - file types accepted are fasta or stockholm\n";
    &help;
    die;
}


&Bio::Rfam::PredictSS::predict_ss($infile, $intype, $outfile, $method, $config->binLocation);

sub help {
    print STDERR "\nPredicts secondary structure from either a single RNA sequence or an alignment.\n\n";
    print STDERR "Required options: -infile <input file> -outfile <output file>\n";
    print STDERR "Plus ONE of the following secondary structure predication methods:\n\n";
    print STDERR "\t-p\tPPfold\n";
    print STDERR "\t-r\tRNAfold\n";
    print STDERR "\t-ra\tRNAalifold\n";
    print STDERR "\t-c\tCMfinder\n\n";
    print STDERR "For single sequences use PPfold, RNAfold or CMfinder.\n";
    print STDERR "For aligmnents use PPfold, RNAalifold or CMfinder.\n\n";
    print STDERR "The input file may be a stockholm file or a fasta file. For multiple sequences this MUST be an aligned fasta file.\n";
    print STDERR "The output is a stockholm format file.\n\n";
    exit(0);

}
