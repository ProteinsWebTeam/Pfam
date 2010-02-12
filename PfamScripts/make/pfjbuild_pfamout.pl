#!/usr/local/bin/perl

#Script to make PFAMOUT file from pfjbuild JOUT file

use strict;
use warnings;
use File::Copy;

use Bio::Pfam::HMM::HMMResultsIO;
use Bio::Pfam::FamilyIO;

my $file = shift;  #Location of JOUT filt
my $check = shift; #File prefix for jackhmmer checkpointing files


#Write results of final iteration to file
my ($header, $header_complete, $flag);
my $outfile = "JOUT.final";
open(JOUT, $file) or die "Couldn't open $file $!";
while(<JOUT>) {
    #Store header info
    if(!$header_complete) {
	if(/^\#/) {
	    $header .= $_;
	}
	elsif(/^Query\:/) {
	    $header .= "\n$_";
	}
	elsif(/^Description\:/) {
	    $header .= "$_\n";
	    $header_complete = 1;
	    }
    }
    #Print header and results for this iteration, overwriting any previous iterations
	elsif(/^Scores for complete sequences/) {
	    open(OUT, ">$outfile") or die "Couldn't open $outfile for writing $!";
	    print OUT $header;
	    print OUT $_;
	    $flag=1;
	}
    elsif($flag) {
	print OUT $_;
    }
}
close OUT;

#Create PFAMOUT style result file
my $hmmRes = Bio::Pfam::HMM::HMMResultsIO->new;
my $result =  $hmmRes->parseHMMER3($outfile);
$hmmRes->writePFAMOUT($result);
    
#Create DESC if there isn't one already
unless(-s 'DESC'){
    my $io = Bio::Pfam::FamilyIO->new;
    $io->writeEmptyDESC;
}

my @list = glob("*.hmm");
    

my $max=0;
foreach my $file (@list) {
	if($file =~ /$check\-(\d+)\.hmm/) {
	    $max = $1 if($1 > $max);
	}
    }

my $hmm_file = $check . "-" . $max . ".hmm";
copy($hmm_file, "HMM") or die "Couldn't copy $hmm_file to HMM $!";    




