#!/usr/bin/env perl

#parses tblout files produced by cmsearch into bigBed files for genome browser hub
#use: tblout2bigBed.pl infile db
#infile is name of input tblout file, db is assembly name (from UCSC)

use strict;
use warnings;
use Data::Dumper;
use DDP;

my $infile = $ARGV[0];
my $db = $ARGV[1];
my $bedfile = $infile . ".bed";

#create BED format file
#open bed file and write header
open (BED, ">$bedfile") or die "Cannot open file $bedfile $!\n";

#open infile and parse

open (IN, "$infile") or die "Cannot open file $infile $!\n";

while (<IN>){

#skip lines beginning #
	unless ($_ =~ /^#/){
		my @data = split(/\s+/,$_);
#need to ignore those where chrom is non-standard
		if ($data[0] =~ /chr\w{1,2}_\S+/){
			next;
		}
#start must be lower than end - so the two need reversing if strand = '-'
		if ($data[9] eq '+'){
	   	  	print BED "$data[0]\t$data[7]\t$data[8]\t$data[2]\n";
		} elsif ($data[9] eq '-'){
	   		print BED "$data[0]\t$data[8]\t$data[7]\t$data[2]\n";
		} else {
	    		print "Strand character unrecognised in line: $_";
		}

	}

} #end of loop through infile
close (IN);
close (BED);

#sort BED file chrom then chromStart: sort -k1,1 -k2,2n unsorted.bed > input.bed

my $sortedfile = $infile . "_sorted.bed";
system("sort -k1,1 -k2,2n $bedfile > $sortedfile");



#use fetchChromSizes to create chrom.sizes file
system("/nfs/production/xfam/rfam/software/fetchChromSizes $db > chrom.sizes") and die "Could not create chrom.sizes for $db $!\n";


#use bedToBigBed to convert BED to bigBed
my $bigbedfile = $infile . ".bb";
system ("/nfs/production/xfam/rfam/software/bedToBigBed $sortedfile chrom.sizes $bigbedfile") and die "Could not convert BED to bigBed $!\n";
