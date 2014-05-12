#!/usr/bin/env perl
# 
# seed2tree.pl: Read in an alignment, convert it to a2m format and call a tree-building program.
#
# 
use strict;
use Bio::Easel::MSA;

my $in_alifile = "";    # name of input alignment

my $usage;
$usage  = "ali_format.pl <alifile>\n";

if(scalar(@ARGV) != 1) { die $usage; }
($in_alifile) = @ARGV;

# open and validate file 
my $msa = Bio::Easel::MSA->new({
   fileLocation => $in_alifile 
});

$msa->write_msa("STDOUT", "a2m"); 
# replace "STDOUT" with name of a file to output to file, 

# PLACE TREE-BUILDING PROGRAM CALL HERE, IF YOU WANT

exit 0;
