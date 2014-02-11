#!/usr/bin/env perl
# 
# ali_format.pl: Read in an alignment and output it in four 
#                     different formats:
#                     1) Stockholm (wrapped)
#                     2) Stockholm (unwrapped)
#                     3) Fasta     (aligned)
#                     4) Fasta     (unaligned)
#
# EPN, Tue Feb 11 13:08:00 2014
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

# output four times in different formats
$msa->write_msa("STDOUT", "stockholm"); # Stockholm (wrapped)
$msa->write_msa("STDOUT", "pfam");      # Stockholm (unwrapped)
$msa->write_msa("STDOUT", "afa");       # Fasta     (aligned)
$msa->write_msa("STDOUT", "fasta");     # Fasta     (unaligned)
# replace "STDOUT" with name of a file to output to file, 
# currently there's no way to get alignment to return
# as a scalar

exit 0;
