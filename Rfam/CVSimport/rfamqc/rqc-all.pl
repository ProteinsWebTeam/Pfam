#! /usr/local/bin/perl -w

# A program to run all checks

use strict;

foreach my $family (@ARGV){
    print STDERR "\nSPELL CHECK\n\n";
    system ("pqc-spell $family");
#    print STDERR "\nSEQUENCE CHECK\n\n";
#    system ("rqc-seqs $family");
    print STDERR "\nOVERLAP CHECK - ignoring $family\n\n";
    system ("rqc-overlap $family -i $family");
    print STDERR "FORMAT CHECK\n\n";
    system ("rqc-format $family");
}



