#! /software/bin/perl -w

# A program to run all checks

use strict;

foreach my $family (@ARGV){
    print STDERR "\n**SPELL CHECK**\n\n";
    system ("rqc-spell $family");
    print STDERR "\n**OVERLAP CHECK** - ignoring $family\n\n";
    system ("rqc-overlap-rdb $family -i $family");
    print STDERR "\n**FORMAT CHECK**\n\n";
    system ("rqc-format $family");
    print STDERR "\n**STRUCTURE CHECK**\n\n";
    system ("rqc-ss-cons $family");
    print STDERR "\n**MISSING CHECK**\n\n";
    system ("rqc-check $family");
    print STDERR "\n**SEQUENCE CHECK**\n\n";
    system ("rqc-seqs $family");
}



