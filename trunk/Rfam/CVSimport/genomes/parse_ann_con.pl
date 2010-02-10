#!/software/bin/perl -w

use strict;


my $infile=shift;

# get the list of confiles 
#my @confiles = glob( "$con_ann/rel_*dat" );

my $outfile=$infile.".mini";

open(IN, "<$infile") || die "Cant open the infile $!";
open(OUT, ">$outfile") ||  die "Cant open the outfile $!";
while (my $line=<IN>){

    if ($line=~/^ID|^AC|^DE|^OC|^OS|^CO|^\/\//){
	print OUT $line;
    }
}
close(IN);
