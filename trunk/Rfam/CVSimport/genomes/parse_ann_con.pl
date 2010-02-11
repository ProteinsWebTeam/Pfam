#!/software/bin/perl -w

=head1 NAME

parse_ann_con.pl

=head1 DESCRIPTION

V. simple bit of code to parse the EMBL con and ann files and reduce
them down to minimal dataset. Greatly increases speed of later code.

=head1 PARAMETERS

parse_ann_con.pl <con file>

=head1 AUTHOR jd7@sanger.ac.uk

=cut

use strict;


my $infile=shift;

my $outfile=$infile.".mini";

open(IN, "<$infile") || die "Cant open the infile $!";
open(OUT, ">$outfile") ||  die "Cant open the outfile $!";
while (my $line=<IN>){

    if ($line=~/^ID|^AC|^DE|^OC|^OS|^CO|^\/\//){
	print OUT $line;
    }
}
close(IN);
