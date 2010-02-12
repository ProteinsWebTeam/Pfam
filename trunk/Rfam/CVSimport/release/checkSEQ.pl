#!/usr/local/bin/perl -w

=head1 NAME

simple bit of code to check the output from getEMBLseq.pl
run over the rfamseq/10 directory will report on number of seqs and bp in each file and give a total as it goes.


=head1 DESCRIPTION


=head1 PARAMETERS



=head1 AUTHOR jd7@sanger.ac.uk

=cut


use strict;
use IO::File;


my $dir='/nfs/pfam_nfs/rfam/rfamseq/10';

my @files=glob("$dir/rfamseq10*");


my $Nseq=0;
my $Nbp=0;
foreach my $f (@files){
    print $f, "\t"; 
    my $fh = new IO::File;
    $fh->open("seqstat $f |" ) or die;
     while (my $l= $fh->getline()) {
         if ($l=~/Number.*\:\s+(\d+)/){
             print $1, "\t";
             $Nseq+=$1;
             
             
         }
         if ($l=~/Total.*\:\s+(\d+)/){
             print $1, "\t";
             $Nbp+=$1;
         }
     }                 
    print " Nseq= $Nseq Nbp= $Nbp\n"; 
}#end of parsing each file

print "Final counts= Nseqs= $Nseq and Nbp= $Nbp in", scalar@files, "files\n";

