#!/software/bin/perl -w


=head1 NAME

checkDATA.pl

=head1 DESCRIPTION

simple bit of code to check the output from getEMBLdata_all.pl.
Checks that all the fields that should have content
warns which ones dont.
Note all fields should be present exept for previous acc.

Possibly should extend it to some format checking...

=head1 PARAMETERS

./checkDATA.pl Data_STD.ENV

=head1 AUTHOR jd7@sanger.ac.uk

=cut



use strict;


my $file=shift;

open(IN, "<$file") || die "cant open the infile";
my @lines=<IN>;
chomp @lines;
close (IN);

my @new;
foreach my $line (@lines){
    my @bits=split("\t", $line);
    my $c=@bits;
    if ($c !=8) {
        warn "EE $line not got 8 ($c) tab delm values\n";
    }
    my ($ac, $ver, $ncbi, $mol, $len, $de, $prev, $source)=@bits;
    #rearrange
    my $string=join("|", @bits);
    if (!$ac){die "problem with AC=> $string\n";}
    if (!$ver) { die "problem with VER=> $string";}
    if (!$ncbi){die "problem with NCBI=>$string";}
    if (!$mol) {die "problem with MOL=> $string";}
    if (!$len) {die "problem with LEN =>$string";}
    if (!$de) {warn "problem with de=> $string";}
    #often no $prev so no check for it.
    if (!$source) {die "problem with source $line";}   
 }
