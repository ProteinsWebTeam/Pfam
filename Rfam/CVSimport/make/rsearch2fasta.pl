#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;

use lib "/pfam/db/Pfam/scripts/Modules";
use lib "/pfam/db/Rfam/scripts/Modules";
use CMResults;
use Rfam;

my( $ethr, $bthr );
&GetOptions( "t=s" => \$bthr,
	     "e=s" => \$ethr );

my $file = shift;
open( R, $file ) or die;
my $res = new CMResults;
$res -> parse_rsearch( \*R );
if( $ethr ) {
    $bthr = $res -> domain_bits_cutoff_from_evalue( $ethr );
}
my $newres = $res -> filter_on_cutoff( $bthr );
open( L, ">$$.list" ) or die;
$newres -> write_list( \*L );
close L;

my $bpi = $Rfam::rfamseq_new_inx;
system "seq_get.pl -d $bpi -l $$.list" and die;
