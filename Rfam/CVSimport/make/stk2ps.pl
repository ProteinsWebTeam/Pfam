#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use Rfam::RfamAlign;

my( $fontsize, 
    $lines, 
    $fitpage,
    $help );

GetOptions( "s=s" => \$fontsize,
	    "l=s" => \$lines,
	    "fit" => \$fitpage,
	    "h"   => \$help );

my $infile = shift;

if( !$infile or $help ) {
    print <<EOF;
Usage: $0 <options> <stkfile>

Take a stockholm alignment with secondary structure markup and
render a postscript page for printing.

Options:   -h         show this help
           -s <n>     set fontsize to <n> points (default 10)
           -l <n>     set number of lines on the page
           --fit      attempt to break pages at block boundaries

EOF
    exit(0);
}

my @args = ( '-fh' => \*STDOUT );
push( @args, '-fontsize' => $fontsize ) if $fontsize;	     
push( @args, '-fitpage' => 1 )          if $fitpage;	     
push( @args, '-lines' => $lines )       if $lines;	     

open( F, $infile ) or die "can't open [$infile]\n";
my $aln = Rfam::RfamAlign->new();
$aln->read_stockholm( \*F );
$aln->write_coloured_ps( @args );
