#!/software/bin/perl -w

use strict;
use Getopt::Long;
use Rfam::RfamAlign;

my( $fontsize, 
    $lines, 
    $fitpage,
    $landscape,
    $width,
    $help );

GetOptions( "s=s" => \$fontsize,
	    "l=s" => \$lines,
	    "fit" => \$fitpage,
	    "landscape" => \$landscape,
	    "width=s" => \$width,
	    "h"   => \$help );

sub usage {
    print STDERR <<EOF;
Usage: $0 <options> <stkfile>

Take a stockholm alignment with secondary structure markup and render
a postscript page for printing.

Options:   -h            show this help
           -s <n>        set fontsize to <n> points (default 10)
           -l <n>        set number of lines on the page
           --fit         attempt to break pages at block boundaries
	   --landscape   rotate the page
	   --width       set the number of columns

EOF
}

my $infile = shift;

if( $help ) {
    &usage();
    exit(0);
}

if( !$infile ) {
    &usage();
    print STDERR "FATAL: you must specify an alignment file\n";
    exit(1);
}

open( F, $infile ) or do {
    &usage();
    print STDERR "FATAL: can't open infile [$infile]\n";
    exit(1);
};

my $aln = Rfam::RfamAlign->new();
$aln->read_stockholm( \*F );
if( !$aln->ss_cons() ) {
    &usage();
    print STDERR "FATAL: alignment doesn't seem to have any structure markup\n";
    exit(1);
}

my @args = ( '-fh' => \*STDOUT );
push( @args, '-fontsize' => $fontsize ) if $fontsize;	     
push( @args, '-fitpage' => 1 )          if $fitpage;	     
push( @args, '-lines' => $lines )       if $lines;	     
push( @args, '-landscape' => 1 )        if $landscape;  
push( @args, '-width' => $width )       if $width;    

$aln->write_coloured_ps( @args );
