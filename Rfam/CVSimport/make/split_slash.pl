#!/usr/local/bin/perl -w

# split a stockholm format file into accessioned files

use strict;

my $file = shift;

open( F, $file ) or die;
if( $file =~ /.*\/(\S+)$/ ) {
    $file = $1;
}
$/ = "//\n";
my $count = 0;
while(<F>) {
    my $outfile;
    if( /[\n^]AC\s+(\S+?)[\;\s]/ ) {
	$outfile = "$1.spl";
    }
    else {
	$count ++;
	$outfile = "$file.$count";
    }
    open( O, ">$outfile" ) or die;
    print O;
}
close F;
