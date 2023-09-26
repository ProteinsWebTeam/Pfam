#! /usr/bin/env perl

# This script takes a list of files and makes a list of (name, length)
# pairs, scanning for ID lines  

my( $seen,
    $len );

while(<>) {
    if( /^\/\// ) {
	$seen = 0;
	undef $len;
    }
    if( /^ID\s+\S+\s+\S+\s+(\S+) AA\.$/ ) {
	$len = $1;
    }
    if( /^AC\s+(\S+);/ and not $seen ) {
	$seen = 1;
	die "problem finding length for $1" if( not $len );
	print "$1 $len\n";
    }
}
