#!/usr/local/bin/perl -w

use strict;
use lib '/pfam/db/Rfam/scripts/Modules';
use Rfam;

my $dir = shift;
if( $dir ) {
	chdir $dir or die "cannot chdir to $dir";
}

foreach my $acc ( &Rfam::get_allaccs() ) {
    system "rfco $acc" and do {
	warn "failed to checkout $acc";
	next;
    };
}
