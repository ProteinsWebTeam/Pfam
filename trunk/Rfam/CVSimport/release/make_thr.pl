#!/usr/local/bin/perl -w

# input Rfam.seed
# output Rfam.thr

use strict;

my( $acc, $id, $ga, $wi );

while(<>) {
    /^\#=GF\s+AC\s+(\S+)/ and do {
        $acc = $1;
	next;
    };
    /^\#=GF\s+ID\s+(\S+)/ and do { 
        $id = $1;
	next;
    };
    /^\#=GF\s+GA\s+(\S+)/ and do { 
        $ga = $1;
	next;
    };
    /^\#=GF\s+BM\s+cmsearch.*-W\s+(\d+)\s+/ and do { 
        $wi = $1;
	next;
    };
    /^\/\// and do {
	print "$acc\t$id\t$ga\t$wi\n";
    };
}
