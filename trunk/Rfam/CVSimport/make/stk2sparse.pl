#!/usr/local/bin/perl -w

use strict;
use Rfam::RfamAlign;

my $file = shift;

if( !$file ) {
    print STDERR <<EOF;
Usage: $0 <alignfile> <id>

       Rewrites an alignment in a sparse format.  The first
       sequence is shown in full, and then only differences
       from that sequences are shown in other sequences. "."
       means that the residue is identical to the reference
       sequence.  "-" is a gap with respect to the reference
       sequence.

       <id> is an optional argument to specify the reference.

EOF
    exit(1);
}

my $aln = Rfam::RfamAlign->new;
open( A, $file ) or die;
$aln->read_stockholm( \*A );
$aln->write_sparse( \*STDOUT, shift );
