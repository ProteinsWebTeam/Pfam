#!/usr/local/bin/perl -w

use strict;

sub usage {
    print STDERR <<EOF;
Usage: $0 <alignfile> <outfile>
          - you must run this script on a linux box!
EOF
}

my $alnfile = shift;
my $outfile = shift;
if( not $outfile ) {
    &usage();
    exit(1);
}

my $pfold_bindir = '/pfam/db/Rfam/software/pfold/bin';
system "$pfold_bindir/fasta2col $alnfile | sed 's/arbitrary/RNA/g' > /tmp/$$.col" and die;
system "$pfold_bindir/findphyl $pfold_bindir/scfg.rate /tmp/$$.col > /tmp/$$.nj.col" and die;
system "$pfold_bindir/mltree $pfold_bindir/scfg.rate /tmp/$$.nj.col > /tmp/$$.ml.col" and die;
system "$pfold_bindir/scfg --treeinfile $pfold_bindir/article.grm /tmp/$$.ml.col > /tmp/$$.res.col" and die;
system "$pfold_bindir/addparen /tmp/$$.res.col | $pfold_bindir/col2fasta > $outfile" and die;
