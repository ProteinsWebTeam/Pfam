#!/usr/local/bin/perl

# input Rfam.full
# output Rfam.fasta (non-redundant to x%)


use lib '/pfam/db/Rfam/scripts/Modules';
use lib '/nfs/disk100/pubseq/Pfam/bioperl';

use strict;
use Getopt::Long;

use Rfam;
use Bio::SimpleAlign;

my $identity;

&GetOptions( "i=i" => \$identity );

my( $id, $acc, %seqs );

while(<>) {
    /^\#=GF\s+AC\s+(\S+)/ and do {
        $acc = $1;
	next;
    };
    /^\#=GF\s+ID\s+(\S+)/ and do { 
        $id = $1;
	next;
    };
    /^(\S+\/\d+\-\d+)\s+(\S+)/ && do {
	$seqs{ $1 } .= $2;
	next;
    };
    /^\/\// and do {
        open( _TMP, ">tmp.aln" ) or die;
	foreach my $nse ( keys %seqs ) {
	    printf _TMP ( "%-32s%s\n", $nse, $seqs{$nse} );
	}
        close( _TMP ) or die "Could not close temp alignment file";
	open( SAVERR, ">&STDERR" );
        open( STDERR, "/dev/null" );
	open( BEL, "belvu -n $identity -o Mul tmp.aln |" )
            or die "Could not open command belvu -n $identity -o Mul tmp.aln";
        while(<BEL>) {
            /(\S+\/\d+\-\d+)\s+(\S+)/ && do {
                my( $name, $seq ) = ( $1, $2 );
                print ">$name $acc;$id;\n";
                $seq =~ s/[\.-]//g;
		$seq =~ tr/[acgtu]/[ACGTU]/;
		$seq =~ s/(.{1,60})/$1\n/g;
		chomp $seq;
                print "$seq\n";
            };
        }
        close( BEL ) or die "Could not close belvu command";
        open( STDERR, ">&SAVERR" );
	unlink "tmp.aln" or die;
	undef %seqs;
    }
}
    
