#!/software/bin/perl

# input Rfam.full
# output Rfam.fasta (non-redundant to x%)

use strict;
use Getopt::Long;
use IO::File;

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
	my $fh = IO::File -> new();
	open( _TMP, ">tmp.aln" ) or die;
	foreach my $nse ( keys %seqs ) {
	    printf _TMP ( "%-32s%s\n", $nse, $seqs{$nse} );
	}
	close( _TMP ) or die "Could not close temp alignment file";
	if( $identity ) {
	    open( SAVERR, ">&STDERR" );
	    open( STDERR, "/dev/null" );
	    $fh -> open( "belvu -n $identity -o Mul tmp.aln |" );
	}
	else {
	    $fh -> open( "tmp.aln" );
	}
        while( <$fh> ) {
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
        close( $fh ) or die "Could not close filehandle";
        open( STDERR, ">&SAVERR" );
	unlink "tmp.aln" or die;
	undef %seqs;
    }
}
    
