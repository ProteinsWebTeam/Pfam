#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use Bio::SeqIO;
use Bio::SearchIO;

my $help;
GetOptions( "h"  => \$help );

# args in wublast order
my $targetfile = shift;
my $queryfile = shift;

if( $help or !$queryfile ) {
    &usage();
    exit(0);
}

sub usage {
    print <<EOF;
Usage: $0 <target> <query>
Options:    -h     show this help
EOF
}


# we need to reverse the query sequence (but not complement)
my $qin = Bio::SeqIO -> new( -file => $queryfile, -format => 'Fasta' );
my $qout = Bio::SeqIO -> new( -file => ">$queryfile.rev", -format => 'Fasta' );
while( my $seq = $qin->next_seq() ) {
    my @rev = reverse( split( //, $seq->seq ) );
    $seq->seq( join( "", @rev ) );
    $qout->write_seq( $seq );
}
undef $qout;
undef $qin;

$ENV{"WUBLASTMAT"} = "/pfam/db/Rfam/BLASTMAT";

system "xdformat -I -p $targetfile > /dev/null 2>&1" and die;
system "wublastp $targetfile $queryfile.rev -matrix COMP_MAT -span1 > $$.blast 2>/dev/null" and die;

my $in = Bio::SearchIO -> new( '-file' => "$$.blast",
			       '-format' => 'Blast' );
while( my $result = $in->next_result() ) {
    while( my $hit = $result->next_hit() ) {
	while( my $hsp = $hit->next_hsp() ) {
	    printf( ">%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n\n",
		    $result->query_name(),
		    $hsp->start('query'),
		    $hsp->end('query'),
		    $hit->name, 
		    $hsp->start('hit'),
		    $hsp->end('hit'),
		    $hsp->score(),
		    $hsp->evalue(),
		    );

	    my $hen = 0;
	    my $qen = $hsp->end('query') + 1;

	    for( my $i=0; $i<$hsp->hsp_length(); $i=$i+60 ) {
		my $hsub = substr( $hsp->hit_string(), $i, $i+60 );
		$hsub =~ s/-//g;
		my $hst = $hen + 1;
		$hen = $hst + length($hsub) - 1;
		$hen = $hsp->end('hit') if( $hen > $hsp->end('hit') );

		my $qsub = substr( $hsp->query_string(), $i, $i+60 );
		$qsub =~ s/-//g;
		my $qst = $qen - 1;
		$qen = $qst - length($qsub) + 1;
		$qen = $hsp->start('query') if( $qen < $hsp->start('query') );

		printf( "%10d  %-60s  %-10d\n", 
			$qst, 
			substr( $hsp->query_string(), $i, $i+60 ),
			$qen,
			);
		printf( "%10s  %-60s\n", "",
			substr( $hsp->homology_string(), $i, $i+60 ) 
			);
		printf( "%10d  %-60s  %-10d\n", 
			$hst, 
			substr( $hsp->hit_string(), $i, $i+60 ),
			$hen,
			);
		print "\n";
	    }
	}
    }
}
