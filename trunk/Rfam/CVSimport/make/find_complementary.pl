#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use Bio::SeqIO;
use Bio::SearchIO;

my( $help );
my $strand = "f";   # default

GetOptions( "h"        => \$help,
	    "strand=s" => \$strand );

# args in wublast order
my $targetfile = shift;
my $queryfile = shift;

if( $help or !$queryfile ) {
    &usage();
    exit(0);
}

if( $strand !~ /^[b|f|r]$/ ) {
    &usage();
    warn "FATAL: unknown --strand argument [$strand]\n";
    exit(1);
}

sub usage {
    print STDERR <<EOF;
$0

Find regions of complementarity to the query using WU-BLAST and a custom scoring matrix.

Usage: $0 <target> <query>
Options:   -h                   show this help
           --strand [b|f|r]     search [b]oth, [f]orward or [r]everse strand (default f)

EOF
}


# we need to reverse the query sequence (but not complement)
my $qin = Bio::SeqIO -> new( -file => $queryfile, -format => 'Fasta' );
my $qout = Bio::SeqIO -> new( -file => ">$$.query.rev", -format => 'Fasta' );
while( my $seq = $qin->next_seq() ) {
    my @rev = reverse( split( //, $seq->seq ) );
    $seq->seq( join( "", @rev ) );
    $qout->write_seq( $seq );
}
undef $qout;
undef $qin;

my @targetfiles;

if( $strand =~ /[b|f]/ ) {
    # search the top strand
     push( @targetfiles, "$targetfile" );
}

if( $strand =~ /[b|r]/ ) {
    # we need the reverse strand
    my $tin  = Bio::SeqIO -> new( '-file' => $targetfile,
				  '-format' => 'Fasta' );
    my $tout = Bio::SeqIO -> new( '-file' => ">$$.target.rc",
				  '-format' => 'Fasta' );
    while( my $seq = $tin->next_seq() ) {
	my $rc = $seq->revcom();
	$rc->id( $seq->id."_rc" );
	$tout->write_seq( $rc );
    }
    undef $tout;
    push( @targetfiles, "$$.target.rc" );
}

# write out the matrix - otherwise I can't make wublast find it!
&dump_matrix();

foreach my $targetfile ( @targetfiles ) {
    system "xdformat -p $targetfile > /dev/null 2>&1" and die "failed to run xdformat";
    system "wublastp $targetfile $$.query.rev -matrix matrix -span1 -warnings > $$.blast" and die "failed to run wublastp";

    my $in = Bio::SearchIO -> new( '-file' => "$$.blast",
				   '-format' => 'Blast' );

    while( my $result = $in->next_result() ) {
	while( my $hit = $result->next_hit() ) {
	    while( my $hsp = $hit->next_hsp() ) {
		# print a summary line
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
		
		# fudge the homology line to use | for A=T, G=C and : for G=U
		my @homol = split( //, $hsp->homology_string() );
		for( my $i=0; $i<@homol; $i++) {
		    my $hitbase = substr( $hsp->hit_string(), $i, 1 );
		    my $querybase = substr( $hsp->query_string(), $i, 1 );
		    if( ($querybase eq "G" and $hitbase eq "T") or ($querybase eq "T" and $hitbase eq "G") ) {
			$homol[$i] = ":";
		    }
		}
		my $homol = join( "", @homol );
		$homol =~ s/\+/\|/g;

		# remember these as a running count
		my $hen = $hsp->start('hit') - 1;
		my $qen = $hsp->end('query') + 1;

		# print the alignments out nicely
		for( my $i=0; $i<$hsp->hsp_length(); $i=$i+60 ) {
		    my $hsub = substr( $hsp->hit_string(), $i, 60 );
		    $hsub =~ s/-//g;
		    my $hst = $hen + 1;
		    $hen = $hst + length($hsub) - 1;
		    $hen = $hsp->end('hit') if( $hen > $hsp->end('hit') );
		    
		    my $qsub = substr( $hsp->query_string(), $i, 60 );
		    $qsub =~ s/-//g;
		    my $qst = $qen - 1;
		    $qen = $qst - length($qsub) + 1;
		    $qen = $hsp->start('query') if( $qen < $hsp->start('query') );
		    
		    printf( "%10d  %-60s  %-10d\n", 
			    $qst, 
			    substr( $hsp->query_string(), $i, 60 ),
			    $qen,
			    );
		    printf( "%10s  %-60s\n", "",
			    substr( $homol, $i, 60 ) 
			    );
		    printf( "%10d  %-60s  %-10d\n", 
			    $hst, 
			    substr( $hsp->hit_string(), $i, 60 ),
			    $hen,
			    );
		    print "\n";
		}
	    }
	}
    }
}



sub dump_matrix {
    open( MAT, ">matrix" ) or die "can't write to matrix";
    print MAT <<EOF;
    A   T   G   C   S   W   R   Y   K   M   B   V   H   D   N
A  -4   5  -4  -4  -4   1   1  -4  -4   1  -4  -1  -1  -1  -2
T   5  -4   3  -4  -4   1  -4   1   1  -4  -1  -4  -1  -1  -2
G  -4   3  -4   5   1  -4   1  -4   1  -4  -1  -1  -4  -1  -2
C  -4  -4   5  -4   1  -4  -4   1  -4   1  -1  -1  -1  -4  -2
S  -4  -4   1   1  -4  -4  -2  -2  -2  -2  -1  -1  -3  -3  -1
W   1   1  -4  -4  -4  -4  -2  -2  -2  -2  -3  -3  -1  -1  -1
R   1  -4   1  -4  -2  -2  -4  -4  -2  -2  -3  -1  -3  -1  -1
Y  -4   1  -4   1  -2  -2  -4  -4  -2  -2  -1  -3  -1  -3  -1
K  -4   1   1  -4  -2  -2  -2  -2  -4  -4  -1  -3  -3  -1  -1
M   1  -4  -4   1  -2  -2  -2  -2  -4  -4  -3  -1  -1  -3  -1
B  -4  -1  -1  -1  -1  -3  -3  -1  -1  -3  -4  -2  -2  -2  -1
V  -1  -4  -1  -1  -1  -3  -1  -3  -3  -1  -2  -4  -2  -2  -1
H  -1  -1  -4  -1  -3  -1  -3  -1  -3  -1  -2  -2  -4  -2  -1  
D  -1  -1  -1  -4  -3  -1  -1  -3  -1  -3  -2  -2  -2  -4  -1
N  -2  -2  -2  -2  -1  -1  -1  -1  -1  -1  -1  -1  -1  -1  -4
EOF
}
