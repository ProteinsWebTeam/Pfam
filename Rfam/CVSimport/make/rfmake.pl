#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use lib '/nfs/disk100/pubseq/Pfam/bioperl';
use Bio::Index::Fasta;
use lib '/nfs/disk100/pubseq/Pfam/scripts/Modules';
use CMResults;

my( $thr, 
    $inxfile,
    $list,
    $help, 
    $cove,
    $overlaps );

&GetOptions( "t=s"      => \$thr,
	     "d=s"      => \$inxfile,
	     "l"        => \$list,
	     "overlaps" => \$overlaps,
	     "cove"     => \$cove,
	     "h"        => \$help );

if( $help ) {
    &help();
    exit(1);
}

not $inxfile and $inxfile = '/pfam/db/rfamseq/rfamseq.fa.bpi';
my $seqinx = Bio::Index::Fasta->new( $inxfile ); 

END {
    # truly wierd, but if I don't do this I get core dumps on program exit!
    undef $seqinx;
}

my $file = shift;

my $local;
#if( not defined $thr and not $list and not $overlaps ) {
open( DESC, "DESC" ) or warn "Can't open DESC to determine global/local requirement\n";
while( <DESC> ) {
    /^GA\s+(\S+)/ and do {
	$thr = $1 if not defined $thr;
    };
    /^BM\s+cmsearch.*-local.*/ and do {
	$local = 1;
    };
}
close DESC;

open( F, $file ) or die;
open( FA, ">$$.fa" ) or die;
   
my $allres = new CMResults;

if( $cove ) {
    $allres -> parse_cove( \*F );
}
else {
    $allres -> parse_infernal( \*F );
}

my $res = $allres -> remove_overlaps();

if( $list ) {
    foreach my $unit ( sort { $b->bits <=> $a->bits } $res->eachHMMUnit() ) {
	printf( "%-15s%8d%8d%8d%8d%10s\n", $unit->seqname, $unit->start_seq, $unit->end_seq, $unit->start_hmm, $unit->end_hmm, $unit->bits );
    }
    exit(0);
}
elsif( $overlaps ) {
    my @ols;
    foreach my $seq ( $res->eachHMMSequence() ) {
	foreach my $unit1 ( $seq->eachHMMUnit() ) {
	    foreach my $unit2 ( $seq->eachHMMUnit() ) {
#		    print "$unit1 $unit2\n";
		next if( $unit1->start_seq == $unit2->start_seq and 
			 $unit1->end_seq   == $unit2->end_seq and
			 $unit1->bits      == $unit2->bits );
		if( ( $unit1->start_seq >= $unit2->start_seq and $unit1->start_seq <= $unit2->end_seq ) or
		    ( $unit1->end_seq   >= $unit2->start_seq and $unit1->end_seq   <= $unit2->end_seq ) ) {
		    my( $score ) = sort { $a<=>$b } ( $unit1->bits, $unit2->bits );
		    push( @ols, { 'score' => $score, 'unit1' => $unit1, 'unit2' => $unit2 } );
		}
	    }
	}
    }
    
    foreach my $ol ( sort { $b->{'score'} <=> $a->{'score'} } @ols ) {
	my $unit1 = $ol->{'unit1'};
	my $unit2 = $ol->{'unit2'};
	
	printf( "%-15s%8d%8d%8d%8d%10s\n", $unit1->seqname, $unit1->start_seq, $unit1->end_seq, $unit1->start_hmm, $unit1->end_hmm, $unit1->bits );
	printf( "%-15s%8d%8d%8d%8d%10s\n\n", "", $unit2->start_seq, $unit2->end_seq, $unit2->start_hmm, $unit2->end_hmm, $unit2->bits );
    }
    exit(0);
}

foreach my $cmseq ( $res->eachHMMSequence() ) {
    foreach my $cmunit ( $cmseq->eachHMMUnit ) {
	next unless $cmunit->bits >= $thr;
	
	my $id    = $cmunit->seqname;
	my $start = $cmunit->start_seq;
	my $end   = $cmunit->end_seq;
	
	my $seq = &get_seq( $id, $start, $end );
	next unless $seq;
	my $seqstr = $seq->seq();
	$seqstr =~ tr/Tt/Uu/;                 # It's RNA dammit! (SRE)
	$seqstr =~ s/(.{1,60})/$1\n/g;
	print FA ">", $seq->id(), "\n$seqstr";
    }
}
close FA;

if( $local ) {
    system "cmalign -l -o ALIGN CM $$.fa" and die "failed to run cmalign";
}
else {
    system "cmalign -o ALIGN CM $$.fa" and die "failed to run cmalign";
}

my $tc_bits = $res -> lowest_true( $thr );
my $nc_bits = $res -> highest_noise( $thr );
$nc_bits = "undefined" if( $nc_bits == -100000 );    # hack!

if( -s "DESC" ) {
    open( DNEW, ">DESC.new" ) or die;
    open( DESC, "DESC" ) or die;
    while(<DESC>) {
	if( /^GA\s+/ ) {
	    printf DNEW ( "GA   %.2f\n", $thr );
	    next;
	}
	if( /^TC\s+/ ) {
	    printf DNEW ( "TC   %.2f\n", $tc_bits );
	    next;
	}
	if( /^NC\s+/ ) {
	    if( $nc_bits eq "undefined" ) {
		printf DNEW ( "NC   %s\n", $nc_bits );		
	    }
	    else {
		printf DNEW ( "NC   %.2f\n", $nc_bits );
	    }
	    next;
	}
	print DNEW $_;
    }
    close DESC;
    close DNEW;

    rename( "DESC", "DESC.old" ) or die;
    rename( "DESC.new", "DESC" ) or die;

}

#######################################

sub help {
    print STDERR <<EOF;
    rfmake.pl
    Usage:      rfmake.pl -t <bits> <output file>
	           -l option lists hits but does not build ALIGN
EOF
}

sub get_seq {
    my $id    = shift;
    my $start = shift;
    my $end   = shift;
    my $reverse;

    $reverse = 1 if( $end < $start );

    my $seq = new Bio::Seq;
    eval {
	$seq = $seqinx -> fetch( $id );
    };
    if( $@ or not $seq ) {
	warn "$id not found in your seq db\n";
	return 0;       # failure
    }
    
    my( $getstart, $getend );
    if( $reverse ) {
	( $getstart, $getend ) = ( $end, $start );
    }
    else {
	( $getstart, $getend ) = ( $start, $end );
    }
    
    my $truncseq = $seq -> trunc( $getstart, $getend );
    $truncseq -> desc( "" );
    $truncseq -> id( "$id/$start-$end" );

    if( $reverse ) {
	my $revseq = $truncseq -> revcom();
	$truncseq = $revseq;
    }

    return $truncseq;
}

