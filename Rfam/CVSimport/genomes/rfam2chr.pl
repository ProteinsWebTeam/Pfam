#!/software/bin/perl -w

use strict;

my $agp      = shift;
my $rfamfull = shift;
#my $rfamseqlist = shift;

#my %rfamseq;
#open( L, $rfamseqlist ) or die;
#while(<L>) {
#    if( /^(\S+)\.(\d+)/ ) {
#        $rfamseq{$1} = $2;
#    }
#}
#close L;

my %hits;
my( $rfamacc, $rfamid, $ready );
open( HIT, "gunzip -c $rfamfull|" ) or die;
while(<HIT>) {
    if( /^\#=GF\sAC\s+(RF\d+)/ ) {
	$rfamacc = $1;
	$ready = 2;
    }
    if( /^\#=GF\sID\s+(\S+)/ ) {
	$rfamid = $1;
    }
    if( /^\s*$/ ) {
	$ready --;
    }
    next if( /^\#/ );
    next if( $ready < 1 );
    if( my( $seq, $st, $en ) = /^(\S+)\/(\d+)-(\d+)\s+/ ) {
	push( @{$hits{ $seq }}, { 'acc' => $rfamacc,
				  'id'  => $rfamid,
				  'st'  => $st,
				  'en'  => $en,
			      } );
    }
}
close HIT;

my %agp;
my $chr;
open( AGP, $agp ) or die;
while(<AGP>) {
    if( /^AC\s+(\S+)/ ) {
	$chr = $1;
    }

    if( my( $chrst, $chren, $clone, $clst, $clen, $clstr ) = 
	/^GP\s+(\d+)\s+(\d+)\s+(\S+)\s+(\d+)\s+(\d+)\s+([+|-])/ ) {

	if( exists $hits{ $clone } ) {
	    foreach my $hit ( sort{ $a->{'st'} <=> $b->{'en'} } @{$hits{ $clone }} ) {
		
		next if( $hit->{'st'} < $clst or 
			 $hit->{'en'} < $clst or 
			 $hit->{'st'} > $clen or 
			 $hit->{'en'} > $clen );
		
		my( $start, $end, $strand );
		if( $clstr eq "+" ) {
		    $start = $chrst - $clst + $hit->{'st'};
		    $end   = $chrst - $clst + $hit->{'en'};
		}
		else {
		    $start = $chrst + $clen - $hit->{'st'};
		    $end   = $chrst + $clen - $hit->{'en'};
		}
		
		
	printf( "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
		$chr,
	       	$start,
		$end,
		$clone,
		$hit->{'st'},
		$hit->{'en'},
		$hit->{'acc'},
		$hit->{'id'},
		);
	    }
	}
    }else {
	if (exists $hits{$chr}) {
	    print "This seq has no GP mappings $chr need to print out\n";
	}
    }
}
close AGP;
