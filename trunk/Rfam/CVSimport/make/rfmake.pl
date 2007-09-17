#!/software/bin/perl -w

use strict;
use Getopt::Long;
use Bio::SeqFetcher::xdget;
use CMResults;
use Rfam;

my( $thr, 
    $inxfile,
    $list,
    $help, 
    $cove,
#    $fasta,
    $trim,
    $overlaps );

&GetOptions( "t=s"      => \$thr,
	     "d=s"      => \$inxfile,
	     "l"        => \$list,
	     "overlaps" => \$overlaps,
	     "cove"     => \$cove,
#	     "fa=s"     => \$fasta,
	     "trim=s"   => \$trim,
	     "h"        => \$help );

if( $help ) {
    &help();
    exit(1);
}

not $inxfile and $inxfile = $Rfam::rfamseq;
my $seqinx = Bio::SeqFetcher::xdget->new( '-db' => [$inxfile] );

my $file = shift;

my $local;
if( not $list and not $overlaps ) {
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
}

my $already;
open( F, $file ) or die;
if( <F> =~ /^\# Rfam/ ) {
    $already = 1;
}
close F;
open( F, $file ) or die;

my $allres = new CMResults;

printf STDERR "Parsing infernal OUTPUT\n";
if( $cove ) {
    $allres -> parse_cove( \*F );
}
else {
    $allres -> parse_infernal( \*F );
}
close F;

if( $trim ) {
    $allres = $allres->filter_on_cutoff( $trim );
}

if( !$already or $trim ) {
    # write a rearranged and slimmed output file
    open( F, ">$file" ) or die;
    $allres -> write_output( \*F );
    close F;
}

my $res = $allres -> remove_overlaps();

if( $list ) {
    my $chunksize = 100;
    my $desclength = 35;
    my %desc;
    $thr = 0 if( not defined $thr );

    my @goodhits = grep{ $_->bits >= $thr } $res->eachHMMUnit();
    my @allnames = map{ $_->seqname } @goodhits;
    
    while( scalar @allnames ) {
	my $string = join( " ", splice( @allnames, 0, $chunksize ) );
	##we had to take the -d embl option out of here for the new version of pfetch
	open( P, "pfetch -a -D $string |" ) or die;
	while( <P> ) {
	    if( /^(\w+\s+)?(\w+)\.\d+\s+(.{1,$desclength})/ ) {
		$desc{$2} = $3;
	    }
	}
	close P or die "can't close pfetch pipe";
    }

    foreach my $unit ( sort { $b->bits <=> $a->bits } $res->eachHMMUnit() ) {
	my( $emblacc );
	if( $unit->seqname =~ /^(\S+)\.\d+$/ ) {
	    $emblacc = $1;
	}
	else {
	    $emblacc = $unit->seqname;
	}
	if( not exists $desc{$emblacc} ) {
	    $desc{$emblacc} = "no description available";
	}
	printf( "%-12s%-".$desclength."s%8d%8d%5d%5d%8s\n", $unit->seqname, $desc{$emblacc}, $unit->start_seq, $unit->end_seq, $unit->start_hmm, $unit->end_hmm, $unit->bits );
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

my $atleastonehit;
open( FA, ">$$.fa" ) or die;
open( SC, ">scores" ) or die;
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
	print SC $cmunit->bits, " $id/$start-$end\n";
	$atleastonehit = 1;
    }
}
close FA;
close SC;

if( !$atleastonehit ) {
    warn "no hits\n";
    exit(0);
}

my $options = "-o ALIGN";
if( $local ) {
    $options = "-l ".$options." --qdb";
}
else {
    $options .= " --hbanded";
}
printf STDERR "Running: cmalign $options CM $$.fa\n";
system "cmalign $options CM $$.fa" and die "failed to run cmalign";

my $tc_bits = $res -> lowest_true( $thr );
my $nc_bits = $res -> highest_noise( $thr );
$nc_bits = "undefined" if( $nc_bits == -100000 );    # hack!

printf STDERR "Updating DESC file\n";
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

    $reverse = 1 if( $end and $end < $start );
    $seqinx->options( '' );    #reset this

    if( $end ) {
	my $options = "";
        my( $getstart, $getend ) = ( $start, $end );
        if( $reverse ) {
            ( $getstart, $getend ) = ( $end, $start );
	    $options .= "-r ";
        }
	$options .= "-a $getstart -b $getend ";
	$seqinx->options( $options );
    }

    my $seq = new Bio::Seq;
    eval {
        $seq = $seqinx -> get_Seq_by_acc( $id );
    };
    if( $@ or not $seq ) {
        warn "$id not found in your seq db\n";
        return 0;       # failure
    }

    if( $end ) {
	$seq->id( $seq->id."/$start-$end" );
    }

    return $seq;
}


