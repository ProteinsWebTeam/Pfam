#!/usr/local/bin/perl -w

# performs seed surgery on Rfam families at rfamseq update time
# (also handy during familiy building!)

use strict;
use Getopt::Long;

use lib '/nfs/disk100/pubseq/Pfam/bioperl';
use Bio::SimpleAlign;
use Bio::SeqIO;
use Bio::Index::Fasta;
use Bio::Tools::BPlite2;
use lib '/pfam/db/Rfam/scripts/Modules';
use Bio::Rfam::RfamAlign;
use Rfam;

my $noaction;

&GetOptions( "noaction" => \$noaction );

my $dir = shift;
chdir $dir or die "can't chdir to $dir\n";
my $newinx = Bio::Index::Fasta->new( $rfamseq_new_inx );
my $curinx = Bio::Index::Fasta->new( $rfamseq_current_inx );
my %cache;

#open( LOG, ">log" ) or die "can't write to $dir/log\n";

foreach my $acc ( &Rfam::get_allaccs() ) {
    my $aln = new Bio::Rfam::RfamAlign;
    open( SEED, "$acc/SEED" ) or die;
    $aln -> read_stockholm( \*SEED );
    foreach my $seq ( $aln -> eachSeq() ) {
	my $curstr = $seq -> seq();
	$curstr =~ s/\.//g;
	my $newseq = &return_seq( $newinx, $seq->id, $seq->start, $seq->end );
	if( not defined $newseq ) {
	    printf( "%s   %-10s%10d%10d     ", $acc, $seq->id, $seq->start, $seq->end );
	    $aln -> removeSeq( $seq );
	    print "DELETE\tFIXED\n";
	}
	elsif( not $newseq ) {
	    printf( "%s   %-10s%10d%10d     ", $acc, $seq->id, $seq->start, $seq->end );
	    print "OUTOFBOUNDS\tNOTFIXED";
	}
	else {
	    my $newstr = $newseq -> seq;
	    $newstr =~ tr/T/U/; 
	    if( $newstr ne $curstr ) {
		printf( "%s   %-10s%10d%10d     ", $acc, $seq->id, $seq->start, $seq->end );
		my $fixed = &find_match( $newinx, $seq );
		if( $fixed == -1 ) {
		    print "RENUMBER\tFIXED\n";
		}
		elsif( $fixed ) {
		    print $fixed, "_MISMATCH\tFIXED\n";
		}
		else {
		    print "MISMATCH\tNOTFIXED\n";
		}
	    }
	}
    }

    unless( $noaction ) {
	open( NEW, ">$acc/SEEDNEW" ) or die "can't write to $acc/SEEDNEW";
	$aln -> write_stockholm( \*NEW );
	close NEW;
	# following will be unnecessary when infernal handles weights
	system "weight -o $acc/SEEDNEW2 $acc/SEEDNEW > /dev/null 2>&1" and die "can't run weight";
	rename( "$acc/SEED", "$acc/SEEDOLD" ) or die "can't rename SEED to SEEDOLD";
	rename( "$acc/SEEDNEW2", "$acc/SEED" ) or die "can't rename SEEDNEW2 to SEED";
    }
}

undef $newinx;
undef $curinx;

sub return_seq {
    my $inx  = shift;
    my $id   = shift;
    my $from = shift;
    my $to   = shift;

    eval {
	$cache{$id} = $inx -> fetch( $id ) unless exists $cache{$id};
    };
    if( $@ or not $cache{$id} ) {
#	warn "$id not found in your database: $@\n";
	return undef;
    }
    my $truncseq;
    if( not $from and not $to ) {
	$truncseq = $cache{$id};
    }
    else {
	eval {
	    if( $from > $to ) {
		$truncseq = $cache{$id} -> trunc( $to, $from );
		$truncseq = $truncseq -> revcom;
	    }
	    else {
		$truncseq = $cache{$id} -> trunc( $from, $to );
	    }
	};
	if( $@ or not $truncseq ) {
#	warn "$id: $from-$to are not reasonable bounds\n";
	    return 0;
	}
    }
    return $truncseq;
}


sub find_match {
    my $newinx = shift;
    my $seq    = shift;
    my $id     = $seq -> id;

    open( OUT, ">$$.new.fa" ) or die "can't write to $$.new.fa";
    my $out = new Bio::SeqIO( -fh => \*OUT, '-format' => 'Fasta' );
    my $newseq = $newinx -> fetch( $seq -> id );
    $out -> write_seq( $newseq );
    close OUT;

    open( OUT, ">$$.old.fa" ) or die "can't write to $$.old.fa";
    $seq -> display_id( $seq->id."/".$seq->start."-".$seq->end );
    $out = new Bio::SeqIO( -fh => \*OUT, '-format' => 'Fasta' );
    $out -> write_seq( $seq );
    close OUT;
    $seq -> display_id( $id );

    system "formatdb -i $$.new.fa -p F" and die "can't formatdb $$.new.fa";
    system "blastall -W8 -F F -d $$.new.fa -i $$.old.fa -p blastn > $$.blast" and die "can't run blastall";

    my $fixed;
    open( BLAST, "$$.blast" ) or die "can't open $$.blast";
    my $report = new Bio::Tools::BPlite2( \*BLAST );
    my $sbjct = $report->nextSbjct;
    my $hsp = $sbjct->nextHSP;     # just take the first hsp which should be the best hit
    if( $hsp -> match == $report->queryLength ) {
	$seq -> start( $hsp->sbjctBegin );
	$seq -> end( $hsp->sbjctEnd );
	$fixed = -1;
    }
    elsif( $hsp->length > ( $hsp->queryEnd - $hsp->queryBegin + 1 ) ) {
	# inserts in subject sequence -- can't deal easily
	$fixed = 0;
    }
    else {
	# map old -> new

	my @oldgap = split( //, $seq -> seq );
	my @newgap;
	my $subaln = $hsp -> sbjctAlignment;
	$subaln =~ tr/acgt-/ACGU\./;
	my @subgap = split( //, $subaln );
	my $j=0;
	for( my $k=1; $k<$hsp->queryBegin; $k++ ) {
	    unshift( @subgap, "." );
	}
	for( my $i=0; $i<@oldgap; $i++ ) {
	    if( $oldgap[$i] !~ /[ACTUGNactugn]/ ) {
		push( @newgap, "." );
	    }
	    elsif( $j>@subgap ) {
		push( @newgap, "." );
	    }
	    else {
		push( @newgap, $subgap[$j] );
		$j++
	    }
	}

	$seq -> start( $hsp->sbjctBegin );
	$seq -> end( $hsp->sbjctEnd );
	$seq -> seq( join( "", @newgap ) );
	$fixed = ( $report->queryLength - $hsp -> match );

#	print "\n", join( "", @oldgap ), "\n", join( "", @newgap ), "\n";
    }

    unlink( "$$.new.fa", "$$.old.fa", "$$.new.fa.nhr", "$$.new.fa.nin", "$$.new.fa.nsq", "$$.blast" ) or die "can't cleanup";

    return $fixed;
}
