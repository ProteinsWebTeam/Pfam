#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $pfam_mod_dir = 
        (defined $ENV{'PFAM_MODULES_DIR'})
            ?$ENV{'PFAM_MODULES_DIR'}:"/pfam/db/Pfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $rfam_mod_dir;
use lib $pfam_mod_dir;
use lib $bioperl_dir;
use strict;
use Getopt::Long;

use Rfam;
use Rfam::RfamAlign;
use CMResults;

my( $agp, $con, $search, @add, $clean, $outfile );
&GetOptions( "agp"     => \$agp,
	     "con"     => \$con,
	     "search"  => \$search,
	     "add=s@"  => \@add,
	     "clean=s" => \$clean,        # clean up a load of results files from previous run
	     "o=s"     => \$outfile,
	     );

my $file = shift;
my $rfamseqlist = '/pfam/db/Rfam/RELEASES/CURRENT/Rfamseq.lst';

sub help {
    print STDERR <<EOF;
Usage:  $0 [--agp|--con] [--search] [--add resultsfile] file
EOF
}

if( not $agp and not $con ) {
    &help();
    exit(1);
}

if( $search and not $outfile ) {
    &help();
    print STDERR "You need to specify an outfile if you use the --search option\n";
    exit(1);
}

#my %ignore = qw( AE000521 1
#                 AE013600 1
#                 AE014293 1
#                 AE014294 1
#                 BA000025 1
#                 BA000027 1
#                 AL672111 1
#                 );

my %rfamseq;
open( L, $rfamseqlist ) or die;
while(<L>) {
    if( /^(\S+)\.(\d+)/ ) {
	$rfamseq{$1} = $2;
    }
}
close L;

# if we need to do a cmblast step on some stuff which isn't
# in rfamseq then we don't bother with this step the first 
# time round
my $res = new CMResults;
unless( $search ) {
    my $db = Rfam::default_db();
    my @family_list = $db->get_allacc();
    foreach my $rfacc ( @family_list ) {
	my $rfid = $db->acc2id( $rfacc );
	open( S, "$Rfam::current_dir/$rfacc/scores" ) or die;
	while(<S>) {
	    if( my( $score, $id, $start, $end ) = /^(\S+)\s+(\S+)\/(\d+)-(\d+)/ ) {
		unless( $res -> getHMMSequence( $id ) ) {
		    my $seq = new HMMSequence;
		    $seq -> name( $id );
		    $res -> addHMMSequence( $seq );
		}
		
		my $unit = new HMMUnit;
		$unit -> seqname( $id );
		$unit -> hmmname( $rfid );
		$unit -> hmmacc( $rfacc );
		$unit -> start_seq( $start );
		$unit -> end_seq( $end );
		$unit -> bits( $score );
		
		$res -> addHMMUnit( $unit );
	    }
	}
	close S;
    }
}

if( $clean ) {
    push( @add, "$clean.*.out" );
}

foreach my $add ( @add ) {
    open( A, $add ) or die;
    while(<A>) {
	if( my( $id, $ver, $start, $end, $rfacc, $bits, $rfid ) = 
	    /(\S+)\.(\d+)\s+(\d+)\s+(\d+)\s+(\S+)\s+\d+\s+\d+\s+(\S+)\s+(\S+)/ ) {

	    $rfamseq{$id} = $ver;

	    unless( $res -> getHMMSequence( $id ) ) {
		my $seq = new HMMSequence;
		$seq -> name( $id );
		$res -> addHMMSequence( $seq );
	    }

	    my $unit = new HMMUnit;
	    $unit -> seqname( $id );
	    $unit -> hmmname( $rfid );
	    $unit -> hmmacc( $rfacc );
	    $unit -> start_seq( $start );
	    $unit -> end_seq( $end );
	    $unit -> bits( $bits );
	    
	    $res -> addHMMUnit( $unit );
	}
    }
    close A;
    unlink $add if $clean;
}

my %search;
my $ignore;
open( E, $file ) or die;
if( $agp ) {
    while(<E>) {
	if( /^\S+\s+\d+\s+\d+\s+\d+\s+\S+\s+(\S+)\.(\d+)\s+(\d+)\s+(\d+)/ ) {
	    my( $id, $ver, $st, $en ) = ( $1, $2, $3, $4 );
	    if( $search and ( not exists $rfamseq{$id} or $ver != $rfamseq{$id} ) ) {
		$search{ $id.".".$ver } = 1;
	    }
	    elsif( not exists $rfamseq{$id} ) {
		print STDERR "$id\.$ver/$st-$en not found in rfamseq\n" unless $clean;
	    }
	    elsif( $ver != $rfamseq{$id} ) {
		print STDERR "$id\.$ver/$st-$en version mismatch\n" unless $clean;
	    }
	    elsif( not $search ) {
		print_hits( $id, $st, $en );
	    }
	}
    }
    print "\/\/\n" unless $search;
}
elsif( $con ) {
    while(<E>) {
	if( /^AC\s+(\S+)\;/ ) {
	    print $_ unless $search;
	}
	if( /^DE/ or /^OC/ ) {
	    print $_ unless $search;
	}
	if( /^\/\// ) {
	    print $_ unless $search;
	}
	if( my( $stuff ) = /^CO\s+(.*)/ ) {
	    while( /([A-Z0-9]+)\.(\d+)\:(\d+)\.\.(\d+)/g ) {
		my( $id, $ver, $st, $en ) = ( $1, $2, $3, $4 );
		if( $search and ( not exists $rfamseq{$id} or $ver != $rfamseq{$id} ) ) {
		    $search{ $id.".".$ver } = 1;
		}
		elsif( not exists $rfamseq{$id} ) {
		    print STDERR "$id\.$ver/$st-$en not found in rfamseq\n" unless $clean;
		}
		elsif( $ver != $rfamseq{$id} ) {
		    print STDERR "$id\.$ver/$st-$en version mismatch\n" unless $clean;
		}
		elsif( not $search ) {
		    print_hits( $id, $st, $en );
		}
	    }
	}
    }
}

if( $search ) {
    my $jid = $$;
    my $options;
    if( $agp ) {
	$options = "--agp";
    }
    elsif( $con ) {
	$options = "--con";
    }
    if( my @search = keys %search ) {
	my $j = @search;
	for( my $i=1; $i<=@search; $i++ ) {
	    system "pfetch $search[$i-1] -n $search[$i-1] > $jid.$i.fa" and die;
	}
	system "echo 'cmblast.pl $jid.\$\{LSB_JOBINDEX\}.fa > $jid.\$\{LSB_JOBINDEX\}.out' | bsub -q pfam_slow -Rlinux -o $jid.\%I.err -Jrf$jid\"[1-$j]\"" and die; 

	# the clever bit - run this script again after the array
	# above has finished and clearing up after myself
	system "bsub -q pfam_slow -m pfam -w'done(rf$jid)' -o genome2rfam.err \"$0 --clean $jid $options $file > $outfile\"" and die;
    }
    else {
	system "$0 $options $file > $outfile" and die;
    }	
}

########

sub print_hits {
    my( $seqid, $constart, $conend ) = @_;
    if( my $seq = $res -> getHMMSequence( $seqid ) ) {
	foreach my $unit ( sort { $a->start_seq <=> $b->start_seq } $seq -> eachHMMUnit() ) {
	    if( ($unit->start_seq > $constart and $unit->end_seq > $constart) or
		($unit->start_seq < $conend   and $unit->end_seq < $conend) ) {
		printf( "RF   %-26s %-10s %-16s %7s\n", $unit->seqname."/".$unit->start_seq."-".$unit->end_seq, $unit->hmmacc, $unit->hmmname, $unit->bits );
	    }
	}
    }
}
