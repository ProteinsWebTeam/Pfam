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
use IO::File;

use Rfam;
use Rfam::RfamAlign;
use CMResults;

use Bio::Tools::BPlite;

my( $agp, $con, $search, @add, $clean, $outfile );
&GetOptions( "agp"     => \$agp,
	     "con"     => \$con,
	     "search"  => \$search,
	     "add=s@"  => \@add,
	     "clean=s" => \$clean,        # clean up a load of results files from previous run
	     "o=s"     => \$outfile,
	     );

my $file = shift;
my $rfamseqlist = '/pfam/db/rfamseq/CURRENT/Rfamseq.lst';

sub help {
    print STDERR <<EOF;
Usage:  $0 [--agp|--con] [--search] [--add resultsfile] file

            --agp things do not work properly at the moment

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
    push( @add, glob("$clean.*.out") );
}

# read in the precalculated stuff
foreach my $add ( @add ) {
    open( A, $add ) or die;
    while(<A>) {
	if( my( $id, $ver, $st, $en, $start, $end, $rfacc, $bits, $rfid ) = 
	    /(\S+)\.(\d+)\/(\d+)-(\d+)\s+(\d+)\s+(\d+)\s+(\S+)\s+\d+\s+\d+\s+(\S+)\s+(\S+)/ ) {

	    $start = $st + $start - 1;
	    $end   = $st + $end - 1;

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
#    unlink $add if $clean;
}

my %search;
my $ignore;
open( E, $file ) or die "can't find your file [$file]\n";
my $seqs;
if( $agp ) {
    $seqs = read_agp( \*E );
}
elsif( $con ) {
    $seqs = read_cons( \*E );
}

foreach my $ctg ( keys %{$seqs} ) {
    if( not $search ) {
	print "AC   $ctg;\n";
	print "DE   ", $seqs->{$ctg}->{'DE'}, "\n" if( $seqs->{$ctg}->{'DE'} );
	print "OS   ", $seqs->{$ctg}->{'OS'}, "\n" if( $seqs->{$ctg}->{'OS'} );
	print "OC   ", $seqs->{$ctg}->{'OC'}, "\n" if( $seqs->{$ctg}->{'OC'} );
    }

    foreach my $seq ( @{ $seqs->{$ctg}->{'list'} } ) {
	my( $id, $ver, $st, $en ) = ( $seq->{'id'}, $seq->{'ver'}, $seq->{'start'}, $seq->{'end'} );

	if( not exists $rfamseq{$id} ) {
	    print STDERR "$id\.$ver/$st-$en not found in rfamseq\n" unless $clean;
	    $search{ "$id\.$ver/$st-$en" } = 1 if $search;
	    next;
	}

	if( $ver != $rfamseq{$id} ) {
	    my( $st2, $en2 ) = &mapseq( $id, $ver, $st, $en );
	    if( not $en2 ) {
		print STDERR "$id\.$ver/$st-$en version mismatch\n" unless $clean;
		$search{ "$id\.$ver/$st-$en" } = 1 if $search;
		next;
	    }
	    ( $st, $en ) = ( $st2, $en2 );
	}

	if( not $search ) {
	    print_hits( $id, $st, $en );
	}
    }
    if( not $search ) {
	print "\/\/\n";
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
	    my( $sv, $st, $en ) = $search[$i-1] =~ /(\S+)\/(\d+)-(\d+)/;
	    system "pfetch -a $sv:$st-$en -n $sv/$st-$en > $jid.$i.fa" and die;
	}
	system "echo 'cmblast.pl $jid.\$\{LSB_JOBINDEX\}.fa > $jid.\$\{LSB_JOBINDEX\}.out' | bsub -q pfam_slow -Rlinux -o $jid.\%I.err -Jrf$jid\"[1-$j]\"" and die; 

	# the clever bit - run this script again after the array
	# above has finished and clearing up after myself
	system "bsub -q pfam_slow -Ralpha -w'done(rf$jid)' -o genome2rfam.err \"$0 --clean $jid $options $file > $outfile\"" and die;
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


sub mapseq {
    my( $id, $ver, $st, $en ) = @_;
    my $fh = IO::File -> new();

  FETCH: open( O, ">$$.sub.fa" ) or die;
#    print STDERR "end $en\n";
    $fh -> open( "pfetch -a $id\.$ver:$st-$en -n $id\.$ver/$st-$en|" ) or die;
    while(<$fh>) {
	if( /end is greater than sequence length.*?(\d+)/ ) {
	    $en = $1;
#	    print O $_;
	    redo FETCH;
	}
	elsif( /no match/ ) {
#	    print O $_;
	    return 0;
	}
	else {
	    print O $_;
	}
    }
    system "pfetch -a $id\.$rfamseq{$id} -n $id\.$rfamseq{$id} > $$.whole.fa" and die;
			
    system "formatdb -i $$.whole.fa -p F" and die;
    system "blastall -F F -d $$.whole.fa -i $$.sub.fa -p blastn -e 0.000001 > $$.blast" and die;

    open( BLAST, "$$.blast" ) or die "can't open $$.blast";
    my $report = new Bio::Tools::BPlite( '-fh' => \*BLAST );
    my $best;
    {
	my $querylength = $report->qlength;
	while( my $sbjct = $report->nextSbjct ) {
	    while( my $hsp = $sbjct->nextHSP ) {
		if( (not $best or $hsp->bits >= $best->bits) ) {
		    $best = $hsp;
		}
	    }
	}
        last if ($report->_parseHeader == -1);
	redo;
    }
    close BLAST;

    if( $best ) {
	print STDERR "mapping $id\.$ver/$st-$en to $id\.$rfamseq{$id}/", $best->hit->start, "-", $best->hit->end, "\n";
	return( $st, $en );
    }
}

sub read_agp {
    my $fh = shift;
    my %seqs;
    while(<$fh>) {
	if( /^\S+\s+\d+\s+\d+\s+\d+\s+\S+\s+(\S+)\.(\d+)\s+(\d+)\s+(\d+)/ ) {
	    my( $id, $ver, $st, $en ) = ( $1, $2, $3, $4 );
	    push( @{ $seqs{'UNKNOWN'}->{'list'} }, { 'id'      => $id,
						     'ver'     => $ver,
						     'start'   => $st,
						     'end'     => $en } );
	}
    }
    return \%seqs;
}

sub read_cons {
    my $fh = shift;
    my( %seqs, $acc );
    while(<$fh>) {
	if( /^AC\s+(\S+)\;/ ) {
	    $acc = $1;
	}
	if( /^(DE)\s+(.*)/ or /^(OC)\s+(.*)/ or /^(OS)\s+(.*)/ ) {
	    $seqs{$acc}->{$1} .= "$2 ";
	}
	if( my( $stuff ) = /^CO\s+(.*)/ ) {
	    while( $stuff =~ /([A-Z0-9]+)\.(\d+)\:(\d+)\.\.(\d+)/g ) {
		my( $id, $ver, $st, $en ) = ( $1, $2, $3, $4 );
		push( @{ $seqs{$acc}->{'list'} }, { 'id'      => $id,
						    'ver'     => $ver,
						    'start'   => $st,
						    'end'     => $en } );
	    }
	}
    }
    return \%seqs;
}
