#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";

}

use lib $bioperl_dir;
use lib $rfam_mod_dir;

use strict;
use Getopt::Long;
use Rfam;
use Bio::Index::Fasta;
use Bio::Tools::BPlite;
use Bio::SeqIO;
use Bio::Tools::Run::StandAloneBlast;
use Bio::SearchIO;
use Bio::SearchIO::Writer::TextResultWriter;

my $evalue = 10;

my( $division, $minidb, $help, $length, $listhits, @blastdb, $multiplex );
&GetOptions( "e=s"    => \$evalue,
	     "d=s"    => \$division,
	     "db=s@"  => \@blastdb,
	     "h"      => \$help,
	     "w=s"    => \$length,
	     "minidb" => \$minidb,
	     "l"      => \$listhits,
	     "mp"     => \$multiplex );

my $fafile = shift;

if( $help or not $fafile ) {
    print STDERR <<EOF;

rfamseq_blast.pl: blast a sequence against rfamseq

Usage:   rfamseq_blast.pl <options> <fastafile>
Options:       -h          show this help
               -d <div>    restrict search to EMBL division
               -e <n>      blast evalue threshold
               --minidb    build minidb of all blast hits
	       -w <n>      override sequence length window 
	       --mp        multiplex sequences (evalues slightly out)

EOF
exit(1);
}

my $blastdbdir = $Rfam::rfamseq_current_dir;
my $inxfile    = $Rfam::rfamseq_current_inx;

my $in = Bio::SeqIO -> new( '-file' => $fafile, '-format' => 'Fasta' );

my @seqs;
my $bigseq = Bio::Seq->new( '-id' => 'multiplex' ) if( $multiplex );
my $count;
while( my $seq = $in->next_seq() ) {
    $length = $seq->length() unless( defined $length );
    if( $multiplex ) {
	$bigseq->seq( $bigseq->seq()."NNNNNNNNNN".$seq->seq() );
	$count ++;
    }
    else { 
	push( @seqs, $seq );
    }
}

if( $multiplex ) {
    # Multiplex option concatenates query seqs for speedup.  Evalues are
    # dependent on query length, so we have to fudge the threshold.  This
    # is likely to be slightly out.

    push( @seqs, $bigseq );
    my $avlength = ( $bigseq->length()-($count*10) ) / $count;
    $evalue = $evalue * $bigseq->length() / $avlength;
}

my $seqinx;
eval {
    $seqinx = Bio::Index::Fasta->new( '-filename'    => $inxfile,
				      '-dbm_package' => 'DB_File' );
};
#if( not $seqinx or $@ ) {
#    warn "failed to get sequence index object\n$@";
#}
END { undef $seqinx; }   # stop bizarre seg faults

unless( @blastdb ) {
    my $glob;
    if( $division ) {
	@blastdb = glob( "$Rfam::rfamseq_current_dir/$division*.fa" );
    }
    else {
	@blastdb = glob( "$Rfam::rfamseq_current_dir/*.fa" );
    }
}


foreach my $db ( @blastdb ) {
    my %hitlist;

    my $factory = Bio::Tools::Run::StandAloneBlast->new( 'program'  => 'blastn',
							 'database' => $db,
							 'outfile'  => "/tmp/$$.blast",
							 'F'        => 'F',
							 'W'        => 7,
							 'b'        => 100000,
							 'v'        => 100000,
							 'e'        => $evalue );

    print STDERR "searching $db\n";

    my $seqio = Bio::SeqIO -> new( '-file' => $fafile, '-format' => 'Fasta' );
    foreach my $seq ( @seqs ) {
	my $report = $factory->blastall( $seq );
	while( my $result = $report->next_result ) {
	    unless( $listhits or $minidb ) {
		my $writer = new Bio::SearchIO::Writer::TextResultWriter();
		my $out = new Bio::SearchIO( -writer => $writer );
		$out->write_result( $result );
		next;
	    }
	    while( my $hit = $result->next_hit() ) {
		while( my $hsp = $hit->next_hsp() ) {
		    if( $listhits ) {
			print $hit->name, " ", $hsp->start('hit'), " ", $hsp->end('hit'), "\n";
		    }
		    else {
			push( @{ $hitlist{$hit->name} }, $hsp );
		    }
		}
	    }
	}
    }

    if( $minidb ) {
	foreach my $id ( keys %hitlist ) {
	    my @se;
	    my @hsplist = sort { $a->start('hit') <=> $b->start('hit') } @{ $hitlist{$id} };
	    while( my $hsp = shift @hsplist ) {
		my( $start, $end ) = ( $hsp->start('hit'), $hsp->end('hit') );
		$start = $start - $length;
		$end   = $end   + $length;
		$start = 1 if( $start < 1 );
		
		my $strand = 1;
		if( $hsp->strand('hit') ne $hsp->strand('query') ) {
		    $strand = -1;
		}

		# Merge overlapping regions - because hsps are sorted by start 
		# we only need to check if it overlaps with the last one
		if( scalar(@se) and $start <= $se[ scalar(@se)-1 ]->{'end'} ) {	 
                    # we have an overlap
		    if( $end >= $se[ scalar(@se)-1 ]->{'end'} ) {
                        # extend the end
			$se[ scalar(@se)-1 ]->{'end'} = $end;
#			print STDERR "$id/$start-$end overlap\n";
		    }
		    else {
			# ignore
#			print STDERR "$id/$start-$end unnecessary\n";
		    }
		}
		else {
		    push( @se, { 'start' => $start, 'end' => $end, 'strand' => $strand } );
#		    print STDERR "$id/$start-$end new\n";
		}
	    }
	    
	    my $faout = Bio::SeqIO -> new( '-format' => 'Fasta' );
	    foreach my $se ( @se ) {
		my $seq = &get_seq( $id, $se->{'start'}, $se->{'end'}, $se->{'strand'} );
		$faout -> write_seq( $seq );
	    }
	}
    }
}

#########

sub get_seq {
    # fixes start < 1 and end > length
    my $id     = shift;
    my $start  = shift;
    my $end    = shift;
    my $strand = shift;

    my $seq = new Bio::Seq;
    eval {
        $seq = $seqinx -> fetch( $id );
    };
    if( not $seq or $@ ) {
        warn "$id not found in your seq db\n$@";
        return 0;       # failure
    }
    my $length = $seq -> length();
    if( $start < 1 ) {
        $start = 1;
    }
    if( $end > $length ) {
        $end = $length;
    }
    my $truncseq = $seq -> trunc( $start, $end );
    if( $strand < 0 ) {
	$truncseq = $truncseq -> revcom();
	$truncseq -> id( "$id/$end-$start" );
    }
    else {
	$truncseq -> id( "$id/$start-$end" );
    }
    $truncseq -> desc( "" );
    return $truncseq;
}
