#!/usr/local/bin/perl -w

BEGIN {
    $pfam_mod_dir = 
        (defined $ENV{'PFAM_MOD_DIR'})
            ?$ENV{'PFAM_MOD_DIR'}:"/nfs/disk100/pubseq/Pfam/scripts";
    $bioperl_dir = 
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/nfs/disk100/pubseq/Pfam/bioperl";
}

use lib $pfam_mod_dir;
use lib $bioperl_dir;

use strict;
use Getopt::Long;
use Bio::Tools::BPlite;
use Bio::SeqIO;
use CMResults;

my $arch = `uname`;
if( $arch =~ /linux/i ) {
    $ENV{'PATH'} = "/pfam/db/Rfam/bin/linux:$ENV{'PATH'}"; # push linux binaries onto front of path
}

my( $local, $blast_dir );
&GetOptions( "local" => \$local,
	     "db=s"  => \$blast_dir );

my $fafile       = shift;

not $blast_dir and $blast_dir = "/pfam/db/Rfam/BLASTDB";
#my $blast2_bin   = "/usr/local/ensembl/bin";
#my $infernal_bin = "/usr/local/ensembl/bin";
my $model_dir    = "$blast_dir";
my $blastdb      = "$blast_dir/Rfam.fasta";
my $thr_file     = "$blast_dir/Rfam.thr";
my $blastcut     = 10;
my $blastcmd     = "blastall -p blastn -e $blastcut -d $blastdb -i $fafile > $$.blast";

# read threshold file
my %thr;
open( T, $thr_file ) or die;
while(<T>) {
    if( /^(RF\d+)\s+(\S+)\s+(\S+)\s+(\d+)\s*$/ ) {
	$thr{ $1 } = { 'id' => $2, 'thr' => $3, 'win' => $4 };
    }
}
close T;

# read fasta file
my %seqs;
my $maxidlength = 0;
my $in = Bio::SeqIO -> new( -file => $fafile, '-format' => 'Fasta' );
while( my $seq = $in->next_seq() ) {
    $seqs{ $seq->id() } = $seq;
    $maxidlength = length( $seq->id() ) if( length( $seq->id() ) > $maxidlength );
}

my $error;

system "$blastcmd" and die;
my %results = %{ &parse_blast( "$$.blast" ) };
foreach my $acc ( keys %results ) {
    my $id = $thr{ $acc } -> { 'id' };
    open( O, ">$$.seq" ) or die;
    my $out = Bio::SeqIO -> new( -fh => \*O, '-format' => 'Fasta' );
	
    foreach my $seqid ( keys %{ $results{ $acc } } ) {
	foreach my $hit ( @{ $results{ $acc } -> { $seqid } } ) {
	    my( $start, $end, $score, $subject ) = ( $hit -> { 'start' },
						     $hit -> { 'end' },
						     $hit -> { 'score' },
						     $hit -> { 'subject' } );
#	    print "$acc $seqid $start $end $subject\n";
	    my $newseq = $seqs{$seqid} -> trunc( $start, $end );
	    $newseq -> display_id( "$seqid/$start-$end" );
	    $out -> write_seq( $newseq );
	}
    }

    close O;

    die if( not -s "$$.seq" );

    my $options = "-W ".$thr{$acc}{'win'};
    $options   .= " --local" if $local;
    system "cmsearch $options $model_dir/$acc.cm $$.seq > $$.res" and do {
	warn "$acc search failed";
	open( TMP, "$$.seq" ) or die;
	while( <TMP> ) {
	    if( /^\>/ ) {
		warn "Sequence:\n$_\n";
	    }
	}
	close TMP;
	$error ++;
    };
    
    open( RES, "$$.res" ) or die;
    my $res = new CMResults;
    $res -> parse_infernal( \*RES );
    $res = $res -> remove_overlaps();
    $res = $res -> filter_on_cutoff( $thr{$acc}->{'thr'} );
    
    foreach my $unit ( sort { $b->bits <=> $a->bits } $res->eachHMMUnit() ) {
	printf( "%-".$maxidlength."s%8d%8d%10s%8d%8d%10s\t%-10s\n", $unit->seqname, $unit->start_seq, $unit->end_seq, $acc, $unit->start_hmm, $unit->end_hmm, $unit->bits, $id );
    }
}

#unlink( "$$.res", "$$.seq", "$$.blast" ) or die;

if( $error ) {
    die "$error errors -- exiting\n";
}


sub parse_blast {
    my $blastfile = shift;
    my %hits;
    open( BL, $blastfile ) or die;
    my $report = new Bio::Tools::BPlite( -fh => \*BL );
    {
        while( my $sbjct = $report -> nextSbjct ) {
            $_ = $sbjct -> name();
	    my( $subject, $acc, $id ) = /^(\S+).*(RF\d+)\;(\S+)\;/;
            while( my $hsp = $sbjct->nextHSP ) {
		my( $start, $end, $score ) = ( $hsp->query->start, 
					       $hsp->query->end,
					       $hsp->bits );
		$_ = $hsp->query->seqname;
		my( $name ) = /^(\S+)/;
		my $win     = $thr{$acc}->{'win'};
		my $length  = $seqs{$name}->length;

		$start -= $win;
		$end   += $win;
		$start  = 1       if( $start < 1 );
		$end    = $length if( $end   > $length );

                my $already;
		if( exists $hits{ $acc } -> { $name } ) {
		    foreach my $se ( sort @{ $hits{ $acc } -> { $name } } ) {
                        if( $se->{'start'} >= $start and $se->{'start'} <= $end ) {
                            $se->{'start'} = $start;
                            $already = 1;
                        }
                        if( $se->{'end'} >= $start and $se->{'end'} <= $end ) {
                            $se->{'end'} = $end;
                            $already = 1;
                        }
                        if( $se->{'start'} <= $start and $se->{'end'} >= $end ) {
                            $already = 1;
                        }
                    }
                }

                unless( $already ) {
		    push( @{ $hits{ $acc } -> { $name } }, { 'subject' => $subject,
							     'start' => $start, 
							     'end' => $end, 
							     'score' => $score } );
		}
#		print "$name $subject $start $end $score\n";
	    }
	}
        last if ( $report -> _parseHeader == -1 );
        redo;
    }
    return \%hits;
}
