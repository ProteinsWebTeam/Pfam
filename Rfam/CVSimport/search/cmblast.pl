#!/usr/local/bin/perl -w

BEGIN {
    $pfam_mod_dir = 
        (defined $ENV{'PFAM_MOD_DIR'})
            ?$ENV{'PFAM_MOD_DIR'}:"/pfam/db/Pfam/scripts/Modules";
    $bioperl_dir = 
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $pfam_mod_dir;
use lib $bioperl_dir;

use strict;
use Getopt::Long;
use Bio::Tools::BPlite;
use Bio::SeqIO;
use CMResults;

my $arch = `uname`;
if( $arch =~ /linux/i ) {     # if we're running on the blades
    $ENV{'PATH'} = "/pfam/db/Rfam/bin/linux:$ENV{'PATH'}"; # push linux binaries onto front of path
}

my( $local, 
    $global,
    $blast_dir, 
    $family_acc,
    $blastdb,
    $thresh,
    $blastcut,
    $noclean,
    $help );

&GetOptions( "local"   => \$local,
	     "global"  => \$global,
	     "db=s"    => \$blast_dir,
	     "acc=s"   => \$family_acc,
	     "fadb=s"  => \$blastdb,
	     "t=s"     => \$thresh,
	     "bt=s"    => \$blastcut,
	     "noclean" => \$noclean,
	     "h"       => \$help );

my $fafile = shift;

if( $help or not $fafile ) {
        print STDERR <<EOF;

$0: search a DNA fasta file against Rfam

Usage: $0 <options> fasta_file
    Options
        -h            : show this help

    Expert options
	-local        : perform local mode search  (default is Rfam mode)
	-global       : perform global mode search (       -- \" --      )
	-db <dir>     : specify directory location of Rfam database
	-acc <acc>    : search against only a single family
	-fadb <file>  : use alternative fasta db
	-t <bits>     : specify cutoff in bits
	-bt <bits>    : specify blast evalue cutoff
        
    Output format is:
        <seq id> <seq start> <seq end> <rfam acc> <model start> <model end> <bit score> <rfam id>

    This search can be very slow for large RNA gene-rich DNA sequences.
    You should probably try different size chunks to find reasonable 
    search times.  As a guide, finding 12 tRNAs in a 2kb chunk of 
    sequence seems to take 2-3 mins.

EOF
exit(1);
}

not $blast_dir and $blast_dir = "/pfam/db/Rfam/BLASTDB";
not $blastdb   and $blastdb   = "$blast_dir/Rfam.fasta";
not $blastcut  and $blastcut  = 10;

my $model_dir    = "$blast_dir";
my $thr_file     = "$blast_dir/Rfam.thr";
my $blastcmd     = "/usr/local/ensembl/bin/blastall -p blastn -i $fafile -d $blastdb -e $blastcut -W7 -F F";

# read threshold file
my %thr;
open( T, $thr_file ) or die;
while(<T>) {
    if( /^(RF\d+)\s+(\S+)\s+(\S+)\s+(\d+)\s+(\S+)\s*$/ ) {
	$thr{ $1 } = { 'id' => $2, 'thr' => $3, 'win' => $4, 'mode' => $5 };
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

system "$blastcmd > /tmp/$$.blast" and die;
my %results = %{ &parse_blast( "/tmp/$$.blast" ) };
foreach my $acc ( keys %results ) {
    if( $family_acc ) {
	next unless( $family_acc eq $acc ); # do single family if $family_acc
    }

    my $id = $thr{ $acc } -> { 'id' };
    open( O, ">/tmp/$$.seq" ) or die;
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

    die if( not -s "/tmp/$$.seq" );

    my $options = "-W ".$thr{$acc}{'win'};
    if( $global ) {
	# don't use local mode
    }
    elsif( $local or $thr{$acc}{'mode'} =~ /local/) {
	$options .= " --local";
    }

#    print "$acc options: $options  cut ", $thr{$acc}->{'thr'}, "\n";

    system "cmsearch $options $model_dir/$acc.cm /tmp/$$.seq > /tmp/$$.res" and do {
	warn "$acc search failed";
	open( TMP, "/tmp/$$.seq" ) or die;
	while( <TMP> ) {
	    if( /^\>/ ) {
		warn "Sequence:\n$_\n";
	    }
	}
	close TMP;
	$error ++;
    };
    
    open( RES, "/tmp/$$.res" ) or die;
    my $res = new CMResults;
    $res -> parse_infernal( \*RES );
    $res = $res -> remove_overlaps();
    if( defined $thresh ) {
	$res = $res -> filter_on_cutoff( $thresh );
    }
    else {
	$res = $res -> filter_on_cutoff( $thr{$acc}->{'thr'} );
    }

    foreach my $unit ( sort { $b->bits <=> $a->bits } $res->eachHMMUnit() ) {
	printf( "%-".$maxidlength."s%8d%8d%10s%8d%8d%10s\t%-10s\n", $unit->seqname, $unit->start_seq, $unit->end_seq, $acc, $unit->start_hmm, $unit->end_hmm, $unit->bits, $id );
    }
}

unless( $noclean ) {
    unlink( "/tmp/$$.res", "/tmp/$$.seq", "/tmp/$$.blast" ) or die;
}

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
