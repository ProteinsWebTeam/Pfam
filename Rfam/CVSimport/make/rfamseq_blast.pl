#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $rfam_mod_dir;
use lib $bioperl_dir;

use strict;
use Getopt::Long;
use Rfam;
use Bio::Tools::BPlite;
use Bio::Index::Fasta;
use Bio::SeqIO;

my( $evalue, $division, $minidb, $help );
&GetOptions( "e=s"    => \$evalue,
	     "o=s"    => \$division,
	     "h"      => \$help,
	     "minidb" => \$minidb );

$evalue = 10 if( not $evalue );

my $fafile = shift;

if( $help or not $fafile ) {
    print STDERR <<EOF;

rfamseq_blast.pl: blast a sequence against rfamseq

Usage:   rfamseq_blast.pl <options> <fastafile>
Options:       -h          show this help
               -o <div>    restrict search to EMBL division
               -e <n>      blast evalue threshold
               --minidb    build minidb of all blast hits

EOF
exit(1);
}

my $blastdbdir = $rfamseq_current_dir;
my $inxfile    = $rfamseq_current_inx;

my $in = Bio::SeqIO -> new( '-file' => $fafile, '-format' => 'Fasta' );
my $length = $in -> next_seq() -> length();

my $seqinx  = Bio::Index::Fasta->new( $inxfile ); 
END { undef $seqinx; }   # stop bizarre seg faults

my $glob;

if( $division ) {
    $glob = "$division*.fa";
}
else {
    $glob = "*.fa";
}

foreach my $db ( glob( "$rfamseq_current_dir/$glob" ) ) {
    print STDERR "searching $db\n";
    if( $minidb ) {
	system "blastall -p blastn -i $fafile -e $evalue -F F -W 7 -d $db > $$.blast" and die;
	my %seqlist = %{ &parse_blast( "$$.blast" ) };
	foreach my $seqid ( keys %seqlist ) {
	    foreach my $reg ( @{ $seqlist{ $seqid } } ) {
		my $seq = &get_seq( $seqid, $reg->{'start'}, $reg->{'end'} );
		next if not $seq;
		my $seqstr = $seq->seq();
		$seqstr =~ s/(.{1,60})/$1\n/g;
		print ">", $seq->id(), "\n$seqstr";
	    }
	}
    }
    else {
	system "blastall -p blastn -i $fafile -e $evalue -F F -W 7 -d $db" and die;
    }
}

#########

sub parse_blast {
    my $blastfile = shift;
    open( BL, $blastfile ) or die;
    my $report = new Bio::Tools::BPlite( -fh => \*BL );
    my %list;
    {
        while( my $sbjct = $report -> nextSbjct ) {
            my $name = $sbjct -> name();
            $name =~ /^(\S+)\s+/;
            $name = $1;
            while( my $hsp = $sbjct->nextHSP ) {
                my( $start, $end ) = ( $hsp->subject->start, $hsp->subject->end );
                # add window length onto each end
                $start = $start - $length;
                $end   = $end   + $length;
                $start = 1 if( $start < 1 );

                # avoid having multiple copies of one region in minidb
                my $already;
                if( exists $list{ $name } ) {
                    foreach my $se ( sort @{ $list{ $name } } ) {
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
                    push( @{ $list{ $name } }, { 'start' => $start,
                                                 'end'   => $end } );
                }
            }
        }
        last if ( $report -> _parseHeader == -1 );
        redo;
    }
    return \%list;
}

sub get_seq {
    # fixes start < 1 and end > length
    my $id    = shift;
    my $start = shift;
    my $end   = shift;

    my $seq = new Bio::Seq;
    eval {
        $seq = $seqinx -> fetch( $id );
    };
    if( not $seq or $@ ) {
        warn "$id not found in your seq db\n";
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
    $truncseq -> desc( "" );
    $truncseq -> id( "$id/$start-$end" );
    return $truncseq;
}
