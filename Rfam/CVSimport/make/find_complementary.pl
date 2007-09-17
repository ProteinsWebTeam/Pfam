#!/software/bin/perl -w

use strict;
use Getopt::Long;
use Bio::SeqIO;
use Bio::SearchIO;

sub usage {
    print STDERR <<EOF;
$0

Find regions of complementarity to the query using WU-BLAST and a custom scoring matrix.

Usage: $0 <target> <query>
Options:   -h                   show this help
           --strand [b|f|r]     search [b]oth, [f]orward or [r]everse strand (default f)

EOF
}

my( $help, $bpscore, $guscore, $mmscore );
my $strand = "f";   # default

GetOptions( "h"        => \$help,
	    "strand=s" => \$strand,
	    "M=s"      => \$bpscore,
	    "M2=s"     => \$guscore,
	    "N=s"      => \$mmscore );

# args in wublast order
my $targetfile = shift;
my $queryfile = shift;

if( $help or !$queryfile ) {
    &usage();
    exit(0);
}

if( $strand !~ /^[b|f|r]$/ ) {
    &usage();
    warn "FATAL: unknown --strand argument [$strand]\n";
    exit(1);
}

if( $bpscore and !$guscore ) {
    $guscore = $bpscore;
}
$bpscore = +5 unless $bpscore;
$guscore = +3 unless $guscore;
$mmscore = -4 unless $mmscore;


# we need to reverse the query sequence (but not complement)
my $qin = Bio::SeqIO -> new( -file => $queryfile, -format => 'Fasta' );
my $qout = Bio::SeqIO -> new( -file => ">$$.query.rev", -format => 'Fasta' );
while( my $seq = $qin->next_seq() ) {
    my @rev = reverse( split( //, $seq->seq ) );
    $seq->seq( join( "", @rev ) );
    $qout->write_seq( $seq );
}
undef $qout;
undef $qin;

my @targetfiles;

if( $strand =~ /[b|f]/ ) {
    # search the top strand
     push( @targetfiles, { 'file' => "$targetfile",
			   'strand' => 1 } );
}

if( $strand =~ /[b|r]/ ) {
    # we need the reverse strand
    my $tin  = Bio::SeqIO -> new( '-file' => $targetfile,
				  '-format' => 'Fasta' );
    my $tout = Bio::SeqIO -> new( '-file' => ">$$.target.rc",
				  '-format' => 'Fasta' );
    while( my $seq = $tin->next_seq() ) {
	my $rc = $seq->revcom();
	$tout->write_seq( $rc );
    }
    undef $tout;
    push( @targetfiles, { 'file' => "$$.target.rc",
			  'strand' => -1 } );
}

# write out the matrix - otherwise I can't make wublast find it!
&dump_matrix( $bpscore, $guscore, $mmscore );

foreach my $target ( @targetfiles ) {
    my $targetfile = $target->{'file'};
    system "xdformat -p $targetfile > /dev/null 2>&1" and die "failed to run xdformat";
    system "wublastp $targetfile $$.query.rev -matrix matrix -span1 -warnings Q=10 R=10 gapK=0.0151 gapH=0.0600 gapL=0.104 > $$.blast" and die "failed to run wublastp";

    my $in = Bio::SearchIO -> new( '-file' => "$$.blast",
				   '-format' => 'Blast' );

    while( my $result = $in->next_result() ) {
	while( my $hit = $result->next_hit() ) {
	    while( my $hsp = $hit->next_hsp() ) {
		my( $hitst, $hiten ) = ( $hsp->start('hit'), $hsp->end('hit') );
		my $direction = "Forward";
		if( $target->{'strand'} < 0 ) {
		    ( $hitst, $hiten ) = ( $hiten, $hitst );
		    $direction = "Reverse";
		}
		
		# print a summary line
		printf( ">%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n\n",
			$result->query_name(),
			$hsp->start('query'),
			$hsp->end('query'),
			$direction,
			$hit->name, 
			$hsp->start('hit'),
			$hsp->end('hit'),
			$hsp->score(),
			$hsp->evalue(),
			);
		
		# fudge the homology line to use | for A=T, G=C and : for G=U
		my @homol = split( //, $hsp->homology_string() );
		for( my $i=0; $i<@homol; $i++) {
		    my $hitbase = substr( $hsp->hit_string(), $i, 1 );
		    my $querybase = substr( $hsp->query_string(), $i, 1 );
		    if( ($querybase eq "G" and $hitbase eq "T") or ($querybase eq "T" and $hitbase eq "G") ) {
			$homol[$i] = ":";
		    }
		}
		my $homol = join( "", @homol );
		$homol =~ s/\+/\|/g;

		# remember these as a running count
		my $hen = $hsp->start('hit') - 1;
		my $qen = $hsp->end('query') + 1;
		if( $target->{'strand'} < 0 ) {
		    $hen = $hsp->end('hit') + 1;
		}

		# find the longest start/end number
		my $numl = length( $hsp->start('query') );
		if( length( $hsp->end('query') ) > $numl ) {
		    $numl = length( $hsp->end('query') );
		}
		if( length( $hsp->start('hit') ) > $numl ) {
		    $numl = length( $hsp->start('hit') );
		}
		if( length( $hsp->end('hit') ) > $numl ) {
		    $numl = length( $hsp->end('hit') );
		}

		# print the alignments out nicely
		for( my $i=0; $i<$hsp->hsp_length(); $i=$i+60 ) {
		    my $hsub = substr( $hsp->hit_string(), $i, 60 );
		    $hsub =~ s/-//g;
		    my $hst;

		    if( $target->{'strand'} < 0 ) {
			$hst = $hen - 1;
			$hen = $hst - length($hsub) + 1;
			$hen = $hsp->start('hit') if( $hen < $hsp->start('hit') );
		    }
		    else {
			$hst = $hen + 1;
			$hen = $hst + length($hsub) - 1;
			$hen = $hsp->end('hit') if( $hen > $hsp->end('hit') );
		    }
		    
		    my $qsub = substr( $hsp->query_string(), $i, 60 );
		    my $strl = length( $qsub );
		    $qsub =~ s/-//g;
		    my $qst = $qen - 1;
		    $qen = $qst - length($qsub) + 1;
		    $qen = $hsp->start('query') if( $qen < $hsp->start('query') );
		    
		    printf( "%5s %".$numl."d %-".$strl."s %-".$numl."d %s\n", 
			    "3'",
			    $qst, 
			    substr( $hsp->query_string(), $i, 60 ),
			    $qen,
			    "5'"
			    );
		    printf( "%".($numl+6)."s %-".$strl."s\n", "",
			    substr( $homol, $i, 60 ) 
			    );
		    printf( "%5s %".$numl."d %-".$strl."s %-".$numl."d %s\n",
			    "5'",
			    $hst, 
			    substr( $hsp->hit_string(), $i, 60 ),
			    $hen,
			    "3'"
			    );
		    print "\n";
		}
	    }
	}
    }
}



sub dump_matrix {
    my $bp = shift || +5;
    my $gu = shift || +3;
    my $mm = shift || -4;
    open( MAT, ">matrix" ) or die "can't write to matrix";
    print MAT <<EOF;
    A   T   U   G   C   S   W   R   Y   K   M   B   V   H   D   N
A  $mm  $bp  $bp  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
T  $bp  $mm  $mm  $gu  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
U  $bp  $mm  $mm  $gu  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
G  $mm  $gu  $gu  $mm  $bp  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
C  $mm  $mm  $mm  $bp  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
S  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
W  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
R  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
Y  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
K  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
M  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
B  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
V  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
H  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  
D  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
N  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm  $mm
EOF
}
