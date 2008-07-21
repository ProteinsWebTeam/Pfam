#!/software/bin/perl

use strict;
use Getopt::Long;
use Bio::SeqFetcher::xdget;
use Rfam;
my ($help, $lh, $rh, $keepalign, $trim);
&GetOptions (
        'h' => \$help,
	'help' => \$help,
        'lh=i' => \$lh,
	'rh=i' => \$rh,
        'trim' => \$trim
        );
if ($help)
{
print STDERR <<EOF;

Program to extend RNA stockholm format multiple alignments
Options:
	-h or --help  	This help screen
	-lh [number] 	Number of bases to extend in left hand (5') direction
	-rh [number]	Number of bases to extend in right hand (3') direction
	-trim           Trim rather than extend the alignment, note still requires an -lh/-rh option
extend_rna.pl -lh [num] -rh [num] [multiple alignment]

WARNINGS: -max NSE length is 60 - we need a better solution for this!
          -trim will toast your structure if you trim half of a basepair off
EOF
exit;
}
my $inxfile = $Rfam::rfamseq;
my $input = shift;
my $seqinx = Bio::SeqFetcher::xdget->new( '-db' => [$inxfile] );
chomp $input;
my $ss_cons;
open (INPUTFILE, "$input") or die "Can't find your input file!";
open (OUT, ">SEED.$$");
while (<INPUTFILE>){
    if ($_=~m/^#=GC SS_cons\s+(.+)/){
	$ss_cons = $1;
	chomp $ss_cons;
    }
}
my $newstart;
my $newend;
my $str_ali;
my ($alistart, $aliend);

my $aln = new Rfam::RfamAlign;
open (ALIGNFILE, "$input");
$aln->read_stockholm( \*ALIGNFILE );
close ALIGNFILE;
print "# STOCKHOLM 1.0\n\n";
foreach my $seq ($aln -> each_seq()){
    $str_ali = $seq -> seq();
    my $acc = $seq->id;
    my $start = $seq->start;
    my $end = $seq->end;
    if (defined($trim)){
	&trim_seqs($acc, $start, $end, $lh, $rh, $str_ali);
    }
    else {
	$newstart = $start-$lh;
	$newend = $end+$rh;
	my $extra = &get_aligned_seqs($acc, $start, $end, $lh, $rh, $str_ali); 
    }
}

#Now print secondary structure
chomp $ss_cons;
my $sshead = "#=GC SS_cons";
my $ssspace = 60 - length($sshead);
print "#=GC SS_cons" . " " x  $ssspace;

if (defined($trim)){
    
    if (defined($lh) ){
	$ss_cons = substr($ss_cons, $lh);
    }
    
    if (defined($rh) ){
	$ss_cons = substr($ss_cons,  0, length($ss_cons)-$lh);
    }
    
    print "$ss_cons";
    
}
else {
    my $i =0;
    while ($i < $lh){
	print ".";
	$i++;
    }
    print "$ss_cons";
    my $j =0;
    while ($j < $rh){
	print ".";
	$j++;
    }
}
print "\n\/\/\n";

unlink "tmp.$$";

exit(0);

sub trim_seqs {
    my $id    = shift;
    my $start = shift;
    my $end   = shift;
    my $lh = shift;
    my $rh =shift;
    my $str_ali = shift;

    if (defined($lh) ){
	if ($start < $end){
	    $start = $start + $lh;
	}
	else {
	    $start = $start - $lh;
	}
	$str_ali = substr($str_ali, $lh);
    }
    
    if (defined($rh) ){
	if ($start < $end){
	    $end = $end - $rh;
	}
	else {
	    $start = $start + $rh;
	}
	$str_ali = substr($str_ali,  0, length($str_ali)-$lh);
    }
    
    my $nse = "$id/$start-$end";
    my $idlength =(length($nse));
    my $space = 60 - $idlength;
    print $nse . " " x $space . $str_ali . "\n";
    
    
}

sub get_aligned_seqs {
    my $id    = shift;
    my $start = shift;
    my $end   = shift;
    my $lh = shift;
    my $rh =shift;
    my $reverse;
    my $starttrunc;
    my $startseq;
    my $endtrunc;
    my $endseq;
    my $oldend = $end;
    my $oldstart = $start;
	
    $reverse = 1 if( $end < $start );
    my $seq = new Bio::Seq;    
    eval {
        $seq = $seqinx -> get_Seq_by_acc( $id );
    };
    if( $@ or not $seq ) {
        warn "$id not found in your seq db\n";
        return 0;       # failure
    }
    if ($reverse){
	$oldstart = $start+$lh;
	$oldend = $end-$rh;
        if ($oldend < 1){
	    $oldend =1;
	}
    } 
    else{
	$end = $end+$rh;
	$start = $start-$lh;
    }
    if ($end > $seq->length){
	$end = $seq->length;
    }
    if ($start < 1){
	$start =1;
    }
    ( $alistart, $aliend ) = ( $start, $end );
    
    $starttrunc = $seq -> trunc( $alistart, $oldstart );
    $startseq = $starttrunc->seq();
    $endtrunc = $seq -> trunc( $oldend, $aliend);
    $endseq = $endtrunc->seq();
    if( $reverse ) {
    my $revseq = $starttrunc -> revcom();
    $startseq = $revseq->seq();
    my $revend = $endtrunc -> revcom();
    $endseq = $revend->seq();
    ( $alistart, $aliend ) = ( $oldstart, $oldend );
    }
    my @stsq = split (//, $startseq);
    pop @stsq;
    $startseq = undef;
    foreach my $el (@stsq){
    	if ($el =~m/T/){
	$el = "U";
	} 
    $startseq = $startseq.$el;
    }
    my @ensq = split (//, $endseq);
    $endseq = undef;
    shift @ensq;
   
    foreach my $el (@ensq){
    	if ($el =~m/T/){
	$el = "U";
	}
    $endseq = $endseq.$el;
    }
    my $gapleft = $lh - (length($startseq));
    my $gapright = $rh - (length($endseq));
    my $nse = "$id/$alistart-$aliend";
    my $idlength =(length($nse));
    my $all="$startseq$str_ali$endseq";
    my $space = 60 - $idlength;
    print $nse;
    my $k =0;
	while ($k < $space){
	print " ";
	$k++;
	}
	my $i =0;
	while ($i < $gapleft){
	print ".";
	$i++;
	}
	print "$all";
	my $j =0;
	while ($j < $gapright){
	print ".";
	$j++;
	}
	print "\n";
}
