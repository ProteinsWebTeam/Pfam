#!/usr/local/bin/perl
use strict;
use Getopt::Long;
use Bio::SeqFetcher::xdget;
use Rfam;
my ($help, $lh, $rh, $realign, $keepalign);
&GetOptions (
        'h' => \$help,
	'help' => \$help,
        'lh=i' => \$lh,
	'rh=i' => \$rh,
	'realign' => \$realign,
	'keepalign' => \$keepalign
        );
if ($help)
{
print STDERR <<EOF;

Program to extend RNA stockholm format multiple alignments
Options:
	-h or --help  	This help screen
	-lh [number] 	Number of bases to extend in left hand (5') direction
	-rh [number]	Number of bases to extend in right hand (3') direction
	-realign	Realigns sequences after extend
	-keepalign	adds new sequence to existing alignment
	
extend_rna.pl -lh [num] -rh [num] [multiple alignment]

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
                unless ($_=~m/\/\//){
                print OUT $_;
                }
        }
system "reformat fasta SEED.$$ > fa.$$";
my $fasta = "fa.$$";
unlink "SEED.$$";
# Read fasta file and put ref into scalars
my @foo = &read_fasta($fasta);
my @sequence    = @{shift @foo};
my @description = @{shift @foo};
my $newstart;
my $newend;
my $str_ali;
open (TMP, ">tmp.$$"); 
my ($alistart, $aliend);

if ($realign){
	foreach my $element (@description){
		if ($element =~m/(\S+\.\d+)\/(\d+)-(\d+)/){
		my $acc = $1;
		my $start = $2;
		my $end = $3;
		$newstart = $start-$lh;
		$newend = $end+$rh;
		my $seq = &get_seq($acc, $newstart, $newend);
		my $seqstr = $seq->seq();
		$seqstr =~ tr/Tt/Uu/;
		$seqstr =~ s/(.{1,60})/$1\n/g;
		print TMP (">", $seq->id(), "\n$seqstr");
		}
	}
system ("create_alignment.pl -fasta tmp.$$ -cl");
close TMP;
unlink "tmp.$$";
}
elsif ($keepalign){
my $aln = new Rfam::RfamAlign;
open (ALIGNFILE, "$input");
$aln->read_stockholm( \*ALIGNFILE );
close ALIGNFILE;
    foreach my $seq ( sort{ $a->id cmp $b->id || $a->start <=> $b->start } $aln -> each_seq()){
	$str_ali = $seq -> seq();
	my $acc = $seq->id;
	my $start = $seq->start;
	my $end = $seq->end;
	$newstart = $start-$lh;
	$newend = $end+$rh;
	my $extra = &get_aligned_seqs($acc, $start, $end, $lh, $rh, $str_ali); 
	}

}

chomp $ss_cons;
print "#=GC SS_cons                            ";
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
unlink "tmp.$$";
unlink "fa.$$";
unlink "fa.$$.fa.$$";
sub read_fasta{

    my $fasta_file = shift;
    system ("reformat fasta $fasta_file > $fasta_file.fa.$$");
    $fasta_file = "$fasta_file.fa.$$";

    my (@sequence,@description,$i);

    open (FASTA, "$fasta_file")|| die "Can't open fasta file $fasta_file\n";

    $/=">";                   # Change record seperator
    $i=0;
    while (<FASTA>){
	if (/^\s?(\S+)\s?.*\n/){
	    $description[$i] = $1;
	    $sequence[$i] = $';
	    chop $sequence[$i];            # Remove > from end of sequence
	    $sequence[$i] =~ s/\n//g;      # Remove newlines from sequence
	    $sequence[$i] =~ s/.- _//g;    # Remove spaces from sequence
	    $sequence[$i] =~ tr/a-z/A-Z/; # Make sequence upper case
	    $i++;
	}
    }
    close (FASTA);
    $/="\n";                  # Change record seperator
    return (\@sequence,\@description);
}

sub get_seq {
    my $id    = shift;
    my $start = shift;
    my $end   = shift;
    my $reverse;
    
    if ($start < 1){
    $start =1;
    }
	
    $reverse = 1 if( $end < $start );    
    my $seq = new Bio::Seq;    
    eval {
        $seq = $seqinx -> get_Seq_by_acc( $id );
    };
    if( $@ or not $seq ) {
        warn "$id not found in your seq db\n";
        return 0;       # failure
    }
    	
    if ($end > $seq->length){
    $end = $seq->length;
    }
    
    my( $getstart, $getend );
    if( $reverse ) {
        ( $getstart, $getend ) = ( $end, $start );

    }
    else {
        ( $getstart, $getend ) = ( $start, $end );

    }
                                                                                                               
    my $truncseq = $seq -> trunc( $getstart, $getend );
    $truncseq -> desc( "" );
    $truncseq -> id( "$id/$start-$end" );
                                                                                                               
    if( $reverse ) {
        my $revseq = $truncseq -> revcom();
        $truncseq = $revseq;
    }
                                                                                                               
    return $truncseq;
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
    my $space = 40 - $idlength;
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
