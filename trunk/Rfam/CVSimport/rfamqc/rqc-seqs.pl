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
use Rfam;

use Rfam::RfamAlign;
use Bio::Index::Fasta;
use Bio::Index::Swissprot;

my $family = shift;
my $inx = Bio::Index::Fasta -> new( '-filename' => $Rfam::rfamseq_new_inx );

my $error;
my @aligns = qw( SEED ALIGN );
foreach my $alnfile ( @aligns ) {
    my $aln = new Rfam::RfamAlign;
    open( ALN, "$family/$alnfile" ) or die "can't open $family/$alnfile";
    $aln -> read_stockholm( \*ALN );
    foreach my $seq ( $aln -> eachSeq ) {
	my $rfamseq = $inx -> fetch( $seq->id );
	if( not $rfamseq ) {
	    print "$family/$alnfile: cannot find ".$seq->id." in rfamseq database\n";
	    $error = 1;
	    next;
	}	    
	my $str_ali = $seq -> seq();
	$str_ali =~ s/[.-]//g;
	$str_ali = uc( $str_ali );
	my( $start, $end ) = ( $seq->start, $seq->end );
	if( $end > $rfamseq->length or $start > $rfamseq->length ) {
	    print "$family/$alnfile: The sequence of ".$seq->id." does not match the rfamseq database\n";
	    $error = 1;
	    next;
	}
	if( $seq->end < $seq->start ) {
	    ( $end, $start ) = ( $start, $end );
	}
	my $rfamseqtrunc = $rfamseq -> trunc( $start, $end );
	my $str_rfamseq;
	if( $seq->end < $seq->start ) {
	    $str_rfamseq = $rfamseqtrunc -> revcom() -> seq();
	}
	else {
	    $str_rfamseq = $rfamseqtrunc -> seq();
	}
	
	$str_ali     =~ tr/T/U/;
	$str_rfamseq =~ tr/T/U/;

	if( $str_ali ne $str_rfamseq ) {
	    print "$family/$alnfile: The sequence of ".$seq->id." does not match the rfamseq database\n";
	    print "$str_ali\n$str_rfamseq\n";
	    $error = 1;
	    next;
	}
    }
}
if( $error ) {
    print "$family contains errors.  You should rebuild this family.\n";
    exit(1);
}
else {
    print "$family passes sequence checks\n";
    exit(0);
}    
