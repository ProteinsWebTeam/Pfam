
#
# Quality control routines for Rfam
#

package RfamQC;

use strict;
use Rfam;
use Rfam::RfamAlign;
use Bio::Index::Fasta;
use Bio::Index::Swissprot;


sub id_exists {
    my $id = shift;
    my $db = Rfam::default_db();
    if( $db -> is_id( $id ) ) {
	my $acc = $db->id2acc( $id );
	return $acc; # id exists
    }
    else {
	return 0;
    }
}


sub valid_sequences {
    my $family = shift;
    my $inx = Bio::Index::Fasta -> new( '-filename' => $Rfam::rfamseq_new_inx );
    my $error;

    my @aligns = qw( SEED ALIGN );
    foreach my $alnfile ( @aligns ) {
	my $aln = new Rfam::RfamAlign;
	open( ALN, "$family/$alnfile" ) or die "can't open $family/$alnfile";
	$aln -> read_stockholm( \*ALN );
	foreach my $seq ( $aln -> each_seq ) {
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
		$error = 1;
		next;
	    }
	}
    }
    if( $error ) {
	return 0;
    }
    else {
	return 1;
    }    
}



1;
