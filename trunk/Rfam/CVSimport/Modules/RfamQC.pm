
#
# Quality control routines for Rfam
#

package RfamQC;

use strict;
use Rfam;
use Rfam::RfamAlign;
use Bio::SeqFetcher::xdget;
use Rfam::DB::DB_RCS;

use vars qw( %tag_mandatory
	     %tag_mandatory_stk
	     %tag_optional
	     %tag_mandatory_aln
	     %tag_optional_aln );

%tag_mandatory = (
		  'AC' => 1,
		  'ID' => 1,
		  'DE' => 1,
		  'AU' => 1,
		  'TC' => 1,
		  'NC' => 1,
		  'GA' => 1,
		  'SE' => 1,
		  'SS' => 1,
		  'TP' => 1,
		  'BM' => 2,
		  );

%tag_mandatory_stk = (
		      'SQ' => 1,
		      );

%tag_optional = ( # numbers don't mean anything here
		  'PI' => 1,
		  'CC' => 1,
		  'RN' => 1,
		  'RT' => 1,
		  'RA' => 1,
		  'RL' => 1,
		  'RM' => 1,
		  'DR' => 1,
		 );

%tag_mandatory_aln = (
		      'SS_cons' => 1,
		      );

%tag_optional_aln = (
		     'RF' => 1,
		     );


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


sub correct_tags {
    my $fh = shift;
    my $stk;
    my %fields;
    my $error = 0;
    my $seenend;
    while(<$fh>) {
	if( /^\# STOCKHOLM/ ) {
	    $stk = 1;
	    next;
	}
	if( $stk and /^\#=GF ([A-Z]{2})   \S+/ ) {
	    $fields{$1}++;
	    next;
	}
	if( !$stk and /^([A-Z]{2})   \S+/ ) {
	    $fields{$1}++;
	    next;
	}
	if( $stk and /^$/ ) {
	    next;
	}
	if( $stk and /^\S+\/\d+-\d+\d+\S+$/ ) {
	    next;
	}
	if( $stk and /^\#=G. (\S+) / ) {
	    $fields{$1}++;
	}
	if( $stk and /^\/\// ) {
	    $seenend = 1;
	    next;
	}

    }
    
    foreach my $f ( keys %fields ) {
	if( exists $tag_mandatory{$f} ) {
	    if( $fields{$f} != $tag_mandatory{$f} ) {
		warn "[$f]: should have $tag_mandatory{$f} lines, we have $fields{$f}\n";
		$error ++;
	    }
	    next;
	}
	elsif( $stk and exists $tag_mandatory_stk{$f} ) {
	    if( $fields{$f} != $tag_mandatory_stk{$f} ) {
		warn "[$f]: should have $tag_mandatory_stk{$f} lines, we have $fields{$f}\n";
		$error ++;
	    }
	    next;
	}
	elsif( exists $tag_optional{$f} ) {
	    next;
	}
	elsif( exists $tag_mandatory_aln{$f} or exists $tag_optional_aln{$f} ) {
	    next;
	}

	warn "[$f]: unknown field\n";
	$error ++;
    }

    foreach my $f ( keys %tag_mandatory ) {
	if( $fields{$f} != $tag_mandatory{$f} ) {
	    warn "[$f]: should have $tag_mandatory{$f} lines, we have $fields{$f}\n";
	    $error ++;
	}
    }

    if( $stk ) {
	foreach my $f ( keys %tag_mandatory_stk ) {
	    if( $fields{$f} != $tag_mandatory_stk{$f} ) {
		warn "[$f]: should have $tag_mandatory_stk{$f} lines, we have $fields{$f}\n";
		$error ++;
	    }
	}
	foreach my $f ( keys %tag_mandatory_aln ) {
	    if( ! exists $fields{$f} ) {
		warn "[$f]: missing line\n";
		$error ++;
	    }
	}
	
	if( not $seenend ) {
	    warn "No \/\/ delimiter\n";
	    $error ++;
	}
    }

    return $error;
}


sub valid_sequences {
    my $family = shift;
    my $db = Rfam::default_db;
    my $error;

    my @aligns = qw( SEED ALIGN );
    foreach my $alnfile ( @aligns ) {
	my $aln = new Rfam::RfamAlign;
	open( ALN, "$family/$alnfile" ) or do {
	    print "$family/$alnfile: can't open -- very bad!\n";
	    $error = 1;
	    next;
	};
	$aln -> read_stockholm( \*ALN );

	if( $alnfile =~ /SEED/ and $aln->no_sequences < 2 ) {
	    print "$family/$alnfile: SEED has < 2 members -- very bad!\n";
	    $error = 1;
	}

	foreach my $seq ( $aln -> each_seq ) {
	    if( $seq->id !~ /\S+\.\d+$/ ) {
		print "$family/$alnfile: ".$seq->id." doesn't have a version\n";
		$error = 1;
	    }

	    my $rfamseq = $db -> get_rfamseq( $seq->id, $seq->start, $seq->end );
	    if( not $rfamseq ) {
		print "$family/$alnfile: cannot find ".$seq->id." in rfamseq database\n";
		$error = 1;
		next;
	    }           
	    my $str_ali = $seq -> seq();
	    $str_ali =~ s/[.-]//g;
	    $str_ali = uc( $str_ali );
	    my( $start, $end ) = ( $seq->start, $seq->end );
#	    if( $end > $rfamseq->length or $start > $rfamseq->length ) {
#		print "$family/$alnfile: The sequence of ".$seq->id." does not match the rfamseq database\n";
#		$error = 1;
#		next;
#	    }
#	    if( $seq->end < $seq->start ) {
#		( $end, $start ) = ( $start, $end );
#	    }

	    my $str_rfamseq = $rfamseq->seq;
	    $str_ali     =~ tr/T/U/;
	    $str_rfamseq =~ tr/T/U/;
	 
#	    print "$str_ali\n$str_rfamseq\n\n";
   
	    if( $str_ali ne $str_rfamseq ) {
		print "$family/$alnfile: The sequence of ".$seq->id."/" . $seq->start . "-" . $seq->end . " does not match the rfamseq database\n";
		print "Sequence is:\n$str_ali\nBut should be:\n$str_rfamseq\n";
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
