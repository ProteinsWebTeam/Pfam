
# This is an horrible horrible hack at the moment.
# CMResults is just an HMMResults with parse methods over the top.
# So stuff like:
#       $res = new CMResults;
#       $res -> parse_hmmer2( \*FILE )
# WILL work when obviously it shouldn't.
#
# The excuse is that I need this in a hurry, and it will get done properly
# at some point blah blah blah.
#
# sgj

package CMResults;

#use lib '/pfam/db/Pfam/scripts/Modules'; # bad bad bad!

use strict;
use HMMResults;
use vars qw( @ISA );

@ISA = ( 'HMMResults' );


sub parse_cove {
    my $self = shift;
    my $file = shift;
    my( $id, $start, $end, $bits );
    while( <$file> ) {
	if( /^\s*(\S+)\s+(\d+)\s+(\d+)\s+\:\s+(\S+)\/(\d+)-(\d+)\s*$/ ) {
	    $id    = $4;
            $start = $2 + $5 - 1;
            $end   = $3 + $5 - 1;
	    $bits  = $1;
	}
        elsif( /^\s*(\S+)\s+(\d+)\s+(\d+)\s+\:\s+(\S+)\s*$/ ) {
	    $id    = $4;
            $start = $2;
            $end   = $3;
	    $bits  = $1;
	}
	unless( $self -> getHMMSequence( $id ) ) {
	    my $seq = new HMMSequence;
	    $seq    -> name( $id );
	    $self   -> addHMMSequence( $seq );
	}

	my $unit = new HMMUnit;
	$unit -> seqname( $id );
	$unit -> hmmname( " " );
	$unit -> hmmacc( " " );
	$unit -> start_seq( $start );
	$unit -> end_seq( $end );
	$unit -> start_hmm( " " );
	$unit -> end_hmm( " " );
	$unit -> bits( $bits );
	$unit -> evalue( " " );

	$self -> addHMMUnit( $unit );
    }
    return $self;
}


sub parse_infernal {
    my $self = shift;
    my $file = shift;

    my( $id, $start, $end, $alnline );
    my( $alnst, $alnen );
    my $unit;  # this should always be the last added HMMUnit

    while( <$file> ) {
	chomp;
	next if( /CPU time:\s+/ or /memory:\s+/ );
	if( /^sequence:\s+(\S+)\s*/ ) {
	    if( $1 =~ /^(\S+)\/(\d+)-(\d+)/ ) {
		( $id, $start, $end ) = ( $1, $2, $3 );
	    }
	    elsif( ($id) = $1 =~ /^(\S+)/ ) {
		$start = 1;
	    }
	    else { 
		die "Don't recognise cmsearch output line [$_]";
	    }
	}
	elsif( my( $st, $en, $bits ) = /^hit\s+\d+\s*:\s+(\d+)\s+(\d+)\s+(\S+)\s+bits/ ) {
	    # make sure we have a Sequence
	    unless( $self -> getHMMSequence( $id ) ) {
		my $seq = new HMMSequence;
		$seq    -> name( $id );
		$self   -> addHMMSequence( $seq );
	    }

	    undef $alnst; # reset ready for new alignment
	    undef $alnen;
	    $alnline = 0;
	
	    $st += $start - 1;
	    $en += $start - 1;

	    # sort out a Unit
	    $unit = new HMMUnit;
	    $unit -> seqname( $id );
	    $unit -> hmmname( " " );
	    $unit -> hmmacc( " " );
	    $unit -> start_seq( $st );
	    $unit -> end_seq( $en );
	    $unit -> bits( $bits );
	    $unit -> evalue( " " );

	    $self -> addHMMUnit( $unit );
	}
	elsif( /^\s*$/ ) {
	    $alnline = 0;
	}
	elsif( /^\s+(\d*)\s*(.+?)\s*(\d*)\s*$/ ) {
	    my $wholeline = $_;
	    $alnline ++;
	    if( $alnline == 2 ) {
		# unit is already in results object, but this should still
		# get to where it needs to be
		$unit -> start_hmm( $1 ) unless $unit -> start_hmm();
		$unit -> end_hmm( $3 );
	    }
	    if( $alnline == 4 ) {
		# cmsearch reports wierd start end numbers in the alignment
		# lines - fix them here
		if( my( $space, $ast, $stuff, $aen ) = /^(\s+)(\d+)\s+(.+)\s+(\d+)/ ) {
		    my $origaln = $stuff;
		    $stuff =~ s/[-\.]//g;

		    my $strand = 1;
		    if( $unit->start_seq > $unit->end_seq ) {
			$strand = -1;
		    }

		    if( $alnen ) {
			# add 1 for + strand stuff, take one for - strand
			$alnst = $alnen + $strand;
		    }
		    else {
			$alnst = $unit->start_seq;
		    }
		    $alnen = $alnst + $strand*( length($stuff)-1 );

		    my $spacing = length($space)+length($ast);
		    $wholeline = sprintf( "%".$spacing."s %s %s", $alnst, $origaln, $alnen );
		}
		else {
		    warn "can't read alignment line [$_]\n";
		}
	    }
	    $unit->add_alignment_line( $wholeline );
	}
	else {
	    warn "failed to parse line [$_]\n";
	}
    }
    return $self;
}    


sub parse_rsearch {
    my $self = shift;
    my $file = shift;

    my( $id, $start, $end, $subst, $suben );
    while( <$file> ) {
	if( /^\>(\S+)\/(\d+)-(\d+)/ ) {
	    ( $id, $start, $end ) = ( $1, $2, $3 );
	}
	elsif( /^\>(\S+)/ ) {
	    ( $id, $start ) =( $1, $2 );
	}
	if( /Query\s*=\s*\d+\s*-\s*\d+\,\s+Target\s*=\s*(\d+)\s*-\s*(\d+)\s*$/ ) {
	    $subst = $start + $1 - 1;
	    $suben = $start + $2 - 1;		
	}
	if( my( $bits ) = /Score\s*=\s*(\S+)\,?/ ) {
	    my $evalue = /E\s*=\s*(\S+)\,?/;
	    $evalue = "UNK" if( not $evalue );
		
	    unless( $self -> getHMMSequence( $id ) ) {
		my $seq = new HMMSequence;
		$seq    -> name( $id );
		$self   -> addHMMSequence( $seq );
	    }
	    
	    my $unit = new HMMUnit;
	    
	    $unit -> seqname( $id );
	    $unit -> hmmname( " " );
	    $unit -> hmmacc( " " );
	    $unit -> start_seq( $subst );
	    $unit -> end_seq( $suben );
	    $unit -> start_hmm( " " );       # could get these from alignment
	    $unit -> end_hmm( " " );         # section if we ever need them
	    $unit -> bits( $bits );
	    $unit -> evalue( $evalue );
	    
	    $self -> addHMMUnit( $unit );
	}
    }
}

	      
sub remove_overlaps {
    my $self = shift;
    my $new = new CMResults;
    foreach my $seq ( $self -> eachHMMSequence() ) {
	my $newseq = new HMMSequence;
	$newseq -> name( $seq -> name() );
	$new -> addHMMSequence( $newseq );

      UNIT:
	foreach my $unit1 ( sort { $b->bits <=> $a->bits } $seq -> eachHMMUnit() ) {
#	      print $unit1->seqname, " ", $unit1->start_seq, " ", $unit1->end_seq, " ", $unit1->bits, "\n";
	    foreach my $unit2 ( $newseq -> eachHMMUnit() ) {
		if( ( $unit1->start_seq >= $unit2->start_seq and $unit1->start_seq <= $unit2->end_seq ) or
		    ( $unit1->end_seq   >= $unit2->start_seq and $unit1->end_seq   <= $unit2->end_seq ) or
		    ( $unit1->start_seq <= $unit2->start_seq and $unit1->end_seq   >= $unit2->end_seq ) ) {
		    next UNIT;
		}
	    }
	    $new -> addHMMUnit( $unit1 );
	}
    }
    return $new;
}

sub filter_on_cutoff {
    my $self = shift;
    my $thr = shift;
    my ($new,$seq,$unit,@array,@narray);

    if( !defined $thr ) {
	carp("CMResults: filter on cutoff needs an argument");
    }

    $new = new CMResults;
    foreach $seq ( $self->eachHMMSequence()) {
	my $newseq = HMMSequence->new();
	$newseq->name($seq->name);
	$new->addHMMSequence($newseq);
	foreach $unit ( $seq->eachHMMUnit() ) {
	    if( $unit->bits() < $thr ) {
		next;
	    }
	    $new->addHMMUnit($unit);
	}
    }
    return $new;
}

sub highest_noise {
    my $self = shift;
    my $thr  = shift;
    my $score = -100000;
    
    foreach my $unit ( $self->eachHMMUnit() ) {
	if( $unit->bits() < $thr and $unit->bits() > $score ) {
	    $score = $unit->bits();
	}
    }

    return $score;
}

sub lowest_true {
    my $self = shift;
    my $thr  = shift;
    my $lowtrue;
    my $score = 100000;
    
    foreach my $unit ( $self->eachHMMUnit() ) {
	if( $unit->bits() >= $thr and $unit->bits() < $score ) {
	    $score = $unit->bits();
	}
    }

    return $score;
}


sub write_list {
    my $self = shift;
    my $fh   = shift;
    foreach my $seq ( $self->eachHMMSequence()) {
        foreach my $unit ( $seq->eachHMMUnit()) {
	    print $fh $unit->seqname, "/", $unit->start_seq, "-", $unit->end_seq, "\n";
	}
    }
}

1;
