#
# RfamAlign.pm
#
# Inherits from Bioperl's SimpleAlign.pm and adds a few things.
# Should live with Rfam code outside of bioperl tree in an attempt
# to leave the two separately upgradable.
#

=head1 NAME

Bio::Rfam::RfamAlign

=head1 SYNOPSIS

    use Bio::Rfam::RfamAlign;

    $aln = new Bio::Rfam::RfamAlign->new;
    eval {
        $aln->read_stockholm( $fh );
    };
    $@ and do { $i->dont_know( $what ) };;

=head1 DESCRIPTION

Inherits from Bioperl's Bio::SimpleAlign and adds things to deal
correctly with secondary structure markup etc from stockholm format.

=cut

package Bio::Rfam::RfamAlign;
use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;

use Bio::SimpleAlign;
use Bio::Rfam::SS;

@ISA = qw( Bio::SimpleAlign );


# new is probably not needed at the moment, but we have it
# here to make the SS_CONS and MATCH_STATES explicit and
# documented

sub new {
    my $caller = shift;
    my $class  = ref( $caller ) || $caller;
    my $self   = $class -> SUPER::new();
    $self -> { 'SS_CONS' }      = undef;
    $self -> { 'MATCH_STATES' } = undef;
    $self -> { 'CONSENSUS' }    = undef;
    return $self;
}


sub consensus {
    my $self = shift;
    if( not $self -> { 'CONSENSUS' } ) {
	$self -> { 'CONSENSUS' } = $self -> _compute_consensus();
    }
    return $self -> { 'CONSENSUS' };
}

sub ss_cons {
    my $self = shift;
    $self -> { 'SS_CONS' } = shift if @_;
    return $self -> { 'SS_CONS' };
}

sub match_states {
    my $self    = shift;
    $self -> { 'MATCH_STATES' } = shift if @_;
    return $self -> { 'MATCH_STATES' };
}


# SimpleAlign docs suggest this is implemented therein but it aint!
# Nicked from Pfam::AlignPfam

# altered to act on $self and return number of columns removed

sub allgaps_columns_removed {
    my ($self) = @_;
    my (@columnlist, %mymap);
    my @index_list = (0..$self->length_aln-1);

    foreach my $seq ($self->eachSeq) {
        my @ary = split( //, $seq->seq() );
        foreach my $el (grep { $ary[$_] ne '.' and $ary[$_] ne '-' }  (@index_list)) {
            $mymap{ $el } = 1;
        }
    }

    my @sortedgappositions = sort { $b <=> $a } grep { not defined( $mymap{$_}) }  (@index_list);
    my @sort2 = map { $_ + 1 } @sortedgappositions;    # removeColumn index starts at 1 not 0
    $self -> ss_cons -> removeColumn( @sort2 );    

    if( $self -> match_states ) {
	my @newrf = split( //, $self -> match_states );
	foreach my $gappos (@sortedgappositions) {
	    splice @newrf, $gappos, 1;
	}
	$self -> match_states( join( "", @newrf ) );
    }

    foreach my $seq ($self->eachSeq) {
	my @newseq = split( //, $seq->seq() );

        foreach my $gappos (@sortedgappositions) {
            splice @newseq, $gappos, 1;
        }

	$seq -> seq( join ( "", @newseq ) );
    }

    return scalar( @sortedgappositions );
}


sub trimmed_alignment {
    my ($self, $start, $end) = @_;

    if (not defined($start) and not defined($end)) {
        $self->throw("The desired extent of the alignment has not been given in any way");
    }
    elsif (not defined($start)) {
        $start = 1;
    }
    elsif (not defined($end)) {
        $end = $self->length_aln();
    }
    
    my @rf = split( //, $self -> match_states );

    my @junkrf = splice( @rf, 0, $start-1 );
    my $newrf  = join( '', splice( @rf, 0, $end-$start+1 ) );
    $self -> match_states( $newrf );

    if( $start > 1 ) {
	$self->ss_cons->removeColumn( 1..$start-1 );
    }
    if( $end < $self->length_aln() ) {
	$self->ss_cons->removeColumn( $end+1..$self->length_aln() );
    }

    foreach my $seq ($self->eachSeq()) {
        my @residues = split( //, $seq->seq() );
        my @discardedleft = splice( @residues, 0, $start-1 );

        # now, if we splice the first ($end - $start + 1) residues, then that is 
        # what we are interested in. @residues will be left with what is discarded right.
 
        $seq -> seq( join( '', splice( @residues, 0, $end-$start+1 ) ) );

	my $newstart = $seq->start();
	my $newend   = $seq->end();

        foreach my $char (@discardedleft) {
            if ($char ne '-' and $char ne '.') {
		if( $seq->start() > $seq->end() ) { 
		    $newstart--;
		}
		else {
		    $newstart++;
		}
            }
        }
        foreach my $char (@residues) {
            if ($char ne '-' and $char ne '.') {
		if( $seq->end() < $seq->start() ) { 
		    $newend++;
		}
		else {
		    $newend--;
		}
            }
        }

	$seq -> start( $newstart );
	$seq -> end( $newend );

        # we may be left with just gaps in the sequence; if this is the case
        # then remove it

        if ($newend == $newstart) {
	    $seq -> removeSeq( $seq );
        }
    }

    return 1;
}


sub read_stockholm {
    my $self = shift;
    my $in = shift;

    my( $ss_cons, 
	$match_states,
	%align,
	%c2name,
	$count );

    while( <$in> ) {
        /^\# STOCKHOLM/ && next;

        /^\/\// && do {
            # we have reached the end of the entry
            last;
        };
        /^\#=GC\s+SS_cons\s+(.+)/ && do {
            $ss_cons .= $1; 
            next;
        };
        /^\#=GC\s+RF\s+(.+)/ && do {
            $match_states .= $1;
            next;
        };
        
        /^([^\#]\S+)\s+([A-Za-z\.\-]+)\s*/ && do {      
            my $name = $1;
            my $seq = $2;
            
            if( ! defined $align{$name}  ) {
                $count++;
                $c2name{$count} = $name;
            }
            
            $align{$name} .= $seq;
            next;
        };

        # Blank line? fine. Comment? fine. Anything else? Forget it

        /\w/ && !/^\#/ && do {
            $self->throw("Line [$_] is not valid stockholm format");
        };
    }

    # ok... now we can make the sequences

    $count = 0;
    foreach my $no ( sort { $a <=> $b } keys %c2name ) {
        my $name = $c2name{$no};
	my( $seqname,
	    $start,
	    $end );
	
        if( $name =~ /(\S+)\/(\d+)-(\d+)/ ) {
            $seqname = $1;
            $start = $2;
            $end = $3;
        } else {
            $seqname=$name;
            $start = 1;
            $end = length($align{$name});
        }

        my $seq = new Bio::LocatableSeq( '-seq'   => $align{$name},
					 '-id'    => $seqname,
					 '-start' => $start,
					 '-end'   => $end, 
					 '-type'  => 'aligned'
					 );

        $self -> addSeq($seq);
        $count++;
    }

    if( $ss_cons ) {
	my $ss = new Bio::Rfam::SS;
	$ss -> parseInfernalString( $ss_cons );
	$self -> ss_cons( $ss );
    }
    if( $match_states ) {
	$self -> match_states( $match_states );
    } 

    return $count; 
}


sub write_structure_ps {
    my $self = shift;
    my $out  = shift;

    my $newaln = new Bio::Rfam::RfamAlign;
    my $conseq = new Bio::LocatableSeq( '-id'  => "Seq_cons",
					'-start' => 1,
					'-end' => $self->length_aln(),
					'-seq' => $self->consensus() );

    $newaln -> addSeq( $conseq );
    $newaln -> ss_cons( $self->ss_cons );
    $newaln -> allgaps_columns_removed();

    my( $seq ) = $newaln -> eachSeq();

    open( T, ">$$.rna" ) or die;
    print T ">$$.rna\n", $seq->seq(), "\n", $newaln->ss_cons->getViennaString(), "\n";
    close T;

    system "RNAplot < $$.rna > /dev/null" and die "can't run RNAplot";
    open( PS, "$$.rna_ss.ps" ) or die;
    while(<PS>) {
	if( /^drawbases/ ) {
	    print $out $_;
	    print $out "  0 setgray\n";
	    next;
	}
	if( /^drawoutline/ ) {
	    print $out $_;
	    print $out "  0.5 setgray\n";
	    next;
	}
	if( /^drawpairs/ ) {
	    print $out $_;
	    print $out "  0.5 setgray\n";
	    next;
	}
	if( /\[9 3.01\] 9 setdash/ ) {
	    print $out "  [5 12] 12 setdash\n";
	    next;
	}
	print $out $_;
    }
    close PS;
    unlink( "$$.rna", "$$.rna_ss.ps" ) or die;
}

sub write_stockholm {
    my $self  = shift;
    my $out   = shift;
    my $block = shift;
    $block = 50 if( not defined $block );
    $block = $self -> length_aln if( not $block );

    my $maxn = $self->maxdisplayname_length() + 2;
    my $iter = $self->length_aln/$block;
    print $out "\# STOCKHOLM 1.0\n\n";

    my $ss_str = $self->ss_cons->getInfernalString();
	
    for( my $i=0; $i < $iter; $i++ ) {
	foreach my $seq ( $self->eachSeq() ) {
	    my $namestr = $self->get_displayname($seq->get_nse());
	    my $subseq = substr( $seq->seq, $i*$block, $block );
	    print $out sprintf( "%-".$maxn."s  %s\n", $namestr, $subseq );
	}
	if( $self->match_states() ) {
	    my $submatch = substr( $self->match_states(), $i*$block, $block );
	    print $out sprintf( "%-".$maxn."s  %s\n", "\#=GC RF", $submatch );
	}
	if( $ss_str ) {
	    my $subcons  = substr( $ss_str, $i*$block, $block );
	    print $out sprintf( "%-".$maxn."s  %s\n", "\#=GC SS_cons", $subcons );
	}
	print $out "\n" unless( ($i+1) >= $iter );
    }
    print $out "\/\/\n";
}


sub write_connect {
    my $self = shift;
    my $out  = shift;

    die "can't read ss_cons line" if( not $self -> ss_cons() );

    my @ss   = split( //, $self->ss_cons->getInfernalString );
    my @cons = split( //, $self->consensus() );

    my( @open, %bp );
    for( my $i=1; $i<=@ss; $i++ ) {
	if( $ss[$i-1] =~ /[\[\(\{\<]/ ) {
	    push( @open, $i );
	}
	elsif( $ss[$i-1] =~ /[\]\)\}\>]/ ) {
	    my $j = pop @open || -1;
	    $bp{$i} = $j;
	    $bp{$j} = $i;
	}
    }
    
    for( my $i=1; $i<=@ss; $i++ ) {
	my $j;
	if( exists $bp{$i} ) {
	    $j = $bp{$i};
	}
	else {
	    $j = 0;
	}
	my $next;
	if( $i+1 > @ss ) {
	    $next = 0;
	}
	else {
	    $next = $i+1;
	} 
	print $out sprintf( "%5d %-1s %5d %5d %5d %5d\n", $i, $cons[$i-1], $i-1, $next, $j, $i );
    }
}


# this returns simply the most common base, no fancy perc. cutoffs or anything
sub _compute_consensus {
    my $self = shift;
    my @columns;

    foreach my $seq ( $self -> eachSeq() ) {
	my @ary = split( //, $seq->seq );
	for( my $i=0; $i<@ary; $i++ ) {
	    $columns[$i]->{$ary[$i]} ++;
	}
    }

    my $str;
    foreach my $col ( @columns ) {
	my $best;
	my $high = 0;
	foreach my $sym ( keys %{$col} ) {
	    if( $col->{$sym} > $high ) {
		$best = $sym;
		$high = $col->{$sym};
	    }
	}
	$str .= $best;
    }
    return $str;
}


1;
