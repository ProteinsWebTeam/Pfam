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
@ISA = qw( Bio::SimpleAlign );


sub new {
    my $caller = shift;
    my $class  = ref( $caller ) || $caller;
    my $self   = $class -> SUPER::new();
    $self -> { 'SS_CONS' }      = undef;
    $self -> { 'MATCH_STATES' } = undef;
    return $self;
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
	$self -> ss_cons( $ss_cons );
    }
    if( $match_states ) {
	$self -> match_states( $match_states );
    } 

#    print "$ss_cons\n$match_states\n";

    return $count; 
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
	if( $self->ss_cons() ) {
	    my $subcons  = substr( $self->ss_cons(), $i*$block, $block );
	    print $out sprintf( "%-".$maxn."s  %s\n", "\#=GC SS_cons", $subcons );
	}
	print $out "\n" unless( ($i+1) >= $iter );
    }
    print $out "\/\/\n";
}

1;
