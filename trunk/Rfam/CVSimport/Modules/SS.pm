# object to represent secondary structure
# takes form of a hash of Pairs objects indexed by left column
# can represent a consensus of an alignment (used in RfamAlign) or
# a single sequence
# in theory the object can handle pseudoknots but somethings using this
# will not

=head1 NAME

Bio::Rfam::SS

=head1 SYNOPSIS


=head1 DESCRIPTION


=cut

package Bio::Rfam::SS;
use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;
use Bio::Rfam::Pair;

################################################
# Use the hashmap with care -- mostly untested #
################################################


sub new {
    my $caller = shift;
    my $class  = ref( $caller ) || $caller;
    my $self = {};
    $self->{'PAIR'}   = [];     # array of Pair objects
    $self->{'MAP'}    = {};     # hash map left<=>right
    $self->{'LENGTH'} = undef;  # length of structure
    bless( $self, $class );
    return $self;
}


sub addPair {
    my $self = shift;
    my $pair = shift;
    push( @{$self->{'PAIR'}}, $pair );
    $self->{'MAP'}->{$pair->left}  = $pair->left;
    $self->{'MAP'}->{$pair->right} = $pair->right;
}


sub eachPair {
    my $self = shift;
    my @sort = sort { $a->[0] <=> $b->[0] } @{$self->{'PAIR'}};
    return @sort;
}


sub getPairByCol {
    my $self = shift;
    my $col  = shift;
 
    foreach my $pair ( $self->eachPair() ) {
	if( $pair->left() == $col or $pair->right() == $col ) {
	    return $pair;
	}
    }
    return 0;
}



sub length {
    my $self = shift;
    my $length = shift;
    if( defined $length ) {
	$self->{'LENGTH'} = $length;
    }
    elsif( $self->{'LENGTH'} ) {
	return $self->{'LENGTH'};
    }
    else {
	my @sort = sort { $b->[1] <=> $a->[1] } $self->eachPair();
	return $sort[0]->[1];
    }
}

sub pairedBase {
    my $self  = shift;
    my $base1 = shift;
    return $self->{'MAP'}->{$base1};
}

sub parseInfernalString {
    my $self = shift;
    my $str  = shift;

    my @ss = split( //, $str );
    $self -> length( scalar( @ss ) );
    
    my @open;
    for( my $i=1; $i<=@ss; $i++ ) {
	if( $ss[$i-1] =~ /[\[\(\{\<]/ ) {
	    push( @open, $i );
	}
	elsif( $ss[$i-1] =~ /[\]\)\}\>]/ ) {
	    my $j = pop @open || -1;
	    my $pair = new Bio::Rfam::Pair( $j, $i );
	    $self -> addPair( $pair );
	}
    }
}


sub removeColumn {
    my $self = shift;
    my @cols = @_;
    
    foreach my $col ( @cols ) {
	# take one off the length
	$self->length( $self->length -1 );

	# first deal with any pair that involves $col
	if( $self->pairedBase( $col ) ) {
	    $self->removePair( $self->getPairByCol( $col ) );
	}

	# then decrement all column numbers after $col
	foreach my $pair ( $self->eachPair() ) {
	    if( $pair->left() > $col ) {
		delete $self->{'MAP'}->{$pair->left};  # deal with map
		delete $self->{'MAP'}->{$pair->right};
		$pair->left(  $pair->left  -1 );
		$pair->right( $pair->right -1 );
		$self->{'MAP'}->{$pair->left}  = $pair->right;
		$self->{'MAP'}->{$pair->right} = $pair->left;
	    }
	    elsif( $pair->right() > $col ) {
		delete $self->{'MAP'}->{$pair->right}; # deal with map
		$pair->right( $pair->right -1 );
		$self->{'MAP'}->{$pair->left}  = $pair->right;
		$self->{'MAP'}->{$pair->right} = $pair->left;	    
	    }
	}
    }
}


sub removePair {
    my $self = shift;
    my $pair = shift;

    # set length explicitely in case we're using coordinates of pairs 
    $self->length( $self->length );

    # remove pair from list
    for( my $i=0; $i<@{$self->{'PAIR'}}; $i++ ) {
	if( $self->{'PAIR'}->[$i] eq $pair ) {
	    splice( @{$self->{'PAIR'}}, $i, 1 );
	}
    }

    # sort map
    delete $self->{'MAP'}->{$pair->left};
    delete $self->{'MAP'}->{$pair->right};

    return $pair;
}


sub writeInfernalString {
    my $self = shift;
    my @ary;
    foreach my $pair ( $self->eachPair() ) {
	$ary[ $pair->left() -1  ] = "<";
	$ary[ $pair->right() -1 ] = ">";
    }
    
    for( my $i=0; $i<$self->length(); $i++ ) {
	$ary[$i] = '.' if( not $ary[$i] );
    }
    return join( '', @ary );
}

1;
