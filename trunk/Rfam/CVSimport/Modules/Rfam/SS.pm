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

package Rfam::SS;
use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;
use Rfam::Pair;

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
    $self->{'MAP'}->{$pair->left}  = $pair->right;
    $self->{'MAP'}->{$pair->right} = $pair->left;
}


sub eachPair {
    my $self = shift;
    my @sort = sort { $a->left() <=> $b->left() } @{$self->{'PAIR'}};
    return @sort;
}



sub get_knotted {
    my $self = shift;

    my @pair = sort{ $a->left <=> $b->left } $self->eachPair;
    my( @knot, %count );

    # consider each pair of base pairs, and fill a matrix with
    # whether they are knotted with respect to one another
    for( my $i=0; $i<@pair; $i++ ) {
	for( my $j=$i+1; $j<@pair; $j++ ) {
	    if( $pair[$j]->left < $pair[$i]->right and $pair[$j]->right > $pair[$i]->right ) {
		# this pair of pairs are knotted
		$knot[$i][$j] = 1;
		# also keep a count of the number of knots involved for
		# each base pair
		$count{$i} ++;
		$count{$j} ++;
	    }
	    else {
		# this pair of pairs are nested (or completely seperate)
		$knot[$i][$j] = 0;
	    }
	}
    }
    
    foreach my $c ( sort{ $count{$b}<=>$count{$a} } keys %count ) {
	next unless( $count{$c} );

	$pair[$c]->knot(1);   # set this pair as a knot

 	for( my $i=0; $i<@pair; $i++ ) {
	    $count{$i}-- if( $knot[$c][$i] );    # remove column $c from the counts so
	    $count{$i}-- if( $knot[$i][$c] );    # decrement any count involving $c
	}
    }

    # now go back and assign the pseudoknot helix number
    my $k=0;
    my @letter = qw( - A B C D E F G H I );
    
    for( my $i=0; $i<@pair; $i++ ) {
	next unless( $pair[$i]->knot() );

	# 012 34 56 210 43 56
	# <<<.AA.BB.>>>.aa.bb matrix looks like
	# 
	#   0 1 2 3 4 5
	# 0 - 0 0 1 1 1
	# 1   - 0 1 1 1
	# 2     - 1 1 1
	# 3       - 0 1
	# 4         - 1
	# 5           -
	#
	# where 0's reach down to diagonal we have a new helix

	if( $i==0 or $knot[$i-1][$i] ) {
	    $k++;
	}
	$pair[$i]->knot($letter[$k]);
    }
}


sub getInfernalString {
    my $self = shift;
    my @ary;

    foreach my $pair ( $self->eachPair() ) {
	if( $pair->knot() ) {   # we have a pseudoknot!
	    $ary[ $pair->left() -1  ] = uc( $pair->knot );
	    $ary[ $pair->right() -1 ] = lc( $pair->knot );
	}
	else {
	    $ary[ $pair->left() -1  ] = "<";
	    $ary[ $pair->right() -1 ] = ">";
	}
    }
    
    for( my $i=0; $i<$self->length(); $i++ ) { # fill in gap chars
	$ary[$i] = '.' if( not $ary[$i] );
    }
    return join( '', @ary );
}


sub getViennaString {
    my $self = shift;
    my $str = $self -> getInfernalString();
    $str =~ s/[\<\{\[]/\(/g;
    $str =~ s/[\>\}\]]/\)/g;
    return $str;
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
	my @sort = sort { $b->right() <=> $a->right() } $self->eachPair();
	return $sort[0]->right();
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
    my %knot; # This will be populated with positions of letters
              # marking pseudoknot bases.  To cope with complex cases
              # the key will be the letter used.

    for( my $i=1; $i<=@ss; $i++ ) {
	if( $ss[$i-1] =~ /[\[\(\{\<]/ ) {
	    push( @open, $i );
	}
	elsif( $ss[$i-1] =~ /([A-Z])/ ) {
	    push( @{ $knot{$1} }, $i );
	}
	elsif( $ss[$i-1] =~ /[\]\)\}\>]/ ) {
	    my $j = pop @open || -1;
	    my $pair = new Rfam::Pair( $j, $i );
	    $self -> addPair( $pair );
	}
	elsif( $ss[$i-1] =~ /([a-z])/ ) {
	    my $j = pop @{ $knot{uc($1)} } || -1;
	    my $pair = new Rfam::Pair( $j, $i, uc($1) );
	    $self -> addPair( $pair );
	}
    }
}


sub removeColumn {
    my $self = shift;
    my @cols = @_;
    
    # first deal with any pair that involves $col
    foreach my $col ( @cols ) {
	if( $self->pairedBase( $col ) ) {
	    my $pair = $self->getPairByCol( $col );
	    $self->removePair( $self->getPairByCol( $col ) );
	}
    }

    # correct the length
    $self->length( $self->length - @cols );

    # then decrement all column numbers after $col
    foreach my $pair ( $self->eachPair() ) {
	my( $leftcount, $rightcount );
	foreach my $col ( @cols ) {
	    if( $pair->left() > $col ) {
		$leftcount ++;
	    }
	    if( $pair->right() > $col ) {
		$rightcount ++;
	    }
	}
	$pair->left(  $pair->left  - $leftcount )  if( $leftcount );
	$pair->right( $pair->right - $rightcount ) if( $rightcount );
    }

    $self -> _rebuild_map();
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


sub column_colourmap {
    my $self = shift;

    my $colour = 1;
    my $lastopen  = 0;
    my $lastclose = 999999999999999;
    my $nest = 0;
    my %colmap;

    $self -> _rebuild_map();  # just make sure

    foreach my $pair ( sort { $a->right() <=> $b->right() } $self->eachPair() ) {

	# catch things like ...
	# <<..>>..<<..>>
	#          *
	if( $pair->left > $lastclose ) {
	    $colour++;
	}

	# catch things like ...
	# <<..<<..>>..<<..>>>>
	#                   *
	foreach my $donepair ( grep{ ( $_->left < $lastopen ) and 
				     ( $_->left > $pair->left ) } $self->eachPair() ) {
	    if( exists $colmap{ $donepair->left } and $colmap{ $donepair->left } != $colour ) {
		$colour++;
		last;
	    }
	}

	$colmap{ $pair->left }  = $colour;
	$colmap{ $pair->right } = $colour;

	$lastclose = $pair->right();
	$lastopen  = $pair->left();
    }
    return \%colmap;
}


sub _rebuild_map {
    my $self = shift;
    $self -> {'MAP'} = {};
    foreach my $pair ( $self -> eachPair() ) {
	$self -> {'MAP'} -> {$pair->left}  = $pair->right;
	$self -> {'MAP'} -> {$pair->right} = $pair->left;
    }
}


1;
