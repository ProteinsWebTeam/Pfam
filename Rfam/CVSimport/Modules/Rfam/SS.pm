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
    my @letters = qw( - A B C D E F G H I J );
    my $k = 0;

    foreach my $pair ( sort{ $a->right - $a->left <=> $b->right - $b->left } 
		       #sort{ $a->left <=> $b->left }
		       $self->eachPair() ) {
	next if $pair->knot();    # ignore if we're already a knot
	my $j = 0;
	my @pairs;
	for( my $i=$pair->left+1; $i<$pair->right; $i++ ) {
	    # loop over all pairs with base between left and right
	    if( my $p = $self->getPairByCol($i) ) {
		next if $p->knot();  # ignore if we're a knot
		$j++ if( $p->left == $i );
		$j-- if( $p->right == $i );
		push( @pairs, $p );  # store the pair
	    }
	}

	$k++ if( $j != 0 );

	while( $j != 0 ) {      # something is pseudoknotted
	    foreach my $p ( @pairs ) {
		if( $j>0 ) {    # we have too many opens
		    # if pair closes here then removing it won't help
		    next if( $p->right > $pair->left and
			     $p->right < $pair->right );
		    $j --;
		}
		else {          # we have too many closes
		    # if pair opens here then removing it won't help
		    next if( $p->left > $pair->left and
			     $p->left < $pair->right );
		    $j ++;
		}

		$p->knot( $letters[$k] );   # clasify this pair as a knot
	    }
	}
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
