# object represents a base pair
# simply a 2 element array

=head1 NAME

Rfam::Pair

=head1 SYNOPSIS


=head1 DESCRIPTION


=cut

package Rfam::Pair;
use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;


sub new {
    my( $caller, $left, $right, $knot ) = @_;
    my $class  = ref( $caller ) || $caller;

    my $self = { 'LEFT'  => undef,
	         'RIGHT' => undef };
    bless( $self, $class );

    if( $right and $right >= $left ) {
	$self->left($left);
	$self->right($right);
    }
    elsif( $right and $left > $right ) {
	$self->left($right);
	$self->right($left);
    }

    $self->knot($knot) if $knot;

    return $self;
}

sub left {
    my $self = shift;
    my $left = shift;
    $self->{'LEFT'} = $left if( $left );
    return $self->{'LEFT'};
}

sub right {
    my $self  = shift;
    my $right = shift;
    $self->{'RIGHT'} = $right if( $right );
    return $self->{'RIGHT'};
}

sub knot {
    my $self  = shift;
    my $knot  = shift;
    $self->{'KNOT'} = $knot if( defined $knot );
    return $self->{'KNOT'};
}

1;
