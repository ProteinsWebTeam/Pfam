# object represents a base pair
# simply a 2 element array

=head1 NAME

Bio::Rfam::Pair

=head1 SYNOPSIS


=head1 DESCRIPTION


=cut

package Bio::Rfam::Pair;
use vars qw( $AUTOLOAD @ISA @EXPORT_OK );
use strict;


sub new {
    my $caller = shift;
    my $left   = shift;
    my $right  = shift;
    my $class  = ref( $caller ) || $caller;
    my $self = [];
    if( $right and $right >= $left ) {
	$self = [ $left, $right ];
    }
    elsif( $right and $left > $right ) {
	$self = [ $right, $left ];
    }
	bless( $self, $class );

    return $self;
}

sub left {
    my $self = shift;
    my $left = shift;
    $self->[0] = $left if( $left );
    return $self->[0];
}

sub right {
    my $self  = shift;
    my $right = shift;
    $self->[1] = $right if( $right );
    return $self->[1];
}

1;
