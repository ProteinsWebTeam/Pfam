
#
# Perl Module for Range
#
# Cared for by Matthew Pocock <mrp@sanger.ac.uk>
#
# Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package Range;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);

use Exporter;
use Carp;
use strict;
use warnings;

@ISA = ( 'Exporter');


#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();



my %fields = (
	       #Insert field names here as field => value,
	   start => undef,  # range start
	   end => undef     # range end
	   );

sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;

    my $self = {
	'_permitted' => \%fields,
	%fields, };

    bless $self, $class;

    # yuk! - Ewan

    if(@_ == 2) {
	$self->start(shift);
	$self->end(shift);
    } elsif (@_) {
	print STDERR "Usage: new Range()\n\tnew Range(start, stop)\n";
    }

    return $self;
}

#
# Autoload up my field name set/get
#
sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) || carp "Ok, $self is not an object of mine!";
    my $name = $AUTOLOAD;

    # don't propagate DESTROY messages...

    $name =~ /::DESTROY/ && return;

    $name =~ s/.*://; #get only the bit we want
    unless (exists $self->{'_permitted'}->{$name} ) {
	carp "In type $type, can't access $name - probably passed a wrong variable into Sequence  ";
    }
    if (@_) {
	return $self->{$name} = shift;
    } else {
	return $self->{$name}
    }
}

#____________________________________________________________________
# Title    : overlap  -from Alex bateman
# Function : Takes pair of domain boundaries and says if overlapping
#          : Overlap if midpoint of either sequence lies in the other
# Usage    : &overlap($from,$to,$from,$to);
# Returns  : 1 if overlapping
#____________________________________________________________________
sub overlap_midpoint{
    my $self = shift;
    my $range = shift;
    my ($mid1,$mid2);

    $mid1=($self->start() + $self->end() )/2;
    $mid2=($range->start() + $range->end() )/2;
    
    if( $mid1 > $range->start() && $mid1 < $range->end()) {
	return 1;
    }
    if( $mid2 > $self->start() && $mid2 < $self->end()) {
	return 1;
    }

    return 0;
}


sub clone {
    my $self = shift;
    my $clone = new Range($self->start(), $self->end());
    return $clone;
}

sub length {
    my $self = shift;
    my $start = $self->start;
    my $end = $self->end;

    defined $start || return undef;
    defined $end || return undef;

    return abs($end - $start) + 1;
}

sub isInverted {
    my $self = shift;

    return $self->end < $self->start;
}

sub translate {
    my $self = shift;
    my $dist = shift;

    $self->start($self->start + $dist);
    $self->end($self->end + $dist);
   
    return $self;
}

sub invert {
    my $self = shift;

    my $temp = $self->start;
    $self->start($self->end);
    $self->end($temp);

    return $self;
}

sub toText {
    my $self = shift;
    return $self->start."..".$self->end;
}

sub toString {
  my $self = shift;
  return '('.$self->start.','.$self->end.')';
}

sub within {
    my $self = shift;
    my $range = shift;

    # self within range if self starts after range starts and ends before range ends
    if(($range->start < $self->start) && ($range->end > $self->end)) {
	return 1;
    } else {
	return 0;
    }
}

sub overlapping {
    my $self = shift;
    my $range = shift;
    my $sstart = $self->start;
    my $send = $self->end;
    my $rstart = $range->start();
    my $rend = $range->end();
    if(
       ($sstart <=> $rstart) == ($send <=> $rstart) && # start and end same side of query start
       ($sstart <=> $rend) == ($send <=> $rend) &&     # start and end same side of query end
       ($sstart <=> $rstart) == ($send <=> $rend)      # the two 'sides' are the same side
       ) {
	return 0;  # the two ranges do not overlap
    } else {
	return 1;  # the two ranges do overlap
    }
}

sub combine {
    my ($self, $range1, $range2);
    my @points;

    if(@_ < 2 || @_ > 3) {
	print STDERR "Usage: Range->compose($range1, $range2)\n\t$range1->compose($range2)\n";
	return undef;
    }

    if(@_ == 3) {
	print "got 3 @_\n";
	shift;
        ($range1, $range2) = @_;
	$self = $range1->clone();
    } else {
	print "got 2 @_\n";
	($range1, $range2) = @_;
	$self = $range1;
    }
	
    @points = ($range1->start(), $range1->end(), $range2->start(), $range2->end());
    @points = sort { $a <=> $b} @points;

    $self->start(shift @points);
    $self->end(pop @points);

    return $self;
}

sub between {
  my ($class, $range1, $range2) = @_;
  my @points = ($range1->start, $range1->end, $range2->start, $range2->end);
  @points = sort { $a <=> $b } @points;                           # sort lowest to highest
  my ($start,$end) = ($points[1]+1, $points[2]-1);  # get the two middle values +/- 1
  my $between = new Range($start,$end);
  ($range1->isInverted()) && $between->invert;
  return $between;
}

1;  # says use was ok

__END__

=head1 NAME

Range

=head1 DESCRIPTION

B<Range> represents a range of bases.  It has a start and end position,
and these co-ordinates are taken to be inclusive of the sequence it refers
to - thus a BaseRange starting at 3 and ending at 9 will refer to the bases
3-9 inclusive of a sequence.

=head1 AUTHOR

B<Matthew Pocock> Email mrp@sanger.ac.uk

=over

=item new

Creates a new Range, optionaly taking a start and end number for the range.

=item clone

Creates a new Range from an already created range. Use it to copy range objects.

=item start

Returns the current start position.  Set it using $range->start($start).

=item end

Returns the current end position.  Set it using $range->end($end).

=item length

Returns the length of the range.  This will always be a positive integer,
regardless of the orientation of the range.

=item isInverted

Returns true if the end position is lower than the start position.  Usefull
if this is how you refer to the other strand.

=item translate

Translates the BaseRange by adding the argument to both start
and end co-ordinates.  This modifies the BaseRange, it does not copy it.
It returns the translated object to allow chaining.

=item isInverted

Returns true if the end position is lower than the start position.  Usefull
if this is how you refer to the other strand.

=item invert

Swaps the start and end co-ordinates.  Usefull for when you use the
start and end co-ordinates to specify strandidness.  It returns the
object to allow chaining.  As for translate, it affects the object it
was called on, not a copy.

=item toText

Returns a string that looks like start..end.

=item within

Use:

    $range->within($boundingRange);

Returns true if $range is contained entirely within $boundingRange (e.g. it is nested), and false otherwise.
Currently returns false if they start or end at the same position.

Note $a->within($b) is not equivelent to $b->within($a).

=item overlapping

Use :

    $range->overlapping($otherRange);

Returns true if $range and $otherRange overlap at all. Returns false of they have no overlap.

Note $a->overlapping($b) is equivelent to $b->overlapping($a).

=item combine

Use:

    $range->combine($otherRange);
    or
    $r = Range->combine($range1, $range2);

Combines two ranges into a single range, and returns that combined range.
When called as a member function, it modifies the object for which it was invoked.
When called as a package function, it returns a new range made by cloning $range1, and
then combining the clone with $range2.

=item between

Use:
    $range = Range->between($range1, $range2);

Returns a new range object that represents the range betwwen $range1
and $range2. It will have start and end sorted in the same direction
as $range1. The returned range will not overlap either $range1 or
$range2, but touch them. I.e., using 3,5 and 12,10 the between will be
6,9. Using 5,3 and 12,10 the between will be 9,6.

=back
