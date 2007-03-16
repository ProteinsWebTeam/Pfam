
#
# Perl Module for HMMSequence
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
#Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package HMMSequence;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;
use warnings;
use HMMUnit;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

@ISA = ( 'Exporter' );


my %fields = (
    #Insert field names here as field => undef,
	      name => undef,
	      desc => undef,
	      bits => undef,
	      evalue => undef,
	      domain => undef, # will be an array of HMMUnits
);


sub new {
    my $ref = shift;
    my $class = ref($ref) || $ref;
    my $self = {
	'_permitted' => \%fields,
	%fields, };

    $self->{'domain'} = [];

    bless $self, $class;
    return $self;
}


sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) || carp "$self is not an object - can't therefore find a member!";
    my $name = $AUTOLOAD;
    $name =~ /::DESTROY/ && return;
    $name =~ s/.*://;
    unless (exists $self->{'_permitted'}->{$name} ) {
	carp "In type $type, can't access $name - probably passed a wrong variable into HMMSequence";
    }
    if (@_) {
	return $self->{$name} = shift;
    } else {
	return $self->{$name};
    }
}


sub addHMMUnit {
    my $self = shift;
    my $unit = shift;

    push(@{$self->{'domain'}},$unit); 
}

sub eachHMMUnit {
    my $self = shift;
    
    return @{$self->{'domain'}};
}




=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut


1;

__END__

=head1 NAME

HMMSequence

=head1 DESCRIPTION

HMMSequence is one of the components for the L<HMMResults>
modules. Each HMMSequence is meant to represent a single
Sequence which has been parsed by the HMMer2 package. It has
a list of HMMUnits, which is the individual HMM hits and a
name

=head1 AUTHOR

B<Ewan Birney> Email birney@sanger.ac.uk

=over

=item addHMMUnit

This adds a single HMMUnit to the HMMSequence

=item eachHMMUnit

Provides an array of HMMUnits

