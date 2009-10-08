=head1 NAME

Region

=head1 DESCRIPTION

The Region object contains info dedicated to a protein region

=head1 AUTHOR

B<Alex Bateman> Email agb@sanger.ac.uk


#COPYRIGHT

#Copyright (c) 2007: Genome Research Ltd.

#Authors:	Alex Bateman (agb at sanger.ac.uk)
#		K.Cara Woodwark (cara at sanger.ac.uk)

#This is free software; you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free Software
#Foundation; either version 2 of the License, or (at your option) any later
#version.

#This program is distributed in the hope that it will be useful, but WITHOUT
#ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
#details.

#You should have received a copy of the GNU General Public License along with
#this program. If not, see <http://www.gnu.org/licenses/>.



=cut

#
# Perl Module for Bio::SCOOP::Region
#
# Cared for by Alex Bateman <agb@sanger.ac.uk>
#

package Bio::SCOOP::Region;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

@ISA = ( 'Exporter' );

#######
# new #
#######

=head2 new

Create new region object.

    $region = new Region('id'=>$id,
			 'start'=>$$res[2],
		         'end'=>$$res[3],
		         'family'=>$$res[0],
		         'evalue'=>$$res[4],
		         'significant'=>$$res[5]);


=cut

sub new {
    my ($class,%hash)=@_;

    my $region = {
    };

    bless $region, $class;

    $region->{'id'}=$hash{'id'};
    $region->{'start'}=$hash{'start'};
    $region->{'end'}=$hash{'end'};
    $region->{'family'}=$hash{'family'};
    $region->{'evalue'}=$hash{'evalue'};
    $region->{'significant'}=$hash{'significant'};
    $region->{'match'}={};

    return $region;
} 

#########
# write #
#########

=head2 write

Debugging method: prints region object data to STDERR

    $region->write();

=cut

sub write {
    my ($self)=@_;

    print STDERR "Region: id:",$self->{'id'}," start:",$self->start()," end:",$self->end()," family:",$self->family()," score:",$self->{'evalue'},"\n";

}


###########
# overlap #
###########

=head2 overlap

Method to check if two regions overlap

    $region1->overlap($region2);

Returns true if there is an overlap

=cut

sub overlap {
    my ($region1,$region2)=@_;

    my $start1=$region1->{'start'};
    my $start2=$region2->{'start'};
    my $end1  =$region1->{'end'};
    my $end2  =$region2->{'end'};

    
    # This method requires the midpoint of one region to lie within the other
    my $mid1=($start1+$end1)/2;
    my $mid2=($start2+$end2)/2;
    
    if ($mid1>$start2 and $mid1<$end2){
	return 1;
    } elsif ($mid2>$start1 and $mid2<$end1){
	return 1;
    }
    return 0;
}

#########
# id #
#########

sub id {
    my $self = shift @_;

    return $self->{'id'};
}

#########
# start #
#########

sub start {
    my $self = shift @_;

    return $self->{'start'};
}

#######
# end #
#######

sub end {
    my $self = shift @_;

    return $self->{'end'};
}

##########
# family #
##########

sub family {
    my $self = shift @_;

    return $self->{'family'};
}


###############
# significant #
###############

sub significant {
    my $self = shift @_;

    return $self->{'significant'};
}

#########
# match #
#########

# Test whether this region has matched to another output already
# This is used to stop multiple counting for repeated regions.
sub match {
    my ($self,$match) = @_;

    if ($self->{'match'}->{$match}){
	return 1;
    } else {
	return 0;
    }
}

#############
# add_match #
#############

# Add an element to the match hash
sub add_match {
    my ($self,$match) = @_;

    $self->{'match'}->{$match}=1;
}


##########
# evalue #
##########

sub evalue {
    my $self = shift @_;

    return $self->{'evalue'};
}





1;  # says use was ok
__END__
