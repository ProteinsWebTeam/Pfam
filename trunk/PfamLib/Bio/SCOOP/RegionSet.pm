=head1 NAME

Bio::SCOOP::RegionSet

=head1 DESCRIPTION

B<RegionSet> contains a collection of region objects.

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
# Perl Module for RegionSet
#
# Cared for by Alex Bateman <agb@sanger.ac.uk>
#

package Bio::SCOOP::RegionSet;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;
use warnings;
use Bio::SCOOP::Region;

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

=head1 new

Creates RegionSet object.

$regionset = new RegionSet($id);

=cut

sub new {
    my ($class,$id)=@_;

    my $set = {
	"id"      => $id,
	"regions" => [],
    };

    bless $set, $class;
    return $set;
}

#######
# add #
#######

=head1 add

Add region to set object

    $regionset->add($region1..$regionN);

=cut
sub add {
  my $self = shift @_;
  push(@{$self->{'regions'}},@_);
}


########
# each #
########

=head1 each

Loops over and returns a list of objects

    @list = $self->each();

=cut

sub each {
  my $self= shift @_;
  my @list=@{$self->{'regions'}};
  return @list;
}

#########
# write #
#########

=head1 write

Loops over List and writes element

    $self->write();

=cut

sub write {
    my $self = shift @_;

    my @list=$self->each();
    foreach my $element (@list){
	$element->write();
    }
}




1;  # says use was ok
__END__

