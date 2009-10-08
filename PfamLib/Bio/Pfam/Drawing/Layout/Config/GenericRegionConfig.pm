
# $Author: jt6 $


package Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;

use strict;
use warnings;
use Convert::Color;

use Moose;
use Moose::Util::TypeConstraints;


has 'colourIndex' => (
  isa     => 'Int',
  is      => 'rw',
  default => '0'
);

sub configureRegion {
  my ($self, $region) = @_;
  # set up the shape type
  
  $self->_setEdges;

   #As we do not knw what sort of region this is we can nt construct a url

  #Now contruct the label
  #$self->constructLabel($region);
  
  #Now Colour the Region
  $self->_setColours($region);
}

sub constructLabel{
  my ($self, $region) = @_;
  
  my $label = 'wibble'; 
}

 
#This sets the generic region to a dark grey colour
sub _setColour{
  my ($self, $region) = @_;
  $region->colour( Convert::Color->new( 'x11:grey') );
}

sub _setEdges{
  my ($self, $region) = @_;
  $region->startStyle('curved');
  $region->endStyle('curved');  
}


=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut


1;

