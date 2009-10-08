
# $Author: jt6 $


package Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig;

use strict;
use warnings;
use Convert::Color;

use Moose;
use Moose::Util::TypeConstraints;


sub configureMarkup {
  my ($self, $markup) = @_;
  
  #Now contruct the label
  #$self->constructLabel($region);
  
  #Set where to display this feature
  $self->_setPosition($markup);
  
  $self->_setStyle($markup);
  
  #Now Colour the Region
  $self->_setColour($markup);
}

sub constructLabel{
  my ($self, $region) = @_;
  
  my $label = 'wibble'; 
}
 
#This sets the generic region to a dark grey colour
sub _setColour{
  my ($self, $markup) = @_;
  
  # print STDERR "This colour\n";
  
  #This sets to colour of the lollipop
  # - If we have a bridge (i.e. end value set) then this will not be set
  unless($markup->end){
    $markup->colour( Convert::Color->new( 'rgb8:CCCCCC') );
    $markup->lineColour( Convert::Color->new( 'rgb8:CCCCCC') );
  }
  #This sets the line colour
  #$markup->lineColour( Convert::Color->new( 'rgb8:000000') );
  $markup->colour( Convert::Color->new( 'rgb8:CCCCCC') );
  
}

sub _setPosition {
  my ($self, $markup) = @_;
  #Do we want to draw this feature above or below the sequence?
  $markup->v_align('top');
}

sub _setStyle {
  my ($self, $markup) = @_;
  $markup->headStyle('line') unless($markup->end);  
  
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

