# Bio::Pfam::Drawing::Layout::Config::DisulphideConfig
#
# Author:        rdf
# Maintainer:    $Id: DisulphideConfig.pm,v 1.6 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.6 $
# Created:       Aug 10, 2009
# Last Modified: $Date: 2009-10-08 12:27:28 $
=head1 NAME

Template - a short description of the class

=cut

package Bio::Pfam::Drawing::Layout::Config::DisulphideConfig;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: DisulphideConfig.pm,v 1.6 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: DisulphideConfig.pm

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



use strict;
use warnings;
use Convert::Color;

use Moose;
use Moose::Util::TypeConstraints;

extends 'Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig';

#-------------------------------------------------------------------------------

=head1 METHODS

=cut





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

#-------------------------------------------------------------------------------
=head2 _setStyle 

  Title    : _setStyle
  Usage    : $config->_setStyle($markup); 
  Function : Sets the style for the config for the this markup object
  Args     : A Bio::Pfam::Sequence::Markup
  Returns  : Nothing, style set on the markup object.
  
=cut


sub _setColour{
  my ($self, $markup) = @_;
  
  # print STDERR "This colour\n";
  
  #This sets to colour of the lollipop
  # - If we have a bridge (i.e. end value set) then this will not be set
  if($markup->end){
    $markup->colour( Convert::Color->new( 'rgb8:AAAAAA') );
  }else{
    $markup->lineColour( Convert::Color->new( 'rgb8:AAAAAA') );
    $markup->colour( Convert::Color->new( 'x11:yellow') );
  }
  #This sets the line colour
  
  
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

