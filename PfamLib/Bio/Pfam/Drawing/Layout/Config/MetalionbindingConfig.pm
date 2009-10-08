# Bio::Pfam::Drawing::Layout::Config::MetalionbindingConfig
#
# Author:        rdf
# Maintainer:    $Id: MetalionbindingConfig.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Aug 10, 2009
# Last Modified: $Date: 2009-10-08 12:27:28 $
=head1 NAME

Template - a short description of the class

=cut

package Bio::Pfam::Drawing::Layout::Config::MetalionbindingConfig;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: MetalionbindingConfig.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: MetalionbindingConfig.pm

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

#use base "Some::Class";
use strict;
use warnings;
use Convert::Color;

use Moose;
use Moose::Util::TypeConstraints;

extends 'Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig';


has 'metalColour' => (
  isa => 'HashRef[Str]',
  is  => 'rw',
  required => 1,
  default =>  sub { {
    cadmium    => 'D0BCFE',
    calcium    => 'CDD11B',
    cobalt     => '7BA7E1',
    copper     => 'FF2626',
    iron       => 'FF8A8A', 
    magnesium  => '79FC4E',
    manganese  => 'EDA9BC', 
    mercury    => 'CCCCCC',
    molybdate  => '555555',
    molybdenum => '555555',
    nickel     => '33FDC0',
    potassium  => 'EEEEA2',
    sodium     => 'AAAAAA',
    tungsten   => 'FFF06A', 
    vanadium   => 'FFBE28',
    zinc       => 'B8E2EF'
  }},
);


#-------------------------------------------------------------------------------

=head1 METHODS

=cut



 

sub _setColour{
  my ($self, $markup) = @_;
  
  #This sets to colour of the lollipop
  # - If we have a bridge (i.e. end value set) then this will not be set
  if($markup->end){
    warn "There should not be an end for an metal binding site markup\n";
  }else{
    $markup->lineColour( Convert::Color->new( 'rgb8:333333') );
    #Default boring grey
    $markup->colour( Convert::Color->new( 'rgb8:AD8BFE') );
    
    foreach my $metal (keys %{ $self->metalColour }){
      if($markup->metadata->description =~ /$metal/){
          $markup->colour( Convert::Color->new( 'rgb8:'.$self->metalColour->{$metal}) );
      }  
    }
  }
  #This sets the line colour
  
  
}

sub _setPosition {
  my ($self, $markup) = @_;
  #Do we want to draw this feature above or below the sequence?
  $markup->v_align('bottom');
}

#-------------------------------------------------------------------------------
=head2 _setStyle 

  Title    : _setStyle
  Usage    : $config->_setStyle($markup); 
  Function : Sets the style for the config for the this markup object
  Args     : A Bio::Pfam::Sequence::Markup
  Returns  : Nothing, style set on the markup object.
  
=cut

sub _setStyle {
  my ($self, $markup) = @_;
  $markup->headStyle('circle') unless($markup->end);  
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

