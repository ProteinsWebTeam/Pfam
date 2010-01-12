# TransmembraneConfig.pm
#
# Author:        rdf
# Maintainer:    $Id: PfambConfig.pm,v 1.9 2010-01-12 09:41:42 jm14 Exp $
# Version:       $Revision: 1.9 $
# Created:       Aug 10, 2009
# Last Modified: $Date: 2010-01-12 09:41:42 $
=head1 NAME

Template - a short description of the class

=cut

package Bio::Pfam::Drawing::Layout::Config::PfambConfig;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: PfambConfig.pm,v 1.9 2010-01-12 09:41:42 jm14 Exp $

=head1 COPYRIGHT

File: TransmembraneConfig.pm

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

extends 'Bio::Pfam::Drawing::Layout::Config::GenericMotifConfig';

subtype 'currentColourNo' =>
  as 'Num' =>
  where {$_ <= 360 and $_ >= 0 } =>
  message { "$_ is not a number between 0 and 360" };
  
coerce 'currentColourNo'
  => from 'Num'
   => via {
     my $c = ( $_ % 360 );
    return $c ;
   };

has 'currentColour' => (
  isa     => 'currentColourNo',
  is      => 'rw',
  coerce  => 1,
  default => rand(360)
);

has 'preDeterminedColours' => (
  isa   => 'ArrayRef[Str]',
  is    => 'rw',
  
  default => sub { [ qw(36F200 FF5353 5B5BFF FFE920  BA21E0 FF9C42 FF7DFF B9264F BABA21 C48484 1F88A7 CAFEB8 4A9586 CEB86C) ] }
#green red blue yellow purple orange cyan fuchsia	maroon olive brown teal bluegreen brown
);

has 'assignedColours' => (
  isa => 'HashRef[ArrayRef]',
  is  => 'rw'
);

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

sub configureMotif {
  my ($self, $region) = @_;

  #Now build the href
  $self->_constructHref($region);  
  #Now Colour the Region
  $self->_setColour($region);
}



#This sets the generic region to a dark grey colour
sub _setColour{
  my ($self, $motif) = @_;
 
  # print STDERR "****This Colour**** ".$motif->metadata->accession."\n"; ;
  my $colours;
  if($self->assignedColours and $self->assignedColours->{$motif->metadata->accession}){
    $colours = $self->assignedColours->{$motif->metadata->accession};
  }else{
    #randomly generate
 
     
     my $colour1 =  Convert::Color->new( 'hsv:' . $self->currentColour.',0.5,1.0');
     # print STDERR "The current colour is :".$self->currentColour."\n";
     $self->currentColour( $self->currentColour + 53 );
     my $h = $colour1->as_hsv->hue;
     my $colour2 = Convert::Color->new( 'hsv:'.( ( $h + 120 ) % 360 ).",". 
                                              $colour1->as_hsv->saturation.",".
                                              $colour1->as_hsv->value );
     my $colour3 = Convert::Color->new( 'hsv:'.( ( $h + 240 ) % 360 ).",". 
                                              $colour1->as_hsv->saturation.",".
                                              $colour1->as_hsv->value );
     
     
     
     push(@{$colours}, $colour1, $colour2, $colour3);
     $self->assignedColours ? $self->assignedColours->{$motif->metadata->accession} = $colours : $self->assignedColours( {$motif->metadata->accession => $colours});
     
  } 
  $motif->colour( $colours );
}


sub _constructHref{
  my ($self, $region) = @_;
  if($region->metadata->accession){
    $region->href('/pfamb/'.$region->metadata->accession);  
  }
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

