
# $Author: jt6 $

package Bio::Pfam::Drawing::Colour::rgbColour;

use strict;
use warnings;

sub new {
  my $caller = shift;
  my %params = @_;
  my $self = bless {}, ref($caller) || $caller;

  my $red = ($params{'-r'}||$params{'-R'});
  my $green = ($params{'-g'}||$params{'-g'});
  my $blue = ($params{'-b'}||$params{'-b'});
  
  $self->red($red);
  $self->green($green);
  $self->blue($blue);
  
  return $self;
}

sub red {
  my ($self, $red) = @_;
  if($red){
    if($red =~ /\d+/){
      $self->{'r'} = $red
    }else{
      warn "This is not integer, $red, setting red channel to 0\n";
      $self->{'r'}= "0";
    }
  }
  return $self->{'r'};
}

sub green {
  my ($self, $green) = @_;
  if($green){
    if($green =~ /\d+/){
      $self->{'g'} = $green
    }else{
      warn "This is not integer, $green, setting green channel to 0\n";
      $self->{'g'}= "0";
    }
  }
  return $self->{'g'};
}

sub blue {
  my ($self, $blue) = @_;
  if($blue){
    if($blue =~ /\d+/){
      $self->{'b'} = $blue
    }else{
      warn "This is not integer, $blue, setting blue channel to 0\n";
      $self->{'b'}= "0";
    }
  }
  return $self->{'b'};
}


sub colour2XMLDOM{
  my ($self, $dom) = @_;
  my $colour_element = $dom->createElement("colour");
  my $colour = $dom->createElement("RGB");
  $colour_element->appendChild($colour);
  $colour->setAttribute("R", $self->red);
  $colour->setAttribute("G", $self->green);
  $colour->setAttribute("B", $self->blue);
  return $colour_element;
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

