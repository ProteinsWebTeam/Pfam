
# $Author: jt6 $

package Bio::Pfam::Drawing::Colour::hexColour;

use strict;
use warnings;

sub new {
  my $caller = shift;
  my %params = @_;
  my $self = bless {}, ref($caller) || $caller;
  my $colour = ($params{'-colour'}||$params{'-COLOUR'});
  $self->colour($colour);
  return $self;
}

sub colour{
  my ($self, $colour) = @_;
  if($colour){
    if($colour =~ /#?(\S+)/){
      $self->{'colour'} = $1
    }else{
      warn "This is not a HEX colour string:$colour, setting to black\n";
      $self->{'colour'}= "000000";
    }
  }
  return $self->{'colour'};
}

sub colour2XMLDOM{
  my ($self, $dom) = @_;
  my $colour_element = $dom->createElement("colour");
  my $colour = $dom->createElement("hex");
  $colour_element->appendChild($colour);
  $colour->setAttribute("hexcode", $self->colour);
  return $colour_element;
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

