
# $Author: jt6 $


package Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;

use strict;
use warnings;

use Bio::Pfam::Drawing::Layout::Region;
use Bio::Pfam::Drawing::Colour::hexColour;
use Bio::Pfam::Drawing::Colour::rgbColour;

sub new {
  my $caller = shift;
  my $self = bless {}, ref($caller) || $caller;
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

  return $self;
}

sub configure_Region {
  my ($self, $region) = @_;
  # set up the shape type
  $region->type("smlShape");

  #A small image does not have ends, so we do not need to set them

  #As we do not knw what sort of region this is we can nt construct a url

  #Now contruct the label
  $self->_construct_label($region);
  
  #Now Colour the Region
  $self->_set_colours($region);
}

sub _construct_label{
  my ($self, $region) = @_;
  if($region->BioAnnotatedRegion->display){
    $region->label($region->BioAnnotatedRegion->display);
  }
}

sub resolve_internal_overlaps {
  my ($self, $region1, $region2) = @_;
  #C-terminal coos of N-terminal region wins
}

#This sets the generic region to a dark grey colour
sub _set_colours{
  my ($self, $region) = @_;
  my $colour = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "707070");
  $region->colour1($colour);
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

