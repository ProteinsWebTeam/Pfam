
# $Author: jt6 $


package Bio::Pfam::Drawing::Layout::Config::TransmembraneConfig;

use vars qw($AUTOLOAD @ISA $VERSION);
use strict;
use warnings;


use Bio::Pfam::Drawing::Layout::Region;
use Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;

@ISA = qw(Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig);


sub configure_Region {
  my ($self, $region) = @_;
  # set up the shape type
  $region->type("smlShape");

  #A small image does not have ends, so we do not need to set them

  #Now construct the URL
  $self->_construct_URL($region);

  #Now contruct the label
  $self->_construct_label($region);
  
  #Now Colour the Region
  $self->_set_colours($region);
}

sub _construct_URL {
  my ($self, $region) = @_;
  #$region->url($Bio::Pfam::Web::PfamWWWConfig::region_help);
}

sub _construct_label{
  my ($self, $region) = @_;
  $region->label("Transmembrane");
}

sub _set_colours{
  my ($self, $region) = @_;
  my $colour1 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "FF0000");
  $region->colour1($colour1);
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
