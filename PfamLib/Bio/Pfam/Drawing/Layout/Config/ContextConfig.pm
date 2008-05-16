
# $Author: jt6 $

=head1 Name

Bio::Pfam::Drawing::Layout::Config::ContextConfig - Contains all of the configuration for
displaying context domains.

=head1 Description

This config sets up the style of a context region graphic. It also resolves the 
overlaps between context domains. These methods work by overriding methods in 
Bio::Pfam::Drawing::Layout::Config::GenericConfig


=cut
package Bio::Pfam::Drawing::Layout::Config::ContextConfig;
use strict;
use warnings;


use vars qw($AUTOLOAD @ISA $VERSION);
use Bio::Pfam::Drawing::Layout::Region;
use Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;

@ISA = qw(Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig);

=head2


=cut

sub configure_Region {
  my ($self, $region) = @_;
  # set up the shape type
  $region->type("bigShape");
  #Now set the image ends
  $self->_leftStyle($region);
  $self->_rightStyle($region);

  #Now construct the URL
  $self->_construct_URL($region);

  #Now contruct the label
  $self->_construct_label($region);

  #Now set the colours
  $self->_set_colours($region);

}

sub _leftStyle {
  my ($self, $region) = @_;
  if($region->BioAnnotatedRegion->from != $region->start){
    #Check that the region has not moved due to overlaps
    $region->leftstyle("jagged");
  }else{
    $region->leftstyle("straight");
  }
}

sub _rightStyle {
  my ($self, $region) = @_;

  if($region->BioAnnotatedRegion->to != $region->end){
    #Check that the region has not moved due to overlaps
    $region->rightstyle("jagged");
  }else{
    $region->rightstyle("straight");
  }
}

sub _construct_URL {
  my ($self, $region) = @_;
  #$region->url($Bio::Pfam::Web::PfamWWWConfig::region_help);
}

sub _set_colours {
  my ($self, $region) = @_;
  my $colour1 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "ff66ff");
  my $colour2 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "ffffff");
  $region->colour1($colour1);
  
  $region->colour2($colour2);
}

sub _construct_label{
  my ($self, $region) = @_;
  if($region->BioAnnotatedRegion->id){
    $region->label($region->BioAnnotatedRegion->id);
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
