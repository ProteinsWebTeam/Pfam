
# $Author: jt6 $


package Bio::Pfam::Drawing::Layout::Config::Coiled_coilConfig;

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
  #$region->url($Bio::Pfam::PfamWWWConfig::region_help);
}

sub _construct_label{
  my ($self, $region) = @_;
  $region->label("Coiled Coil");
}

sub _set_colours{
  my ($self, $region) = @_;
  my $colour1 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "9CFF00");
  $region->colour1($colour1);
}
