
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::CathConfig;

use vars qw($AUTOLOAD @ISA $VERSION);
use strict;
use warnings;


use Bio::Pfam::Drawing::Layout::Region;
use Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;

@ISA= qw(Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig);


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
  }elsif($region->BioAnnotatedRegion->start_frag){
    $region->leftstyle("jagged");
  }else{
    $region->leftstyle("curved");
  }
}

sub _rightStyle {
  my ($self, $region) = @_;

  if($region->BioAnnotatedRegion->to != $region->end){
    #Check that the region has not moved due to overlaps
    $region->rightstyle("jagged");
  }elsif($region->BioAnnotatedRegion->end_frag){
    $region->rightstyle("jagged");
  }else{
    $region->rightstyle("curved");
  }
}

sub _construct_URL {
  my ($self, $region) = @_;
  #This should be defined by some
  my $id = $region->BioAnnotatedRegion->id;
  $region->url("http://cathwww.biochem.ucl.ac.uk/cgi-bin/cath/GotoCath.pl?cath=$id");
}

sub _construct_label{
  my ($self, $region) = @_;
  $region->label($region->BioAnnotatedRegion->id);
}

sub _set_colours {
  my ($self, $region) = @_;
  my $colour1 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "CD679A");
  $region->colour1($colour1);
  my $colour2 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "ffc1f4");
  $region->colour2($colour2);
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
