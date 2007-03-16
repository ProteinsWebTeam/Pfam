
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::MeropsConfig;
use strict;
use warnings;

use vars qw($AUTOLOAD @ISA $VERSION);

use Bio::Pfam::Drawing::Layout::Region;
use Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;

@ISA= qw(Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig);

my (@R, @G, @B);


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

  #Now assign the colours
  $self->_set_colours($region);
  
}

sub _leftStyle {
  my ($self, $region) = @_;

  if($region->BioAnnotatedRegion->njag) {
      $region->leftstyle("jagged");
  }
  elsif($region->BioAnnotatedRegion->from != $region->start ){
    #Check that the region has not moved due to overlaps
    $region->leftstyle("jagged");
  }else{
    $region->leftstyle("curved");
  }
}

sub _rightStyle {
  my ($self, $region) = @_;
  
  if($region->BioAnnotatedRegion->cjag) {
      $region->rightstyle("jagged");
  }
  elsif($region->BioAnnotatedRegion->from != $region->start ){
    #Thus is must be a fragment match as it does not exist the model at the last position
    $region->rightstyle("jagged");
  }else{
    $region->rightstyle("curved");
  }
}

sub _construct_URL {
  my ($self, $region) = @_;
  #This should be defined by some
  #$region->url("/cgi-bin/pepsum?mid=".$region->BioAnnotatedRegion->id);
  my $pcode;
  ($pcode = $region->BioAnnotatedRegion->id) =~ s/\./p/;
  $pcode = lc($pcode);
  $region->url("/pepcards/$pcode.htm");
  #my $pcode;
  #($pcode = $region->BioAnnotatedRegion->id) =~ s/\./p/;
  #$region->url("/pepcards/$pcode.htm target='main'");
}


sub _construct_label{
  my ($self, $region) = @_;
  $region->label($region->BioAnnotatedRegion->id);
}



sub _set_colours {
  my ($self, $region) = @_;
  #my $dc = $self->_domain_colour($region->BioAnnotatedRegion->accession);
  my ($c1, $c2);
  if($region->BioAnnotatedRegion->id =~ /^I\d{2}\.\w{3}$/) {
    #($c1, $c2) = ('#993300', '#663300'); #inhibitor domains brown
    ($c1, $c2) = ('#006666', '#40E0D0'); #inhibitor domains inhib green
  }
  else {
   # ($c1, $c2) = ('#33FF33', '#00AF33'); #peptidase domains green
    ($c1, $c2) = ('#008000', '#7FFF00'); #peptidase domains green
  }
  my $colour1 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $c1);
  $region->colour1($colour1);
  my $colour2 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $c2);
  $region->colour2($colour2);
}


sub _domain_colour{
  my ($self, $acc) = @_;
  #Have we seen this domain before? If not set the colour
  if(!$self->_assigned_colours($acc)){
    #Okay, for this layout, the config has not seen this domain before
    my ($colour1, $colour2) = $self->_next_colour;
    $self->_assigned_colours($acc, $colour1, $colour2);
  }
  return $self->_assigned_colours($acc);
}

sub _assigned_colours {
  my ($self, $acc, $colour1, $colour2) = @_;
  if($colour1 && $colour2){
    $self->{'colours'}->{$acc}->{'colour1'} = $colour1;
    $self->{'colours'}->{$acc}->{'colour2'} = $colour2;
  }else{
    if($self->{'colours'}->{$acc}){
      return $self->{'colours'}->{$acc};
    }else{
      return 0;
    }
  }
}

sub _next_colour{
  my $self = shift;
  my $n = $self->{'colour_no'};
  my ($i1, $i2, $dark, $light);

  if ($n<18) {
    $i1=$n;  #Same color
    $i2=$n;
  }

  if ($n>17 && $n<99) {
    $i1 = $n % 9;     #All combinations of first nine colors on upper stripe
    $i2 = 7+($n / 9); #and last nine colors on lower stripe.
  }
  
  if ($n>98 && $n<181) {
    $i1 = ($n / 9)-2; #All combinations of last nine colors on upper stripe
    $i2 = $n % 9;     #and first nine colors on lower stripe.
  }
    
  if ($n>181) {  ## Lots of images in a page then just mix & match
    $i1 = $n  % 18;
    $i2 = $n  % 18;
  }	
  
  my @tmp_dark = (&control_limits($R[$i2]) , &control_limits($G[$i2]) , &control_limits($B[$i2]) );
  $dark = sprintf("%02x%02x%02x", @tmp_dark);
  my @tmp_light =  (&control_limits($R[$i1]+40) , &control_limits($G[$i1]+40) , &control_limits($B[$i1]+40) );
  $light = sprintf("%02x%02x%02x", @tmp_light);
  $self->{'colour_no'} += 1;
  return ($dark, $light);
}

=head2 control_limits

 Title   : control_limits
 Usage   : $data=chr(&control_limits($B[$i1]+40)); 
 Function: 
    Returns the given number, if it is within the control
    limits required for GIF output, else returns the max 
    or min allowed, as applicable
 Returns : An integer
 Args    : An integer

=cut

sub control_limits {
  my $in=shift;
  my $out;
  $out=$in;
  if ($in>255) {
    $out=255;
  }
  if ($in<0) {
    $out=0;
  }
  return $out;
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

