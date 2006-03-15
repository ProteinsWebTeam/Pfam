
package Bio::Pfam::Drawing::Layout::Config::PfamaConfig;
use strict;
use vars qw($AUTOLOAD @ISA $VERSION);

use Bio::Pfam::Drawing::Layout::Region;
use Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig;

@ISA= qw(Bio::Pfam::Drawing::Layout::Config::GenericRegionConfig);

my (@R, @G, @B);

# Color table 

 $R[0]=0;
 $G[0]=3;
 $B[0]=192;

 $R[1]=31;
 $G[1]=192;
 $B[1]=31;

 $R[2]=192; 
 $G[2]=15;
 $B[2]=15;

 $R[3]=189;
 $G[3]=192;
 $B[3]=0;

 $R[4]=192;
 $G[4]=8;
 $B[4]=174;

 $R[5]=0;
 $G[5]=186;
 $B[5]=192;

 $R[6]=132;
 $G[6]=132;
 $B[6]=192;

 $R[7]=147;
 $G[7]=192;
 $B[7]=144;

 $R[8]=192;
 $G[8]=175;
 $B[8]=146;

 $R[9]=133;
 $G[9]=133;
 $B[9]=230;

 $R[10]=142;
 $G[10]=37;
 $B[10]=17;

 $R[11]=242;
 $G[11]=146;
 $B[11]=66;

 $R[12]=0;
 $G[12]=135;
 $B[12]=0;

 $R[13]=255;
 $G[13]=135;
 $B[13]=250;

 $R[14]=235;
 $G[14]=235;
 $B[14]=48;

 $R[15]=0;
 $G[15]=100;
 $B[15]=244;

 $R[16]=69;
 $G[16]=69;
 $B[16]=69;

 $R[17]=255;
 $G[17]=135;
 $B[17]=164;

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
  if($region->BioAnnotatedRegion->region eq "Motif" || $region->BioAnnotatedRegion->region eq "Repeat"){
    $region->leftstyle("straight");
  }elsif($region->BioAnnotatedRegion->model_from != 1){
    #Check that the region has not moved due to overlaps
    $region->leftstyle("jagged");
  }else{
    $region->leftstyle("curved");
  }
}

sub _rightStyle {
  my ($self, $region) = @_;
  if($region->BioAnnotatedRegion->region eq "Motif" || $region->BioAnnotatedRegion->region eq "Repeat"){
    $region->rightstyle("straight");
  }elsif($region->BioAnnotatedRegion->model_to != $region->BioAnnotatedRegion->model_length ){
    #Thus is must be a fragment match as it does not exist the model at the last position
    $region->rightstyle("jagged");
  }else{
    $region->rightstyle("curved");
  }
}

sub _construct_URL {
  my ($self, $region) = @_;
  #This should be defined by some
  $region->url($Bio::Pfam::Web::PfamWWWConfig::cgibin."/getacc?".$region->BioAnnotatedRegion->accession);
}


sub _construct_label{
  my ($self, $region) = @_;
  $region->label($region->BioAnnotatedRegion->id);
}


sub resolve_internal_overlaps {
  my ($self, $region1, $region2) = @_;

}

sub _set_colours {
  my ($self, $region) = @_;
  my $dc = $self->_domain_colour($region->BioAnnotatedRegion->accession);
  my $colour1 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $dc->{'colour1'});
  $region->colour1($colour1);
  my $colour2 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $dc->{'colour2'});
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
1;
