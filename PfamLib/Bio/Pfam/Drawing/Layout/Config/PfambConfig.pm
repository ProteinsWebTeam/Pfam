
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::PfambConfig;

use strict;
use warnings;

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

=head1 

 some pod needs to go here
    

=cut


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
  #This should be defined by some

  my $url = ( defined $ENV{PFAM_FAMILY_ROOT} )
	? $ENV{PFAM_FAMILY_ROOT}."/pfamb?acc=".$region->BioAnnotatedRegion->accession
	  : "/pfamb?acc=".$region->BioAnnotatedRegion->accession;
	$region->url( $url );
}

#  $region->url($Bio::Pfam::Web::PfamWWWConfig::cgibin."/pfambget.pl?acc=".$region->BioAnnotatedRegion->accession);


sub _construct_label{
  my ($self, $region) = @_;
  $region->label($region->BioAnnotatedRegion->id);
}

sub _set_colours{
  my ($self, $region) =@_;
  my $dc = $self->_domain_colour($region->BioAnnotatedRegion->accession);
  my $colour1 = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => $dc);
  $region->colour1($colour1);

}

sub _domain_colour{
  my ($self, $acc) = @_;
  #Have we seen this domain before? If not set the colour
  if(!$self->_assigned_colours($acc)){
    #Okay, for this layout, the config has not seen this domain before
    my ($colour1) = $self->_next_colour;
    $self->_assigned_colours($acc, $colour1);
  }
  return $self->_assigned_colours($acc);
}

sub _assigned_colours {
  my ($self, $acc, $colour1) = @_;
  if($colour1){
    $self->{'colours'}->{$acc} = $colour1;
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


    #Based on the index, we need to generate a PfamB stripe.  There are seven colours per stripe
    my $x=(3*$n)%18 + ($n/6)%3; # 'Window' 
    my $i1=($x)%18;
    my $i2=($x+1+$n/324)%18;
    my $i3=($x+2+$n/18)%18;
    
    
    my $seven = sprintf("%02x%02x%02x", ( &control_limits($R[$i3]-50), &control_limits($G[$i3]-50), &control_limits($B[$i3]-50) ));
    my $six   = sprintf("%02x%02x%02x", ( &control_limits($R[$i3]-20), &control_limits($G[$i3]-20), &control_limits($B[$i3]-20) ));
    my $two   = sprintf("%02x%02x%02x", ( &control_limits($R[$i1]+20), &control_limits($G[$i1]+20), &control_limits($B[$i1]+20) ));
    my $one   = sprintf("%02x%02x%02x", ( $R[$i1], $G[$i1], $B[$i1] ));
    my $four  = sprintf("%02x%02x%02x", ( $R[$i2], $G[$i2], $B[$i2] ));
    my $three = sprintf("%02x%02x%02x", ( &control_limits($R[$i1]+40), &control_limits($G[$i1]+40), &control_limits($B[$i1]+40) ));
    my $five  = sprintf("%02x%02x%02x", ( $R[$i3], $G[$i3], $B[$i3] ));

    $self->{'colour_no'} += 1;
 
    return "$one~$two~$three~$four~$five~$six~$seven";
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

