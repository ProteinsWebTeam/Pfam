
# $Author: jt6 $

package Bio::Pfam::Drawing::Image::Graph;

use strict;
use warnings;
use XML::LibXML;
use XML::LibXML::XPathContext;
use Digest::MD5 qw(md5_hex);
use GD;
use GD::Graph::lines;
use GD::Graph::bars;
use GD::Graph::hbars;
use GD::Graph::linespoints;
use GD::Graph::area;
use GD::Graph::mixed;
use GD::Graph::colour;
#use Sanger::Graphics::ColourMap;
GD::Graph::colour::add_colour("offblack1" => [1,1,1]);
GD::Graph::colour::add_colour("offblack2" => [2,2,2]);

my $ns = "http://www.sanger.ac.uk/Software/Pfam/xml/pfamDomainGraphics.xsd";


sub new {
  my $class = shift;
  my $params = shift;
  my $self = bless {}, ref($class) || $class;
  $self->{'height'} = undef;
  $self->{'length'} = undef;
  $self->{'type'} = undef;
  $self->{'y_pos'}  = undef;
  $self->{'graph_name'} = undef;
  $self->{'graph_map'} = undef;
  $self->{'graph_info'} = undef;
  $self->{'graph_obj'} = undef;
  $self->{'format'} = undef;
  $self->{'bg_colour'} = undef;
  $self->{'labelclr'} = "offblack2";
  $self->{'axislabelclr'} = "offblack2";
  $self->{'textclr'} = "offblack2";
  $self->{'allocated_colours'} = {};
  $self->{'axis_colour'} = "offblack1";
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


  $self->{timeStamp} = $params->{timeStamp}
    if defined $params->{timeStamp};
  return $self;
}


sub height {
  my($self, $height) = @_;
  if($height){
    $self->{'height'} = $height;
  }
  return $self->{'height'};
}

sub length{
  my($self, $length) = @_;
  if($length){
    $self->{'length'} = $length;
  }
  return $self->{'length'};
}

sub type {


}


sub format {


}

sub bg_colour{

}

sub graphTitle {
   my ($self,$title) = @_;
  if($title){
    $self->{'title'} = $title;
  }
  return $self->{'title'};

}

sub graphType {
  my ($self,$type) = @_;
  if($type){
    $self->{'type'} = $type;
  }
  return $self->{'type'};
}

sub yAxisLabel {
  my ($self,$label) = @_;
  if($label){
    $self->{'y_label'} = $label;
  }
  return $self->{'y_label'};
}

sub xAxisLabel {
  my ($self,$label) = @_;
  if($label){
    $self->{'x_label'} = $label;
  }
  return $self->{'x_label'};
}

sub axisColour {
  my ($self,$colour) = @_;
  if($colour){

    $self->{'axis_colour'} = $colour;
  }
  return $self->{'axis_colour'};
}


sub graphImage {

  my ($self, $image) = @_;
  if($image) {
    $self->{'graph_image'} = $image;
  }
  return $self->{'graph_image'};

}

sub create_graph{
  my ($self, $graphDom) = @_;
  my $xc = XML::LibXML::XPathContext->new( $graphDom );
  $xc->registerNs( "pf" => $ns );
  #First get the graph type and get the corresponding GD::graph object
  $self->graphType($graphDom->getAttribute("graphType"));
  $self->length($graphDom->getAttribute("length")+45);
  $self->height($graphDom->getAttribute("height"));
  $self->graphTitle($graphDom->getAttribute("title"));
  my $legend = $graphDom->getAttribute("showLegend");
  my $module = "GD::Graph::".$self->graphType;
  my $graph = $module->new($self->length, $self->height);

  my $yAxis = $xc->findnodes("pf:axisY")->shift;
  $self->yAxisLabel($yAxis->getAttribute("label"));
  my $xAxis = $xc->findnodes("pf:axisX")->shift;
  $self->xAxisLabel($xAxis->getAttribute("label"));



  #Now get the data from the DOM
  my $dataIndex;
  my @data;
  my @dataSeriesColour;
  my @dataSeriesLabel;
  my @type;
  foreach my $dataSeries ($xc->findnodes( "pf:graphData/pf:dataSeries" )){
    $xc = XML::LibXML::XPathContext->new( $dataSeries );
    $xc->registerNs( "pf" => $ns );
    if($dataSeries->hasAttribute("colour")){
      $dataSeriesColour[$self->{'dataSeries'}-1] = $dataSeries->getAttribute("colour")
    }else{
      $dataSeriesColour[$self->{'dataSeries'}-1] = "black"
    }
    
    #See if the series has a name?
    if($dataSeries->hasAttribute("label")){
      $dataSeriesLabel[$self->{'dataSeries'}-1] = $dataSeries->getAttribute("label");
    }else{
      $dataSeriesLabel[$self->{'dataSeries'}-1] = "undef";
    }

    if($dataSeries->hasAttribute("type")){
      $type[$self->{'dataSeries'}-1] = $dataSeries->getAttribute("type")
    }else{
      $type[$self->{'dataSeries'}-1] = "area"
    }
    
    foreach my $data ($xc->findnodes( "pf:data" )){
      my $x = $data->getAttribute("x");
      my $y = $data->getAttribute("y");
      push(@{$data[0]}, $x) if ($self->{'dataSeries'} == 1);
      push(@{$data[$self->{'dataSeries'}]}, $y);
    }
    $self->{'dataSeries'}++;
  }


  $graph->set("correct_width"     => 1,
	      "x_label"           => $self->xAxisLabel,
	      "y_label"           => $self->yAxisLabel,
              #"title"             => $self->graphTitle,
	      "dclrs"             => \@dataSeriesColour,
	      "types"             => \@type,
	      "x_label_skip"      => 100,
	      "box_axis"          => 0,
	      "x_label_position"  => 0.5,
	      "axislabelclr"      => "black",
	      fgclr               => $self->{'axis_colour'},
	      labelclr            => $self->{'labelclr'},
	      axislabelclr        => $self->{'axislabelclr'},
	      #textclr             => $self->{'textclr'},
	      #$self->{'colourMap'} = Sanger::Graphics::ColourMap->new();
	      line_width          => 1 ) or die $graph->error;

  if($legend){
    $graph->set_legend(@dataSeriesLabel);
  }
  #Now we have the data, lets plot it
  my $gd = $graph->plot(\@data) or die $graph->error;
  #Finally make an image of the data.
  $self->graphImage($gd);
  $self->workOutOffset;
}

sub print_image{
  my $self = shift;
  my $file = "simap.png";
  
  my $dirName = ( $self->{timeStamp} ) ? $self->{timeStamp} : $$;
  my $root;
  if($ENV{PFAM_DOMAIN_IMAGES}) {
    $root = "$ENV{'PFAM_DOMAIN_IMAGES'}";
    ($root)  = $root =~ m|([a-z0-9_\./]+)|i;
  } elsif( $ENV{DOCUMENT_ROOT} ) {
    $root = "$ENV{'DOCUMENT_ROOT'}/tmp/pfam";
    ($root)  = $root =~ m|([a-z0-9_\./]+)|i;
  }else{
    die "Do not know where to print images to: Please set the environment variable PFAM_DOMAIN_IMAGES\n"; 
  }
  my $file_location = "domain_gfx/$dirName";
  if(!-d "$root/$file_location"){
      mkdir("$root/$file_location") || die "Could not mkdir $root/$file_location:[$!]";
  }
  
  $file_location .= "/graphs";
  if(!-d "$root/$file_location" ){
    mkdir( "$root/$file_location") || die "Could not mkdir $root/$file_location:[$!]";
  }
  open(OUTFILE, ">$root/$file_location/$file") or warn "Cannot print $root/$file_location/$file:[$!]\n";

  binmode OUTFILE;
  # Convert the image to PNG and print it on standard output
  print OUTFILE $self->graphImage->png;
  close(OUTFILE) or warn "Cannot close $root/$file_location/$file :[$!]";
  $self->file_location("$file_location/$file");
}

=head2 file_location

   Title   :file_location
   Usage   :$self->file_location("mypathtofile/image.format");
   Function:Sets the location where the file is printed out.  
           :This is set by the print file sub.
   Args    :The file_location

=cut

sub file_location {
  my ($self, $value) = @_;
  if($value){
      $self->{'file_location'} = $value;
  }
  return($self->{'file_location'});
}


sub workOutOffset {
  my $self = shift;
  my ($axisR,$axisG,$axisB) = GD::Graph::colour::_rgb($self->axisColour);

  my $y = 30;
  my $x = 0;
  while($x < $self->length){
    my $index = $self->graphImage->getPixel($x,$y);
    my ($r,$g,$b) = $self->graphImage->rgb($index);
    if($r == $axisR && $b == $axisB && $g == $axisG){
      $self->{'offset'} = $x;
      last;
    }
    $x++;
  }
}

sub offSet {
  my $self = shift;
  return $self->{'offset'};
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

