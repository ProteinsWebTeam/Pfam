package Bio::Pfam::Drawing::Image::Image;

use strict;
use XML::LibXML;
use XML::LibXML::XPathContext;
use Digest::MD5 qw(md5_hex);
use GD;
use Sanger::Graphics::ColourMap;


my $ns = "http://www.sanger.ac.uk/Software/Pfam/xml/pfamDomainGraphics.xsd";

=head1 

   This module can roughly be divided in to three section.
   1. Object control
   2. Major drawing subs
   3. Minor drawing subs

=head2 new

  Title    :new
   ****Unfinished*****

=cut

sub new {
  my $class = shift;
  my %params = @_;
  my $self = bless {}, ref($class) || $class;
  $self->{'height'} = undef;
  $self->{'length'} = undef;
  $self->{'max_domain_height'} = 26;
  $self->{'y_pos'}  = undef;
  $self->{'image_name'} = undef;
  $self->{'image_map'} = undef;
  $self->{'image_info'} = undef;
  $self->{'image_obj'} = undef;
  $self->{'scale_x'} = undef;
  $self->{'scale_y'} = undef;
  $self->{'format'} = undef;
  $self->{'bg_colour'} = undef;
  $self->{'markup_top'} = [];
  $self->{'markup_bottom'} = [];
  $self->{'regions'} = [];
  $self->{'allocated_colours'} = {};
  $self->{'colourMap'} = Sanger::Graphics::ColourMap->new();

  return $self;
}


=head2 height

  Title   :height
  Usage   :$self->height(26);
  Function:get/sets the height of the image.  The is modified based on the number of
          :markups which are defined.  The default is 26.
  Args    :height
  returns :The height in pixels

=cut

sub height {
  my ($self, $value) = @_;
  if($value){
    $self->{'height'} = $value;
  }
  #If not set, set to the max height of the domain images (default height of 26)....
  if(!$self->{'height'}){
    $self->{'height'} = $self->_max_domain_height;
  }
  return $self->{'height'};
}

=head2

  Title   : length
  Usage   : $self->length(237)
  Function: gets/set the length of the image.  All images are drawn such that 1 pixel == 1 amino acid
          : then subsequently scaled.
  Args    : The length of the sequence/image.
  Returns : The preset length

=cut

sub length {
  my ($self, $value) = @_;
  if($value){
    $self->{'length'} = $value;
  }
  if($self->{'length'}){
    return($self->{'length'});
  }
}


=head2 y_start

   Title    :y_start 
   Usage    :$self->y_start(y_start_position)
   Function :Gets/sets the the position where the sequence images starts.  The markup
            :above the line dicates this value.
   Args     :int

=cut

sub y_start { 
  my ($self, $value) = @_;
  if($value){
    $self->{'y_start'} = $value;
  }
  if(!$self->{'y_start'}){
    #looks like there s no vertical markup
    $self->{'y_start'} = 0;
  }
  return($self->{'y_start'});
}


sub y_pos { 
  my ($self, $value) = @_;
  if($value){
    $self->{'y_pos'} = $value;
  }
  return($self->{'y_pos'});
}

=head2 scale_y

   Title   : scale_y
   Usage   : $self->scale_y($my_image_scale);
   Function: All images are drawn with a scale of 1 pixel == 1 amino acid
           : and then scaled.  Default scale is 1, i.e. no scaling factor.
           : For pfam, usually we want 1 pixel == 2 amino acids, thus we
           : use a scale factor of 0.5
   Args    : The scale factor
   Returns : The scale factor and if not set the default scale

=cut


sub scale_y {
  my ($self, $value) = @_;
  if($value){
    $self->{'scale_y'} = $value;
    $self->_max_domain_height($self->{'scale_y'});
  }
  if(!$self->{'scale_y'}){
    $self->_max_domain_height($self->{'scale_y'});
    $self->{'scale_y'} = 1.0;
  }
  
  return($self->{'scale_y'});
}

sub _max_domain_height {
  my ($self, $scale) = @_;
  #default height is 26;
  if($scale){
    $self->{'max_domain_height'} = $self->{'max_domain_height'} * $scale;
  }
  return $self->{'max_domain_height'};
} 

=head2 scale_x

   Title   : scale_x
   Usage   : $self->scale_x($my_image_scale_x);
   Function: All images are drawn with a scale of 1 pixel == 1 amino acid
           : and then scaled.  Default scale is 1, i.e. no scaling factor.
           : For pfam, usually we want 1 pixel == 2 amino acids, thus we
           : use a scale factor of 0.5
   Args    : The scale factor
   Returns : The scale factor and if not set the default scale

=cut


sub scale_x {
  my ($self, $value) = @_;
  if($value){
    $self->{'scale_x'} = $value;
  }
  if(!$self->{'scale_x'}){
    $self->{'scale_x'} = 1.0;
  }
  return($self->{'scale_x'});
}


=head2 _scale_x_coos

=cut

sub _scale_x_coos {
  my($self, $region) = @_;

  $region->setAttribute("start",($region->getAttribute("start") * $self->scale_x));
  $region->setAttribute("end",($region->getAttribute("end") * $self->scale_x));
}

=head2 format

   Title   :format
   Usage   :$self->format("png");
   Function:Sets the format of the final image.  Recognised formats
           :are png,gif,jpg
           :The default format is png.  If an unrecognised format is supplied,
           :then it is set to the default.
   Args    :The

=cut

sub format {
  my ($self, $value) = @_;
  if($value){
    if($value =~ /png|gif|jpg/i){
      $self->{'format'} = $value;
    }else{
      warn "Unrecognised format, setting to png\n";
      $self->{'format'} = "png";
    }
  }
  if(!$self->{'format'}){
    return($self->{'format'});
  }
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

=head2 image

  Title   : image
  Usage   : $self->image($GDimage);
  Function: This get/sets the GD image in this object.  This means that the
          : image can be easily passed arround.
  Args    : a GD image object

=cut

sub image {
  my ($self, $value) = @_;
  if($value){
    $self->{'image_obj'} = $value;
  }
  return($self->{'image_obj'});
}

=head2 image_info

  Title   : image_info
  Usage   : $self->image_info($image_data);
  Function: This contians any aditional display info. Bit of a hack, but is speeds things up.
  Args    : string

=cut

sub image_info {
  my ($self, $value) = @_;
  if($value){
    $self->{'image_info'} = $value;
  }
  return($self->{'image_info'});
}


=head2 bg_colour

   Title   : bg_colour
   Usage   : $self->bg_colour($colour_dom);
   Function: The first colour allocated to GD image is the background colour.
           : This is contains the xml bg colour which needs to be set. If this
           : is not set, then the default is 255,255,255 (white)
   Args    : A colour dom object.
   Returns : A hash containing the rgb value.
   ***** Unfinished *****


=cut

sub bg_colour {
  my ($self, $colour) = @_;
  if($colour){
    $self->{'bg_colour'} = $self->_get_colour_as_RGB($colour);
  }
  if(!$self->{'bg_colour'}){
    $self->{'bg_colour'} = {'R'=>255,'G'=>255,'B'=>255};
  }
  return $self->{'bg_colour'};
}

=head2 image_name

  Title    : image_name
  Usage    : $self->image_name
  Function : Gets/sets the "name" of the image.  This will normally by the sequence-id+process_id;
  Args     : A name (optional)

=cut

sub image_name {
  my ($self, $value) = @_;
  if($value){
    $self->{'image_name'} = $value;
  }
  if(!$self->{'image_name'}){
    $self->{'image_name'} = $$;
  }
  return($self->{'image_name'});
}

=head2 image_map

   Title    : image_map
   Usage    : $self->image_map("html string");
   Function : This get/sets the image map.  
   Args     : a piece of an image map.

=cut

sub image_map {
  my ($self, $value) = @_;
  if($value){
    $self->{'image_map'} .= $value;
  }
  if($self->{'image_map'}){
    return($self->{'image_map'});
  }
}

=head2 _add_colour

   Title    :_add_colour
   Usage    :$self->_add_colour($GDcolour, $colourstring)
   Function :Once a colour has been allocated, we do not need to keep
            :allocating.  Thus, we need to store the colour.  The storage
            :is performed by this subroutine, where it takes a GD colour and
            :adds to the allocated colours hash.  The key for the hash is the
            :rgb string joined with ~.
   Args     :A GD colour and the colour string (see above).

=cut

sub _add_colour {
  my ($self, $col, $key) = @_;
  if($col && $key){
    $self->{'allocated_colours'}->{$key} = $col;
  }
}

=head2 get_colour

   Title    :get_colour
   Usage    :$self->get_colour(%RGB);
   Function :This takes the a hash containing RGB values and allocated the
            :the colour in the image
   Args     :hash containing the RGB values
   Returns  :gd colour object.

=cut

sub get_colour{
  my $self = shift;
  my %colour_hash = @_;

  if(!$self->{'allocated_colours'}->{$colour_hash{R}."~".$colour_hash{G}."~".$colour_hash{B}}){
    my $col = $self->image->colorAllocate($colour_hash{R}, $colour_hash{G}, $colour_hash{B});
    $self->_add_colour($col, $colour_hash{R}."~".$colour_hash{G}."~".$colour_hash{B});
  } 
  return $self->{'allocated_colours'}->{$colour_hash{R}."~".$colour_hash{G}."~".$colour_hash{B}};
}

=head2 add_top_markup

  Title    :add_top_markup
  Usage    :$self->add_top_markup($markup_dom);
  Function :Add a "top" markup
  Args     :A markup dom object
  Returns  :Nothing

=cut

sub add_top_markup{
  my ($self, $markup) = @_;
  if($markup){
    $markup->setAttribute("start", $markup->getAttribute("start")*$self->scale_x) if($markup->getAttribute("start"));
    $markup->setAttribute("end", $markup->getAttribute("end")*$self->scale_x) if($markup->getAttribute("end"));
    push(@{$self->{'markup_top'}}, $markup);
  }
}

=head2 each_top_markup

  Title    :each_top_markup
  Usage    :$self->each_top_markup();
  Function :gets an array of "top" markups
  Args     :Nothing
  Returns  :array of markup XML::DOM objects

=cut

sub each_top_markup{
  my $self = shift;
  return @{$self->{'markup_top'}};
}

=head2 add_bottom_markup

  Title    :add_bottom_markup
  Usage    :$self->add_bottom_markup($markup_dom);
  Function :Add a "bottom" markup
  Args     :A markup dom object
  Returns  :Nothing

=cut

sub add_bottom_markup{
  my ($self, $markup) = @_;
  if($markup){
     $markup->setAttribute("start", $markup->getAttribute("start")*$self->scale_x) if($markup->getAttribute("start"));
    $markup->setAttribute("end", $markup->getAttribute("end")*$self->scale_x) if($markup->getAttribute("end"));
    push(@{$self->{'markup_bottom'}}, $markup);
  }
}

=head2 each_bottom_markup

  Title    :each_bottom_markup
  Usage    :$self->each_bottom_markup;
  Function :Get the bottom markups
  Args     :A markup dom object
  Returns  :Nothing

=cut

sub each_bottom_markup{
  my $self = shift;
  return @{$self->{'markup_bottom'}};
}

=head2 sort_and_resolve_markups

    *****Unfinished*****

=cut

sub sort_and_resolve_markups {
  my $self = shift;
  my @elements = @_;
  
  my (@top_lolly, @top_bridge, @bottom_lolly, @bottom_bridge);
  # sort according to position & style;
  foreach my $markup_dom (@elements) {
      
    if($markup_dom->getAttribute("v_align") =~ /top/i){
      if($markup_dom->getAttribute("end")){
	#must be a bridge;
	  
	push(@top_bridge, $markup_dom);
      }else{
	#must be a lolly pop;
	  
	  $markup_dom->setAttribute("offset", 0);
	push(@top_lolly, $markup_dom);
      }
    }else{
      if($markup_dom->getAttribute("end")){
	#must be a bridge;
	  push(@bottom_bridge, $markup_dom);
      }else{
	#must be a lolly pop;
	push(@bottom_lolly, $markup_dom);
      }
    }
  }
  
  @top_bridge = sort{$a->getAttribute("start") <=> $b->getAttribute("start")}@top_bridge;
  @top_lolly = sort{$a->getAttribute("start") <=> $b->getAttribute("start")}@top_lolly; 
  @bottom_bridge = sort{$a->getAttribute("start") <=> $b->getAttribute("start")}@bottom_bridge; 
  @bottom_lolly = sort{$a->getAttribute("start") <=> $b->getAttribute("start")}@bottom_lolly;

  #All bridges must not go through the lolly pops! We can add the lolly pops now, as these should not clash
  
  for(my $i = 0; $i <= $#top_bridge; $i++){
    my $markup = $top_bridge[$i];
    my $current_offset =0;
    foreach (@top_lolly){
	
      if($markup->getAttribute("start")<=$_->getAttribute("start") && $markup->getAttribute("end")>=$_->getAttribute("start")){
	  
	  $current_offset++;
	last;
      }
    }
    #need to make sure that these do not clash with other bridges
    for(my $j = 0; $j < $i;){
      if($markup->getAttribute("start")<=$top_bridge[$j]->getAttribute("end") && 
	 $markup->getAttribute("end")>=$top_bridge[$j]->getAttribute("start")){
	#overlaps in x;
	if($top_bridge[$j]->getAttribute("offset") == $current_offset){
	  $j = 0;
	  $current_offset++;
	}else{
	  $j++;
	}
      }else{
	$j++;
      }
    }
    $top_bridge[$i]->setAttribute("offset", $current_offset);
    
}
  
  # Now do the bottom
  for(my $i = 0; $i <= $#bottom_bridge; $i++){
    my $markup = $bottom_bridge[$i];
    my $current_offset =0;
    foreach (@bottom_lolly){
      if($markup->getAttribute("start")<=$_->getAttribute("start") && $markup->getAttribute("end")>=$_->getAttribute("start")){
	$current_offset++;
	last;
      }
    }
    #need to make sure that these do not clash with other bridges
    for(my $j = 0; $j < $i;){
      if($markup->getAttribute("start")<=$bottom_bridge[$j]->getAttribute("end") && 
	 $markup->getAttribute("end")>=$bottom_bridge[$j]->getAttribute("start")){
	#overlaps in x;
	if($bottom_bridge[$j]->getAttribute("offset") == $current_offset){
	  $j = 0;
	  $current_offset++;
	}else{
	  $j++;
	}
      }else{
	$j++;
      }
    }
    $bottom_bridge[$i]->setAttribute("offset", $current_offset);
  }

  #Need to get MAX offset for each to that the image height can be set....Also,so that the markups
  #can be added to the ogject.
  my ($max_top_offset, $max_bottom_offset);
  $max_top_offset = 0;
  $max_bottom_offset = 0;
  
  foreach (@top_bridge){
    $self->add_top_markup($_);
    if($max_top_offset < $_->getAttribute("offset")){
	$max_top_offset = $_->getAttribute("offset");
	
    }
  }
  
  foreach (@bottom_bridge){
    $self->add_bottom_markup($_);
    if($max_bottom_offset < $_->getAttribute("offset")){
      $max_bottom_offset = $_->getAttribute("offset");
    }
  }
  foreach (@top_lolly){
    $self->add_top_markup($_);
  }

  foreach (@bottom_lolly){
    $self->add_bottom_markup($_);
  }

 
  my $top = 0;
  if($self->each_top_markup){
    $top = $max_top_offset * 3 + 20;
  }
  my $bottom = 0;
  if($self->each_bottom_markup){
    $bottom = $max_bottom_offset * 3 + 20;
  }
  my $height = $self->height;

  $height += $top;
  $height += $bottom;
  $self->height($height);
  $self->y_start($top);
}

=head2 _add_map

   Title    : _add_map
   Usage    : $self->_add_map($region);
   Function : Builds up the area html so that an image map can be produced.
            : As with the image, this will orginally be produce with 1 pixel
            : to 1 amino acid.  It will be scaled subsequently.
   Args     : region XML::DOM object

****Affect by change in scale, FIX *****

=cut

sub _add_map{
  my ($self, $region) = @_;
  
  if($region){
  
    my $area = qq(<area shape=\"rect\" coords=\");
    #Okay, I am going to assume that there is an start and end coos. Write the area.
    $area .= $region->getAttribute("start").", ".$self->y_start.", ".$region->getAttribute("end").", ".($self->y_start+$self->_max_domain_height);
    #Okay, now add the href
	if( defined $region->getAttribute("link_URL" ) ) {
	  $area .= "\" href=\"".$region->getAttribute("link_URL");
	} else {
	  $area .= "\" nohref=\"". $region->getAttribute("label")."\"";
	}
    #Alternative
    $area .= "\" title=\"";
    $area .= $region->getAttribute("label").":" if($region->getAttribute("label"));
    $area .=  $region->getAttribute("start")/$self->scale_x."-".$region->getAttribute("end")/$self->scale_x."\" />";

	

    $self->image_map($area);
  }	       
}

=head2 _add_regions

   Title    : _add_regions
   Usage    : $self->_add_regions(@arrayofregiondoms);
   Function : Allows the image object to store the region XML::DOM objects.
            : The removes the need of passing them around in the code.
   Args     : Array of region XML::DOM objects.

=cut

sub _add_regions{
  my ($self, @regions) = @_;
  if(@regions){
    foreach my $region (@regions){
      $self->_scale_x_coos($region);
    }
    $self->{'regions'} =  \@regions;
  }
}

=head2 _get_colour_as_RGB

   Title    : _get_colour_as_RGB
   Usage    : $self->_get_colour_as_RGB
   Function : This takes a colour XML::DOM (either RGB or HEX) and returns
            : a hash where the keys are R,G,B.
   Args     : A colour XML::DOM object
   Returns  : A hash containing the RGB

=cut 

sub _get_colour_as_RGB {
    my($self, $colour_dom) = @_;

    my %rgb;
    my @stripe_colours;
    if($colour_dom) {
	my $colNode;
	my $xc = XML::LibXML::XPathContext->new( $colour_dom );
	  $xc->registerNs( "pf" => $ns );
	  if( $colNode = $xc->findnodes( "pf:hex" )->shift ) {
	    #foreach my $hexnode ($colour_dom->getChildNodes){
	    my $hexcode = $colNode->getAttribute("hexcode");
		#Pfam B
		if($hexcode =~ /\~/){
		  #looks like we have a pfamB hack!
		  my @colours = split(/~/, $hexcode);
		  my $c1 = shift @colours;
		  my ($r, $g, $b) =$self->{colourMap}->rgb_by_hex($hexcode);
		  $rgb{R}=$r;
		  $rgb{G}=$g;
		  $rgb{B}=$b;
		  foreach (@colours){
			($r, $g, $b) = $self->{colourMap}->rgb_by_hex($_);
			my %rgb1;
			$rgb1{R}=$r;
			$rgb1{G}=$g;
			$rgb1{B}=$b;
			push(@stripe_colours, \%rgb1);
		  }
		}
		
		my ($r, $g, $b) =$self->{colourMap}->rgb_by_hex($hexcode);
		$rgb{R}=$r;
		$rgb{G}=$g;
		$rgb{B}=$b;
		#last;
	    #}
	  }elsif( $colNode = $xc->findnodes( "pf:RGB" )->shift ){
	    $rgb{R}=$colNode->getAttribute("R");
		$rgb{G}=$colNode->getAttribute("G");
		$rgb{B}=$colNode->getAttribute("B");
	  }
	}
    if(@stripe_colours){
	  return(\@stripe_colours, %rgb);
    }else{
	  return(%rgb);
    }
}

##########################################
# Below are the image generation methods #
##########################################

=head2 create_image

    Tilte   :create_image
    Usage   :$self->create_image($seq_dom_object, $stored_images_ref);
    Function:Creates an image of the sequence domain organisation based on the
             sequence domain object.  The format and scale are part of the 
             image object. As a speed up, previous image fragments are stored
             in a hash reference.  See below subroutines for more information about 
             how the images are drawn.

             Simply, a blank image is generated, next the sequence is drawn on it,
             then the "regions".  Subsequently, any markup is drawn.  Finally, the image
             is scaled (note, everything is drawn with scale 1) and printed out.

    Returns : Nothing
    Args    : A sequence dom object (produced from the XML), a reference to a
            : hash that stores any previous images.

=cut

sub create_image {
  my ($self, $seq_dom, $stored_image_ref) = @_;

  # store a single XPathContext for this object
  my $xc = XML::LibXML::XPathContext->new( $seq_dom );
  $xc->registerNs( "pf" => $ns );

  # set the length and current y_pos;
  $self->length($seq_dom->getAttribute("length")*$self->scale_x);
  
  $self->y_pos(0);

  $self->image_name($seq_dom->getAttribute("name"));
  $self->image_info($seq_dom->getAttribute("display_data"));
  #this will set the height
  $self->sort_and_resolve_markups($xc->findnodes("pf:markup"));

  #This will sort the regions out, which is important for later

  my @regs = $xc->findnodes( "pf:region" );
  

  $self->_add_regions(@regs);

  $self->_new_image;

  #Draw each section, adding any new images to the store images hash
  $self->_draw_sequence();
  $self->_draw_regions($stored_image_ref);
  $self->_draw_top_markup();
  $self->_draw_bottom_markup();
}


=head2 _draw_sequence

   Title    : _draw_sequence
   Usage    : $self->_draw_sequence
   Function : draws a representation of the sequence
   Args     : Nothing

=cut

sub _draw_sequence{
  my $self = shift;
  #get the grey colour
  my %rgb = ( 'R' => 192,
	      'G' => 192,
	      'B' => 192);
  my $grey = $self->get_colour(%rgb);
  #get the dark grey colour
  %rgb = ( 'R' => 128,
	   'G' => 128,
	   'B' => 128);
  
  my $dark_grey = $self->get_colour(%rgb);

  #now draw the entire sequence. This is comprised of two stripes, a thick grey stripe,
  #followed by a thinner dark grey stripe.
  my $dy = $self->scale_y;
  my $seq_y_start = int(($self->_max_domain_height/2) - $dy);
  $self->image->filledRectangle(1, $seq_y_start + $self->y_start, $self->length, $seq_y_start + $self->y_start + 3*$dy , $grey);
  $self->image->filledRectangle(1, $seq_y_start + $self->y_start + 3*$dy, $self->length, $seq_y_start + $self->y_start + (4*$dy), $dark_grey);
}

=head2 _draw_regions

   Title    :_draw_regions
   Usage    :$self->_draw_regions();
   Function :This takes a sequence dom object....
   Args     :

=cut

sub _draw_regions{
  my $self = shift;
  my $stored_regions_ref = shift;

  #
  #sizes of the different images
  #
  my %sizes = (#These deal with edge withs 
	       'jagged' => 13,
		'straight' => 2,
		'curved' => 13,
		'curvedsmall' => 3,
		'jaggedsmall' => 3,
		#These deal with heights
		'sml' => 15 * $self->scale_y,
		'med' => 18 * $self->scale_y,
		'big' => 26 * $self->scale_y, );

  my $xc = XML::LibXML::XPathContext->new;
  $xc->registerNs( "pf" => $ns );

  foreach my $reg_dom (@{ $self->{regions} } ){

	# set the context for the XML::LibXML::XPathContext
	$xc->setContextNode( $reg_dom );

    my ($region, %colour1, %colour2, $leftStyle, $rightStyle);
    #All regions has colour1 element
    %colour1 = $self->_get_colour_as_RGB( $xc->findnodes("pf:colour1/pf:colour")->shift );

    #work out what sort of region we want to draw, big, med, sml
    if($xc->find("pf:bigShape")){
      %colour2 = $self->_get_colour_as_RGB( $xc->findnodes( "pf:colour2/pf:colour" )->shift );
      $region = "big";
    } elsif ($xc->find("pf:medShape")){
      $region = "med";
    } elsif ($xc->find("pf:smlShape")){
      $region = "sml";
    } else{
      die "%%% Shape improperly defined; contact developer !";
    }

	

    #Okay, we need to build up a unique key for the hash
    my $key;
    if($colour1{R} || $colour1{G} || $colour1{B}){
	  $key = $region."~".$colour1{R}."~".$colour1{G}."~".$colour1{B};
    }else{
	  $key = $region;
    }
    #This will store the size of the left and right images
    my ($left, $straight, $right, $reverse);
    if($region eq "big"){
      my $shape = $xc->findnodes("pf:bigShape")->shift;
      #extend the hash to make unique for large images i.e. colour1 could be the same
      $key .= "~".$colour2{R}."~".$colour2{G}."~".$colour2{B};
      #Okay, is the domain is very small, we need the smaller domain edges
      
      $leftStyle = $shape->getAttribute("leftStyle");
      $rightStyle = $shape->getAttribute("rightStyle");
      
      #The middle bit and if striaght edges forms the edge
      if(!$$stored_regions_ref{$key}{'straight'}){
	($$stored_regions_ref{$key}{'straight'}, $$stored_regions_ref{$key}{'reverse'}) = $self->_straight($sizes{$region}, \%colour1, \%colour2);
      }

      $straight = $$stored_regions_ref{$key}{'straight'};

      #Make sure that we have the reverse
      if(!$$stored_regions_ref{$key}{'reverse'}){
	($$stored_regions_ref{$key}{'straight'}, $$stored_regions_ref{$key}{'reverse'}) = $self->_straight($sizes{$region}, \%colour1, \%colour2);
      }

      $reverse =  $$stored_regions_ref{$key}{'reverse'};
      
      if($leftStyle ne "straight"){
	#jagged or curved gets here
	if($reg_dom->getAttribute("end")  - $reg_dom->getAttribute("start") <= 26){
	  #This is a small domain, so we want small versions of the domain
	  $leftStyle .= "small";
	  $rightStyle .= "small";
	}
	#Okay, if the images are not in the store hash, make them. Note, the
	#style of the edge is the name of the sub.
	if(!$$stored_regions_ref{$key}{$leftStyle."left"}){
	  ($$stored_regions_ref{$key}{$leftStyle."left"}, $$stored_regions_ref{$key}{$leftStyle."right"})  = $self->$leftStyle($reg_dom, \%colour1, \%colour2, $straight, $reverse);
	}
	$left = $$stored_regions_ref{$key}{$leftStyle."left"};
      }elsif( $rightStyle eq "straight") {
	if(!$$stored_regions_ref{$key}{$leftStyle."left"}){
	  ($$stored_regions_ref{$key}{$leftStyle."left"}, $$stored_regions_ref{$key}{$leftStyle."right"})  = $self->straight($reg_dom, \%colour1, \%colour2, $reverse);
	}
	$left = $$stored_regions_ref{$key}{$leftStyle."left"};
      }else{
	$left = 0;
      }

      if($rightStyle !~ /straight/){
	#jagged or curved gets here
	
	#Okay, if the images are not in the store hash, make them. Note, the
	#style of the edge is the name of the sub.
	if(!$$stored_regions_ref{$key}{$rightStyle."right"}){
	  ($$stored_regions_ref{$key}{$rightStyle."left"}, $$stored_regions_ref{$key}{$rightStyle."right"})  = $self->$rightStyle($reg_dom, \%colour1, \%colour2, $straight, $reverse);
	}
	$right = $$stored_regions_ref{$key}{$rightStyle."right"};
      }elsif($rightStyle eq "straight"){
	if(!$$stored_regions_ref{$key}{$rightStyle."right"}){
	  ($$stored_regions_ref{$key}{$rightStyle."left"}, $$stored_regions_ref{$key}{$rightStyle."right"})  = $self->straight($reg_dom, \%colour1, \%colour2, $reverse);
	}
	$right = $$stored_regions_ref{$key}{$rightStyle."right"};
      }else{
	$right = 0;
      }
	
      
	
    }else{
	if( $reg_dom->getAttribute("label") =~ /pfam-b/i){
	    my ($stripes, %stripe1) = $self->_get_colour_as_RGB( $xc->findnodes("pf:colour1/pf:colour")->shift );
	    foreach my $colour (@{$stripes}){
		$key .= "~".$$colour{R}."~".$$colour{G}."~".$$colour{B};
	    }
	    if(!$$stored_regions_ref{$key}){
		unshift(@{$stripes}, \%stripe1);
		$straight = $self->$region($reg_dom, $sizes{$region},$stripes);
	    	$$stored_regions_ref{$key} = $straight;
	    }
	    $right = $left = 0;
	    $straight = $$stored_regions_ref{$key};
	
	    
	}else{
	    #does the hash of stored images contain the smaller images?
	    if(!$$stored_regions_ref{$key}){
		$straight = $self->$region($reg_dom, $sizes{$region}, \%colour1);
		$$stored_regions_ref{$key} = $straight;
	    }
	    $right = $left = 0;
	    $straight = $$stored_regions_ref{$key};
	
	}
    }
    $self->_combine_images($reg_dom, $region, $left, $right, $straight, $leftStyle, $rightStyle, \%sizes);
    #Now write the image map;
    if($reg_dom->getAttribute("link_URL")){
      $self->_add_map($reg_dom);
    }
  }
}

=head2 _draw_top_markup

  Title   : _draw_top_markup
  Usage   : $self->_draw_top_markup()
  Function: The markup should have already been sorted by this time.  When the
          : markup has been sorted, it is added to the this Image object.  So, here
          : we take each top markup and draw these.
  Args    : Nothing


=cut

sub _draw_top_markup{
  my ($self) = @_;

#  With a scale factor of 1
#
#        O  [__]  HEIGHT = 6
#        |  ||||  HEIGHT = 10
#        |  ||||
#      ############ $self->y_start()  |  
#     #            #                  |  
#    #              #                 | HEIGHT = max_domain_height (26)
#    #              #                 |
#     #            #                  |
#      ############                   
#       |  
#      \|/  HEIGHT=10


  #get the background colour
  my $bg_colour = $self->bg_colour;
  my $bg_gd_col = $self->get_colour(%{$bg_colour});
  
  #Do the vertical alignment
  foreach my $markup ($self->each_top_markup) {
      #Changed to use LibXML.
      my $xc = XML::LibXML::XPathContext->new( $markup );
      $xc->registerNs( "pf" => $ns );

      #get the line colour
      my $line = $xc->findnodes( "pf:line")->shift;
      my %colour = $self->_get_colour_as_RGB($xc->findnodes( "pf:line/pf:colour" )->shift);
      
      my $line_colour = $self->get_colour(%colour);
      my ($y1, $y2) = $self->_draw_vertical_line($markup->getAttribute("start"), $markup, $line, $line_colour, $bg_colour, $bg_gd_col);
      if($markup->getAttribute("end")){
	  $self->_draw_vertical_line($markup->getAttribute("end"), $markup, $line, $line_colour, $bg_colour, $bg_gd_col);
	  $self->_draw_horizontal_line($markup, $line, $y1, $line_colour, $bg_gd_col );
      }else{
	  my $head = $xc->findnodes("pf:head")->shift;
	  #assume that there is a head element!
	  $self->_draw_head($markup, $y1, $y2, $head) if($head);
      }
  }
  #Add map here
}


=head2 _draw_bottom_markup

  Title   : _draw_bottom_markup
  Usage   : $self->_draw_bottom_markup()
  Function: The markup should have already been sorted by this time.  When the
          : markup has been sorted, it is added to the this Image object.  So, here
          : we take each top markup and draw these.
  Args    : Nothing


=cut

sub _draw_bottom_markup{
  my ($self) = @_;

#  With a scale factor of 1
#
#        O  [__]  HEIGHT = 6
#        |  ||||  HEIGHT = 10
#        |  ||||
#      ############ $self->y_start()  |  
#     #            #                  |  
#    #              #                 | HEIGHT = max_domain_height (26)
#    #              #                 |
#     #            #                  |
#      ############                   
#       |  
#      \|/  HEIGHT=10


  #get the background colour
  my $bg_colour = $self->bg_colour;
  my $bg_gd_col = $self->get_colour(%{$bg_colour});

  #Do the vertical alignment
  foreach my $markup ($self->each_bottom_markup) {
      #get the line colour
      my $xc = XML::LibXML::XPathContext->new( $markup );
      $xc->registerNs( "pf" => $ns );
      
      my $line = $xc->findnodes( "pf:line")->shift;
      my %colour = $self->_get_colour_as_RGB($xc->findnodes( "pf:line/pf:colour" )->shift);
	
      my $line_colour = $self->get_colour(%colour);
    
    my ($y1, $y2) = $self->_draw_vertical_line($markup->getAttribute("start"), $markup, $line, $line_colour, $bg_colour, $bg_gd_col);
    if($markup->getAttribute("end")){
      $self->_draw_vertical_line($markup->getAttribute("end"), $markup, $line, $line_colour, $bg_colour, $bg_gd_col);
      $self->_draw_horizontal_line($markup, $y2, $line_colour, $bg_gd_col );
    }else{
      my $head = $xc->findnodes("pf:head")->shift;
      #assume that there is a head element!
      $self->_draw_head($markup, $y2, $y1, $head) if($head);
    }
  }
}


sub _draw_vertical_line {
  my ($self, $x, $markup, $line, $line_colour, $bg_colour, $bg_gd_col) = @_;
  #First part of this subroutine is to find where to start drawing the line.
  #y1 is always < y2
  my ($y1, $y2);
  if($markup->getAttribute("v_align") eq "top"){
    foreach my $p (0..$self->height()) {
      my $index = $self->image->getPixel($x,$p);
      my ($r,$g,$b) = $self->image->rgb($index);
      
      if ( ($r == $$bg_colour{R}) && ($g == $$bg_colour{G}) && ($b == $$bg_colour{B}) ) { #
	next;
      }else{
	#we have hit a non background colour
	  my $q = $p+1; #The next pixel as this could be a line
	  my $index1 = $self->image->getPixel($x,$q);
	  my ($r1,$g1,$b1) = $self->image->rgb($index1);
	  if ( ($r1 == $$bg_colour{R}) && ($g1 == $$bg_colour{G}) && ($b1 == $$bg_colour{B}) ) { #
	      next;
	  }else{
	      $y2 = $p - 1;
	      last;
	  }
      }
    }
    $y1 = $self->y_start - 10;
    
    $y1 -= $markup->getAttribute("offset")*3;
}elsif($markup->getAttribute("v_align") eq "bottom"){
    for(my $p = $self->height(); $self->y_start <= $p; $p--) {
	my $index = $self->image->getPixel($x, $p);
	my ($r,$g,$b) = $self->image->rgb($index);
	if ( ($r == $$bg_colour{R}) && ($g == $$bg_colour{G}) && ($b == $$bg_colour{B}) ) { #
	    next;
	}else{
	    #we have hit a non background colour
	    my $q = $p-1; #The next pixel as this could be a line
	    my $index1 = $self->image->getPixel($x,$q);
	    my ($r1,$g1,$b1) = $self->image->rgb($index1);
	    if ( ($r1 == $$bg_colour{R}) && ($g1 == $$bg_colour{G}) && ($b1 == $$bg_colour{B}) ) { #
		next;
	    }else{
		$y1 = $p + 1;
		$p = $self->y_start;
	    }
	}
    }
    $y2 = $self->y_start()+ $self->_max_domain_height + 10;
    $y2 += $markup->getAttribute("offset")*3;
  }
  
  
  if($line->getAttribute("style") =~ /dashed|mixed/){
      # Need to make vertical lines dashes! 2 pixels on, 2pixels off
      for(my $dash = $y1; $dash <= $y2; $dash +=4){
	  if($dash+2 >$y2){
	      $self->image->line($x, $dash, $x, $y2, $line_colour);
	  }else{
	      $self->image->line($x, $dash, $x, $dash+2, $line_colour);
	  }
      }
  }elsif($line->getAttribute("style") =~ /bold/){
      #$self->image->setThickness(2);
      $self->image->line($x, $y1, $x, $y2, $line_colour);
      #$self->image->setThickness(1);
  }else{
      $self->image->line($x, $y1, $x, $y2, $line_colour);
  }
  
  if($markup->getAttribute("label")){
      my $area = qq(<area shape=\"rect\" coords=\");
      #Okay, I am going to assume that there is an start and end coos. Write the area.
      $area .= ($x-1).", ".($y1-6).", ".($x+1).", ".($y2+6)."\" nohref=\"".$markup->getAttribute("label")."\"";
      #Alterantive
      $area .= " title=\"";
      $area .= $markup->getAttribute("label");
      $area .=  "\" />";
      $self->image_map($area);
  }

  return($y1, $y2);
}

sub _draw_horizontal_line {
    my ($self, $markup, $line, $y, $line_colour, $bg_gd_col) = @_;
    my $x1 = $markup->getAttribute("start");
    my $x2 = $markup->getAttribute("end");
    
    if($line->getAttribute("style") eq "dashed"){
	for(my $dash = $x1; $x2-2 >= $dash; $dash += 4){
	    $self->image->line($dash, $y, $dash+2, $y, $line_colour);
	}
    }else{
	$self->image->line($x1, $y, $x2, $y, $line_colour);
    }
    
   if($markup->getAttribute("label")){
      my $area = qq(<area shape=\"rect\" coords=\");
      $area .= "$x1, ".($y+1).", $x2,".($y-1)."\" nohref=\"".$markup->getAttribute("label")."\"";
      $area .= " title=\"";
      $area .= $markup->getAttribute("label");
      $area .=  "\" />";
      $self->image_map($area);
  }
}



sub _draw_head {
  my ($self, $markup, $apex, $arrow_apex, $head)=@_;
  my $start = $markup->getAttribute("start"); # This is a speed-up as we are going to it a lot
  #The head colour
  my $xc = XML::LibXML::XPathContext->new( $head );
  $xc->registerNs( "pf" => $ns );
  my %colour = $self->_get_colour_as_RGB($xc->findnodes("pf:colour"));
	
  my $h_colour = $self->get_colour(%colour);
  
  if($head->getAttribute("style") eq "arrow"){
      
      if ($markup->getAttribute("v_align")  eq "bottom"){
      ### UP ARROW
      ### left side of the arrow 
      #         ##
      #        # #
      #       #  #
      #      #   #
      $self->image->line($start - 1, $arrow_apex, $start - 1, $arrow_apex, $h_colour);
      $self->image->line($start - 2, $arrow_apex + 1, $start - 2, $arrow_apex + 1, $h_colour);
      $self->image->line($start - 3, $arrow_apex + 2, $start - 3, $arrow_apex + 2, $h_colour);
      $self->image->line($start - 4, $arrow_apex + 3, $start - 4, $arrow_apex + 3, $h_colour);
      
      ### Right side of the arrow
      #       ##   
      #       # #  
      #       #  # 
      #       #   #
      $self->image->line($start + 1, $arrow_apex, $start + 1, $arrow_apex, $h_colour);
      $self->image->line($start + 2, $arrow_apex + 1, $start + 2, $arrow_apex + 1, $h_colour);
      $self->image->line($start + 3, $arrow_apex + 2, $start + 3, $arrow_apex + 2, $h_colour);
      $self->image->line($start + 4, $arrow_apex + 3, $start + 4, $arrow_apex + 3, $h_colour);
      
    } elsif ($markup->getAttribute("v_align") eq "top") {

      # DOWN ARROW
    
      ### left side of the arrow
      #      #   #
      #       #  #
      #        # #
      #         ##

      $self->image->line($start - 1, $arrow_apex, $start - 1, $arrow_apex, $h_colour);
      $self->image->line($start - 2, $arrow_apex - 1, $start - 2, $arrow_apex - 1, $h_colour);
      $self->image->line($start - 3, $arrow_apex - 2, $start - 3, $arrow_apex - 2, $h_colour);
      $self->image->line($start - 4, $arrow_apex - 3, $start - 4, $arrow_apex - 3, $h_colour);	
      
      ## right side of the arrow
      #       #   #
      #       #  #
      #       # #
      #       ##
    
      $self->image->line($start + 1, $arrow_apex, $start + 1, $arrow_apex, $h_colour);
      $self->image->line($start + 2, $arrow_apex - 1, $start + 2, $arrow_apex - 1, $h_colour);
      $self->image->line($start + 3, $arrow_apex - 2, $start + 3, $arrow_apex - 2, $h_colour);
      $self->image->line($start + 4, $arrow_apex - 3, $start + 4, $arrow_apex - 3, $h_colour);	
    }
  }elsif($head->getAttribute("style") eq "diamond"){
    my $poly = new GD::Polygon;
    
	$apex -= 3;
    
	    
    $poly->addPt($start ,$apex ); ## top line
    $poly->addPt($start - 3, $apex +3 ); ##left
    $poly->addPt($start, $apex + 6 );  ## bottom
    $poly->addPt($start + 3, $apex + 3); ## right
    $self->image->filledPolygon($poly, $h_colour);
  }elsif($head->getAttribute("style") eq "square"){
    my $poly = new GD::Polygon;
    $apex -= 2;
    #if ($markup->getAttribute("v_align")  eq "top");
    $poly->addPt($start - 2, $apex); ## top left 
    $poly->addPt($start - 2,$apex + 4);
    $poly->addPt($start + 2, $apex + 4);
    $poly->addPt($start + 2, $apex ); ## top right 
    $self->image->filledPolygon($poly, $h_colour);
  }elsif($head->getAttribute("style") eq "circle"){
    
    #$self->image->filledEllipse($start, $apex, 3, 3, $h_colour);
    $self->image->arc($start, $apex, 5, 5, 0, 360, $h_colour);
    $self->image->fill($start, $apex, $h_colour);
}else{
    warn "Unknown shape:".$head->getAttribute("style").". Cannot draw markup head\n";
  }
}

# These are the methods called from within the main drawing sub

=head2

   Title    : _new_image
   Usage    : $self->_new_image;
   Function : This generates a new GD image object based on the contents of this object.
            : The GD object is added to this object.
   Args     : Nothing
   Returns  : Nothing

=cut

sub _new_image {
  my $self =  shift;

  
  my $im = new GD::Image($self->length, $self->height);
  

  #Add the GD image object to this object
  $self->image($im);

  my $col;
  my $bg_colour = $self->bg_colour;
  
  $col = $im->colorAllocate($$bg_colour{'R'}, $$bg_colour{'G'}, $$bg_colour{'B'});
  $self->_add_colour($col, $$bg_colour{'R'}."~".$$bg_colour{'G'}."~".$$bg_colour{'B'} );

  # make the background transparent and interlaced
  $im->transparent($col);
  $im->interlaced('true');
}

=head2 print_image

   Title    : print_image
   Usage    : $image->print_image
   Function : Prints the image to the Pfam WWW temp directory.  All images are printed as png.
            : The png file from GD is the converted to the a different format if necessary.
   Args     : None
   ***** Unfinished *****Put back pfamWWWConfig on output file

=cut

sub print_image {
  my $self = shift;
  my $file = $self->image_name.".png";
  my $pid = $$;
  my $file_location = "tmp/pfam/domain_gfx/$pid";
  if(!-d "$Bio::Pfam::Web::PfamWWWConfig::doc_root/$file_location"){
      mkdir("$Bio::Pfam::Web::PfamWWWConfig::doc_root/$file_location") || die "Could not mkdir $Bio::Pfam::Web::PfamWWWConfig::doc_root/$file_location:[$!]";
  }


  

  #make temp lib of all with unknown location
  my @md5 = md5_hex($file) =~ /\G(..)/g;
  foreach ($md5[0], $md5[1], $md5[2]){
      $file_location .= "/$_";
      if(!-d "$Bio::Pfam::Web::PfamWWWConfig::doc_root/$file_location" ){
	  mkdir( "$Bio::Pfam::Web::PfamWWWConfig::doc_root/$file_location") || die "Could not mkdir $Bio::Pfam::Web::PfamWWWConfig::doc_root/$file_location:[$!]";
      }
  }
  

  
  open(OUTFILE, ">$Bio::Pfam::Web::PfamWWWConfig::doc_root/$file_location/$file") or warn "Cannot print $Bio::Pfam::Web::PfamWWWConfig::doc_root/$file_location/$file:[$!]\n";

  binmode OUTFILE;
  # Convert the image to PNG and print it on standard output
  print OUTFILE $self->image->png;
  close(OUTFILE) or warn "Cannot close $file_location/$file :[$!]";
  
  
  if($self->format ne "png" && $self->format){
      warn "try to convert from png to different format, this is not implemented!\n";
       $self->file_location("../../$file_location/$file");
      #convert the image
      #unlink($Bio::Pfam::Web::PfamWWWConfig::tempdir/$file)";
  }else{
      $self->file_location("../../$file_location/$file");
  }

  
}

=head2 _combine_images

   Title    : _combine_images
   Usage    : $self->_combine_images($region, $style, $left_image, $right_image, $middle_image, $sizesref);
   Function : This takes all of the individual images that makes up the domain image and combines them
            : with the "master" image.
   Args     : region XML::DOM object, the domain style (big,med,sml), left edge image,
            : right edge image, middle image, a reference to the size hash.

   **** still need to implement small curved *****

=cut

sub _combine_images {
    my($self, $region, $style, $left, $right, $straight, $l_style, $r_style, $sizes) = @_;
    #get the region length
    my $reg_length = $region->getAttribute("end")-$region->getAttribute("start") + 1;

	
    if ($style =~ /sml/) {
	my $start = $region->getAttribute("start");
	
	for ($start =  $region->getAttribute("start"); $start <=($region->getAttribute("end")-100); $start +=100 ){
	    $self->image->copy($straight, $start, $self->y_start + 7*$self->scale_y, 0, 0, 100, $$sizes{$style});
	}
	
	$self->image->copy($straight, $start, $self->y_start + 7*$self->scale_y, 0, 0, $region->getAttribute("end")-$start+1, $$sizes{$style});
	
    } elsif ($style =~ /med/) {
    #do something

  } elsif($style =~ /big/) {
    #must be a big shape......
    my $left_shape_length = $$sizes{$l_style};
    if($left){
      $self->image->copy($left, $region->getAttribute("start"), $self->y_start, 0, 0, $left_shape_length, $$sizes{$style});
    }
    
    #Do the middle
    my $right_shape_length = $$sizes{$r_style};
    my $middle_width = $reg_length;
    $middle_width = $middle_width - $left_shape_length - $right_shape_length;

    ### $middle_width-- if ( ($region->left_shape() =~ /small/) || ($region->right_shape() =~ /small/) ); # only need to test once
    
    $self->image->copyResized($straight, $region->getAttribute("start") + $left_shape_length    , $self->y_start, 4, 0, $middle_width , $$sizes{$style}, 10, $$sizes{$style});

    #Do the right edge
    if($right){
      $self->image->copy($right, $region->getAttribute("end") - $right_shape_length, $self->y_start, $right_shape_length,0, $right_shape_length, $$sizes{$style});
    }
  }
  #Add a label to the region
  $self->_funky_label($region, $reg_length);
}

=head2 _straight

   Title    : _straight
   Usage    : $self->straight($image_height, $rgbhashref1, $rgbhashref2)
   Function : produces a template image for the domains.  The image embossed appearance
            : comes from the overlay of the rectangles (see internal comments for more).
   Args     : images height, RGB colour hash, RGB colour hash
   Returns  : GD image, and the mirror of the gradient

=cut

sub _straight {
  my($self, $image_size, $colour1, $colour2) = @_;
  
  my $image_width = 50;
  my $st_im = new GD::Image($image_size, $image_size);
  my $rev_im = new GD::Image($image_size, $image_size);
  #background colour is set to grey
  my $bg_colour = $self->bg_colour;
  my $white = $rev_im->colorAllocate($$bg_colour{R},$$bg_colour{G},$$bg_colour{B});
  $rev_im->transparent($white);
  $rev_im->interlaced('true');

  $white = $st_im->colorAllocate($$bg_colour{R},$$bg_colour{G},$$bg_colour{B});
  $st_im->transparent($white);
  $st_im->interlaced('true');
  my $c1 = $st_im->colorAllocate($$colour1{'R'}, $$colour1{'G'}, $$colour1{'B'});
  #Colour rectangle using colour1

# |------------||    This is how the straight image is drawn.  There on the background 
# ||----------|||    colour (0) there are three rectangles. The first is a solid colour
# |||        ||||    using colour1 (1).  Then there is a gradient running from colour1 to
# ||----3-----|||    colour2 (rectangle 2).  Finally, the reverse of rectange is drawn.
# |-----2------||    for each of these three rectangles, the size of the rectangle is
# ------1-------|    reduced by 1 pixel.  For rectangle 1, it is only 1 pixel of x and y
# ------0--------    is produced to give the embossed look.

  $st_im->filledRectangle(0, 0,  $image_width - 1 , $image_size  - 1, $c1);  
  $self->_funky_rectangle($st_im,  1,  1, $image_width - 2, $image_size - 2, $colour1, $colour2);
  $self->_funky_rectangle($rev_im,  1,  1, $image_width - 2, $image_size - 2, $colour1, $colour2);
  $self->_funky_rectangle($st_im, 2 , 2,  $image_width  - 3, $image_size - 3, $colour2, $colour1);

  return ($st_im, $rev_im);
}

=head2 _funky_label

   Title    : _funky_label
   Usage    : $self->_funky_label($region_length);
   Function : This takes a region XML::DOM object and prints the label (if any)
            : such that is it whitetext with a black shadow.
   Args     : region XML::DOM object, $region_length

=cut

sub _funky_label {
  my ($self, $region, $width) = @_;

  if($region->getAttribute("label")){
    my $text = $region->getAttribute("label");
      
    #### x_offset 
    my $min_region_width =  (((CORE::length($text)-1) * 7) + 10);
    my $width_offset = int(($width - $min_region_width) /2 );
    $width_offset = 1 if($width_offset < 1);
    my $x = $region->getAttribute("start");
    $x = $x + $width_offset;

    #Get a scalable font
    #Possible speed up here......
    my ($font, $y_offset);
    #test image height against the font height
    if($self->_max_domain_height/2 >= 13){
       $font = GD::Font->MediumBold();
       $y_offset = int($self->_max_domain_height/2 - 13/2); 
    }elsif($self->_max_domain_height/2 >= 8){
      $font = GD::Font->Tiny();
      $y_offset = int($self->_max_domain_height/2 - 8/2);
    }else{
      #no font as image is too small.
    }
    #Work out if the region is big enough to have the label
    if($font){
      if ($width - 4 > ($font->width * CORE::length($text))){
	$x = $x + 2;
	my $black = $self->get_colour( ('R'=>0,'G'=>0,'B'=>0));
	my $white = $self->get_colour( ('R'=>255,'G'=>255,'B'=>255));
	
	$self->image->string($font, $x, $self->y_start + $y_offset, $text, $black);
	$self->image->string($font, $x-1, $self->y_start + $y_offset, $text, $white);
      }
    }
  }
}

=head2 _funky_rectangle

   Title    : _funky_rectangle
   Usage    : $self->funky_rectangle()
   Function : Draw a rectange on the supplied GD image, and colours it with a
            : a colour gradient between to two supplied colours, running top to bottom
   Args     : GD image, rectangle co-ordinates, 2 colours (hashes)

=cut

sub _funky_rectangle {
  my ($self, $image, $x1, $y1, $x2, $y2, $colour1, $colour2) = @_;
  
  #Okay we need to add colour1 and colour2 to the colourmap.
  $self->{colourMap}->add_colour($colour1);
  $self->{colourMap}->add_colour($colour2);
  my @colours;
  foreach ($colour1, $colour2){
    push(@colours, $self->{colourMap}->add_rgb([$$_{R},$$_{G},$$_{B}]));
  }
  my %colourcache;
  my @gradient = $self->{colourMap}->build_linear_gradient(30, \@colours);
  
  #Okay, work out the number of colours in the gradient, then the size of each
  #step.
  my $steps = scalar @gradient;
  my $i     = $steps -1;
  my $ty1   = $y1;
  my $ty2   = $y2;
  my $dy    = ($ty2-$ty1)/$steps;
  
  #Nasty contruct by mm1, by hey lets keep it and remove the redundancy!
  while(1) {
    last if($i <= 0);
    my $c = $gradient[$i];
    if(! $colourcache{$c}){
      $colourcache{$c} = $image->colorAllocate($self->{colourMap}->rgb_by_name($c));
    }
    $image->filledRectangle($x1, $ty1, $x2, $ty2, $colourcache{$c});
    $ty1+=$dy;
    $i--;
  }
}


=head2 sml

    Title     : sml
    Usage     : 
    Function  :
    Args      : 
    Returns   :
***Stripes affected by scale, fix!***.

=cut

sub sml {
  my($self, $region, $im_height, $colour) = @_;
  my $region_length = 100;
  my $new_im = new GD::Image($region_length, $im_height);
  my $bg_colour = $self->bg_colour;
  my $white = $new_im->colorAllocate($$bg_colour{R}, $$bg_colour{G}, $$bg_colour{B});
  $new_im->transparent($white);
  $new_im->interlaced('true');
  
  #This next bit is a complete and utter hack to test for pfamB
  if(ref($colour) eq "HASH"){
    my $other_col = $new_im->colorAllocate($$colour{R}, $$colour{G}, $$colour{B});
    $new_im->filledRectangle(0, 0, $region_length, $im_height, $other_col);
  }elsif(ref($colour) eq "ARRAY"){
    #Okay looks like multiple colours have been supplied
      my @stripe_colours;
      foreach my $rgb (@{$colour}){
	  
	  #stripes
	  push(@stripe_colours, $new_im->colorAllocate($$rgb{R}, $$rgb{G}, $$rgb{B}));
	  
      }
      
      my $dy = $im_height/12;
      # stripe 1
      $new_im->filledRectangle(0, 0, $region_length, 1*$dy, $stripe_colours[0]);
      # stripe 2
      $new_im->filledRectangle(0, 2*$dy, $region_length, 3*$dy, $stripe_colours[1]);
      # stripe 
      $new_im->filledRectangle(0, 3*$dy, $region_length, 4*$dy, $stripe_colours[2]);
      # stripe 4
      $new_im->filledRectangle(0, 4*$dy, $region_length, 8*$dy, $stripe_colours[3]);
      # stripe 5
      $new_im->filledRectangle(0, 8*$dy, $region_length, 10*$dy, $stripe_colours[4]);
      # stripe 6
      $new_im->filledRectangle(0, 10*$dy, $region_length, 11*$dy, $stripe_colours[5]);
      # stripe 7
      $new_im->filledRectangle(0, 11*$dy, $region_length, 12*$dy, $stripe_colours[6]);      
    
  }
  my $y_count = 1;
  my($prev_x) = 0;

  #This part is doing the transparency
  my ($start_x) = 1;
  my $x_trace = $start_x;
  while ($y_count <= $im_height - 2) {
    while($x_trace <= $region_length - 2) {
      $new_im->filledRectangle($x_trace, $y_count, $x_trace , $y_count, $white);
      $x_trace = $x_trace + 4;
    }
    if ($start_x == 1) {
      $start_x = 3;
      $x_trace = 3;
    } else {
      $x_trace = 1;
      $start_x = 1;
    }
    $y_count++;
  }
  return $new_im;
}



sub straight {
  my ($self, $region, $colour1, $colour2, $reverse) = @_;
  my $image_height = $self->_max_domain_height;
  my $image_width = 4;
  
  my $im = new GD::Image($image_width,$image_height);
  my $bg_colour = $self->bg_colour;
  
  my $white = $im->colorAllocate($$bg_colour{R},$$bg_colour{G},$$bg_colour{B});
  $im->transparent($white);
  $im->interlaced('true');
  
  my $c1 = $im->colorAllocate($$colour1{R}, $$colour1{G}, $$colour1{B});
  my $c2 = $im->colorAllocate($$colour2{R}, $$colour2{G}, $$colour2{B}); 
  
  #top horizontal line
  $im->line(0,0,3,0,$c1);
  
  for(my $i = 1; $i < ($image_height - 1); $i++){
    $im->line(0,$i,0,$i,$c1);
    #$im->line(1,$i,2,$i,$c2);
    $im->line(3,$i,3,$i,$c1);
  }
  $im->setTile($reverse);
  $im->filledRectangle(1,1, 2, $image_height-1, gdTiled);
  #bottom horizontal line
  $im->filledRectangle(0,$image_height - 2, 3, $image_height - 1, $c1);
  return($im ,$im);
}


sub curvedsmall {
  my ($self, $region, $colour1, $colour2, $straight, $reverse) = @_;
  my $diff = $region->getAttribute("end")  - $region->getAttribute("start");

#Image layout
#     ###########
#    #@@@@@@@@@@@
#   #@+++++++++++
#   #@+++++++++++
#   #@+++++++++++
#    #@@@@@@@@@@@
#     ########### 


  my ($image_height, $image_width);
  $image_height = $self->_max_domain_height;
  $image_width = 6;
  ##########################################
  ###### GRADIENT COLOURS ##################
  ##########################################
  #my @colours;
  my $im = new GD::Image($image_width,$image_height);
  my $bg_colour = $self->bg_colour;
  my $white = $im->colorAllocate($$bg_colour{R},$$bg_colour{G},$$bg_colour{B});
  $im->transparent($white);
  $im->interlaced('true');

  
  
  
  my $outer_colour = $im->colorAllocate($$colour1{R}, $$colour1{G}, $$colour1{B});
  my $inner_colour = $im->colorAllocate($$colour2{R}, $$colour2{G}, $$colour2{B});
  
  if ($diff <=2) {
    $im->setTile($straight);
    $im->filledRectangle(0, 0, $image_width, $image_height, gdTiled);
    return $im;
  } else{
    


    ######################
    #### outer colour ####
    ######################
    
    $im->line(2, 0, $image_width - 3, 0, $outer_colour); # top line
    $im->line(1, 1, 1, 1, $outer_colour); # top left indent
    $im->line(0, 2, 0, $image_height - 3, $outer_colour); # left line
    $im->line(1, $image_height - 2, $image_width-3, $image_height - 2 , $outer_colour); # bottom left indent
    $im->line(2, $image_height - 1, $image_width-3, $image_height - 1, $outer_colour); # bottom line
    $im->line($image_width - 2, $image_height - 2, $image_width - 2, $image_height - 2, $outer_colour); # bottom right indent
    $im->line($image_width - 1, 2, $image_width - 1, $image_height - 3, $outer_colour); # right line
    $im->line($image_width - 2, 1, $image_width - 2, 1, $outer_colour); # top right indent
    
    ##############################
    ### inner highlight colour ###
    ##############################
    
    $im->line(2, 1, $image_width - 3, 1, $inner_colour); ### top line
    
    
    #  side lines
    my $side_temp = 2;
    $im->setTile($reverse);
    $im->line(1, 2 , 1, $image_height - 2, gdTiled);
    $im->line($image_width-2, 2, $image_width - 2, $image_height - 2, gdTiled);
     
    ##############
    ### MIDDLE ###
    ##############
    $im->setTile($straight);
    $im->filledRectangle(2, 2, $image_width-3, $image_height-3, gdTiled);
    #$im->line(2, $image_height-2 , $image_width - 3, $image_height - 2, $outer_colour); ## bottom line
    #$im->line(2, $image_height - 1 , $image_width - 3, $image_height-1, $outer_colour); # bottom line
  }
  
  return ($im, $im);
  
}

#this is jagged
sub jagged {
  my($self, $region, $colour1, $colour2, $straight, $reverse) = @_;
  my $width = 26;
  #Set up the height of the three domains
  my $height = $self->_max_domain_height;
  my $arrow_size = 4;  ## default 4
  #Create an empty image
  my $im = new GD::Image($width, $height);
  my $bg_colour = $self->bg_colour;
  my  $white = $im->colorAllocate($$bg_colour{R},$$bg_colour{G},$$bg_colour{B});
  # make the background transparent and interlaced
  $im->transparent($white);
  $im->interlaced('true');

  #Foundation colour
  my $colour = $im->colorAllocate($$colour1{R}, $$colour1{G}, $$colour1{B} );
  my $x_start = 0;
  my $command = "plus";
  my $length = ($width - $arrow_size);
  $length--;
  for (my $y = 0; $y < $height; $y++ ){
    $im->filledRectangle($x_start, $y, $x_start + $length, $y+1, $colour);

    $x_start-- if ($command eq "minus");
    $x_start++ if ($command eq "plus");
    if (($x_start - $arrow_size == 0) && ($command =~ /plus/) ) {
      $command = "minus";
    }
    if ($x_start <= 0) {
      $command = "plus";
    }
  }
  my $trim = 2 ;
  foreach my $image ($reverse, $straight){
    $im->setTile($image);
    my $x_start = $trim/2;
    my $command = "plus";
    $length -= 2;
    for (my $y = 1; $y < $height; $y++ ){
      $im->filledRectangle($x_start, $y, $x_start + $length, $y+1, gdTiled);
      $x_start-- if ($command eq "minus");
      $x_start++ if ($command eq "plus");
      if (($x_start - $arrow_size == $trim/2) && ($command =~ /plus/) ) {
	$command = "minus";
      }
      if ($x_start <= $trim/2) {
	$command = "plus";
      }
    }
    $trim +=2;
  }
  return ($im, $im);
}

sub jaggedsmall {
  my($self, $region, $colour1, $colour2, $straight, $reverse) = @_;
  my $width = 6;
  #Set up the height of the three domains
  my $height = $self->_max_domain_height;
  my $arrow_size = 2;  ## default 1
  #Create an empty image
  my $im = new GD::Image($width, $height);
  my $bg_colour = $self->bg_colour;
  my $white = $im->colorAllocate($$bg_colour{R},$$bg_colour{G},$$bg_colour{B});
  # make the background transparent and interlaced
  $im->transparent($white);
  $im->interlaced('true');

  #Foundation colour
  
  my $x_start = 0;
  my $command = "plus";
  my $length = ($width - $arrow_size);
  $length--;
  $im->setTile($straight);
  for (my $y = 0; $y < $height; $y++ ){
    $im->filledRectangle($x_start, $y, $x_start + $length, $y+1, gdTiled);

    $x_start-- if ($command eq "minus");
    $x_start++ if ($command eq "plus");
    if (($x_start - $arrow_size == 0) && ($command =~ /plus/) ) {
      $command = "minus";
    }
    if ($x_start <= 0) {
      $command = "plus";
    }
  }

  return ($im, $im);
}


sub curved {
  my($self, $region, $colour1, $colour2, $straight, $reverse) = @_;
  my $image_height = $self->_max_domain_height;
  my $image_width = 26;
  
  my $im_border = new GD::Image($image_width,$image_height);
  
  my $bg_colour = $self->bg_colour;
  my $white = $im_border->colorAllocate($$bg_colour{R},$$bg_colour{G},$$bg_colour{B});
  $im_border->transparent($white);
  $im_border->interlaced('true');

  my $c1 = $im_border->colorAllocate($$colour1{R}, $$colour1{G}, $$colour1{B});
  my $c2 = $im_border->colorAllocate($$colour2{R}, $$colour2{G}, $$colour2{B});
  
  #allow us to colour the curved domain according to the image
  

  $im_border->arc($image_width/2,$image_height/2,$image_width-2,$image_height ,0, 360,$c1);
  $im_border->setTile($reverse);
  $im_border->arc($image_width/2,$image_height/2,$image_width-2,$image_height-2 ,0, 360, gdTiled);
  $im_border->setTile($straight);
  my $dw = ($image_width-4)/($image_height-4);
  my $width = $image_width-4;
  for(my $h = $image_height-4; $h > 0; $h--){
    $im_border->arc($image_width/2,$image_height/2, $width  ,$h, 0, 360, gdTiled); # left hand  half circl
    $width -= $dw;
  }
  return ($im_border, $im_border);
}



1;
