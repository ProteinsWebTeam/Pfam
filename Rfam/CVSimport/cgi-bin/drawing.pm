#! /usr/local/bin/perl

use GD;



=head2 generate_domain_gif

 Title   : generate_domain_gif
 Usage   :  my $new_image =  generate_domain_gif( $width, $colours, $region, $return_gif, $width_offset, $beg_shape, $end_shape);
        
 Returns :
        1.
 Args    :
   1.  $width, $colours, $region, $return_gif, $width_offset, $beg_shape, $end_shape

=cut


sub generate_domain_gif { 
  # create a new image
  
  #########
  # This contains a lot of fussy code that has been worked out exactly, change at your peril !
  #
  #
  #
 
  
  my($image_length, $tmp_src_name, $image_text, $new_gif, $width_offset, $beg_shape, $end_shape) = @_;


  my $src_name;
  my @urls = split(/\//, $tmp_src_name);
  foreach (@urls) {
    $src_name = $_ if ( ($_ =~ /colors/i) || ($_ =~ /smart/i)  );
  }

  $im = new GD::Image($image_length, 23);
  
  
  $black = $im->colorAllocate(0, 0, 0);
  $white = $im->colorAllocate(255, 255, 255);
  

  my $template_file;

  if ($src_name =~ /smart/i) {
    $template_file = $RfamWWWConfig::image_dir . "/" . "$src_name";
  } else {

    $template_file = $RfamWWWConfig::tempdir . "/" . "$src_name";
  }

  open(GIF,$template_file) ||  die print STDERR  "Canna do that $! \n";;
  $img_copy = newFromGif GD::Image(GIF);
  my $grab_width = $image_length;
  $grab_width = "112" if($image_length > 112); 
  $im->copyResized($img_copy,0,0,0,0,$image_length,23,$grab_width,195);
  close(GIF);
  
  ### ADD THE POLYGON
  
  if ($image_length >= 10) {
    
    
    my $angle = 5;
    ### Normal image
    if ($beg_shape =~ /l/) {
      
      
      
      # |------
      # |     /
      # | 1  /
      # |   /
      # |  /
      # | /
      # ||
      # ||
      # | \
      # |  \
      # |   \
      # | 2  \
      # |     \
      # -------
      
      #   ## top left 1
      my $poly1 = new GD::Polygon;
      $poly1->addPt(0,8.5);
      $poly1->addPt( $angle,0);
      $poly1->addPt(0,0);
      $im->filledPolygon($poly1, $white);
      
      ## bottom left 2 
      my $poly2 = new GD::Polygon;
      $poly2->addPt(0,16.5);
      $poly2->addPt( $angle,23);
      $poly2->addPt(0,23);
      $im->filledPolygon($poly2, $white);
    }
    
    
    if ($end_shape =~ /r/) {
      
      #-----
      # \    |
      #  \ 1 |
      #   \  |
      #    \ |
      #     || 
      #     ||
      #    / |
      #   /  |
      #  /   |
      # / 2  |
      #-----
      
      
      
      ## top right 1
      my $poly3 = new GD::Polygon;
      $poly3->addPt($image_length,8.5);
      $poly3->addPt( $image_length - $angle ,0);
      $poly3->addPt($image_length,0);
      $im->filledPolygon($poly3, $white);
      
      ## bottom right 2
      my $poly4 = new GD::Polygon;
      $poly4->addPt($image_length,16.5);
      $poly4->addPt($image_length - $angle ,23);
      $poly4->addPt($image_length ,23);
      $im->filledPolygon($poly4, $white);
      
    }
    
    
    
    
  } else {
    ### image is very small
    
    if (  ($beg_shape =~ /l/) && ($end_shape =~ /r/) ) {
      
      # |------------
      # |     /\     |
      # | 1  /  \  3 |
      # |   /    \   |
      # |  /      \  |
      # | /        \ |
      # | \        / |
      # |  \      /  |
      # |   \    /   |
      # | 2  \  /  4 |
      # |     \/     |
      # --------------
      
      
      # 1  ## top left
      my $poly5 = new GD::Polygon;
      $poly5->addPt(0,8.5);
      $poly5->addPt( 4 ,0 - 12);
      $poly5->addPt(0,0);
      $im->filledPolygon($poly5, $white);
      
      ## 2 bottom left
      my $poly6 = new GD::Polygon;
      $poly6->addPt(0,16.5);
      $poly6->addPt( 3 ,23 + 4);
      $poly6->addPt(0,23);
      $im->filledPolygon($poly6, $white);
      
      
      ## 3 top right
      $poly = new GD::Polygon;
      $poly->addPt($image_length,8.5);
      $poly->addPt( $image_length - 4 ,0 - 12);
      $poly->addPt($image_length  ,0);
      $im->filledPolygon($poly, $white);
      
      ## 4 bottom right
      $poly = new GD::Polygon;
      $poly->addPt($image_length  ,16.5);
      $poly->addPt( $image_length - 3 ,23 + 4);
      $poly->addPt($image_length ,23);
      $im->filledPolygon($poly, $white);
      
      
      
      
      
    } elsif ( ($beg_shape =~ /l/) && ($image_length > 2) ) {
      
      # |------
      # |     /
      # | 1  /
      # |   /
      # |  /
      # | /
      # ||
      # ||
      # | \
      # |  \
      # |   \
      # | 2  \
      # |     \
      # -------
      
      
      my $line_print = $image_length;
      
      $line_print =  "0.5" if ($image_length =~ /3/);

      # 1
      $poly = new GD::Polygon;
      $poly->addPt(0 ,8.5);
      $poly->addPt( $line_print ,0);
      $poly->addPt(0 ,0);
      $im->filledPolygon($poly, $white);
      
      
      # 2
      $poly = new GD::Polygon;
      $poly->addPt( 0 ,16.5);
      $poly->addPt( $line_print  ,23);
      $poly->addPt( 0 ,23);
      $im->filledPolygon($poly, $white);
      
      
    } elsif ( ($end_shape =~ /r/) && ($image_length > 2)  ) {
      
      #-----
      # \    |
      #  \ 1 |
      #   \  |
      #    \ |
      #     || 
      #     ||
      #    / |
      #   /  |
      #  /   |
      # / 2  |
      #-----
      
      
      my $line_print = 0 ;
      
      $line_print =  "0.5" if ($image_length =~ /3/);
      
      # 1
      $poly = new GD::Polygon;
      $poly->addPt($image_length,8.5);
      $poly->addPt( $line_print,0);
      $poly->addPt($image_length, 0);
      $im->filledPolygon($poly, $white);

      # 2
      $poly = new GD::Polygon;
      $poly->addPt($image_length,16.5);
      $poly->addPt( $line_print,23);
      $poly->addPt($image_length, 23);
      $im->filledPolygon($poly, $white);



    }
     
  }
 
  if ($src_name !~ /smart/i) {
    $im->transparent($white);
  }



  ## Add the font
  if ($src_name =~ /smart/i) {
 
    $im->string(gdTinyFont,$width_offset + 2,1,"Smart:",$white) if ($image_text);
    $im->string(gdTinyFont,$width_offset + 2,10,$image_text,$white);


  } else { ## pfamA
    $im->string(gdMediumBoldFont,$width_offset + 2,4,$image_text,$black);
    
  }


  ### prepare the new gif to be written to & write to it !
  my $temp_full_gif_dir = $RfamWWWConfig::tempdir . "/new_gifs/" . "$new_gif";


  my $full_gif_dir = $1 if ($temp_full_gif_dir =~  /^([-\w\d_~\/.]+)$/);
  open(OUTFILE, ">$full_gif_dir") or print STDERR "Canna print out file  $! ";
 
  binmode OUTFILE;
  # Convert the image to GIF and print it on standard output
  print OUTFILE $im->gif;

 
  close(OUTFILE) or print STDERR "error closing pfam gif: $new_gif as $!\n";;


}



sub create_family_header_tag  {

 
  my ($acc, $name) = @_;
#print "NAME: $name <P>";
  my @arr = ( 'small', 'large');
#print STDERR "BLEE\n";
####  my $db = &RfamWWWConfig::get_database();
 
  my $pfam_type = "family";
##  $pfam_type = $db->get_family_type($acc) if ($acc =~ /^PF/i);  ## only get type if pfamA

  foreach (@arr) {
    
    my $output_file = $RfamWWWConfig::tempdir . "/new_gifs/" . $acc . "_" . $_ . ".gif";
    #print "OUT: $output_file <P>";
    if (-e $output_file) {
    #  print "EXISTS $file <P>";

  } else {
   # print "DOSENT NAME: $name <P>";
     _generate_family_gif($acc, $name, $_, $pfam_type, $output_file) if ($_ =~ /[A-Z]/i);
  }


   
    
  }
  

}



sub _generate_family_gif {

  my ($acc, $name, $type,$pfam_type, ,$temp_full_gif_dir) = @_;
 # print "ARES: $acc, $name, $type, $pfam_type, $temp_full_gif_dir <BR>";

  ########### ADD CODE TO CHECK IF THE FILE IS ALREADY CREATED #############

  my $template_file = $RfamWWWConfig::file_root . "/gifs/page_header" . "/" . "family_name_" . $type . "_tab.gif";

#  print "TEMP: $template_file <P>";
  my $im = new GD::Image(150, 25);

  my $color;
  
  if ($type =~ /large/i) {
    $color = $im->colorAllocate(223, 242, 12);
  } else {
    $color = $im->colorAllocate(255, 255, 255);
  }

  open(GIF,$template_file) ||  die print STDERR  "Canna do that $! \n";;
  $img_copy = newFromGif GD::Image(GIF);

  $im->copy($img_copy, 0, 0, 0, 0, 180, 25);

  
  close(GIF);
 
  $name = $name . " $pfam_type";

 ## Add the font

##########  WORK OUT WIDTH OFFSET
  
    ##  mim size for text to appear
  my $min_region_width =  (( (length($name) - 1) * 7) + 10);
  my $width_offset = int(  (150 - $min_region_width) /2 );
    $width_offset = 1 if($width_offset < 1);

  $im->string(gdSmallFont,$width_offset + 10,9, $name,$color);
 

#  my $temp_full_gif_dir = $PfamWWWConfig::tempdir . "/new_gifs/" . $acc . "_" . $type . ".gif";

#  print "TEMP: $temp_full_gif_dir <BR>";
  my $full_gif_dir = $1 if ($temp_full_gif_dir =~  /^([-\w\d_~\/.]+)$/);
 
  open(OUTFILE, ">$full_gif_dir") or print STDERR "Canna print out file  $! ";

  binmode OUTFILE;
  # Convert the image to GIF and print it on standard output
  print OUTFILE $im->gif;

 
  close(OUTFILE) or print STDERR  "Canna print out file $full_gif_dir as : $! ";;




}


sub generate_all_download_gif {

  
  my ($all, $tempname) = @_;
  
  my (@all) = split(/\!/, $all);
  
 # print "GIFS GALORE <P>";
  
  my $total_length = 0;
  
  foreach (@all) {
    my ($junk, $len) = split(/\:/, $_);
    $total_length = $total_length + $len;
    
  }
  
#  print "TOTAL LEN : $total_length <P>";

  my $im = new GD::Image($total_length, 23);

  my $white = $im->colorAllocate(255, 255, 255); ## cgange back to white later

  my $len_count = 0;
  foreach (@all) {

    my ($domain, $len) = split(/\:/, $_);
    
    if($domain eq "space") {
      
      my $template_file = "$RfamWWWConfig::image_dir/space.gif";
      open(GIF,$template_file) ||  die print STDERR  "Canna do that $! \n";;
      $img_copy = newFromGif GD::Image(GIF);
      ## $im->copyResized($img_copy,0,0,0,0,$image_length,23,$grab_width,195);
      my $blee_len = $len;
      $im->copyResized($img_copy, $len_count, 8,  0, 6, $len, 7, 1, 8);

  
      close(GIF);


 #    print "SPACE <BR>";

    } elsif ($domain =~ /colors(\d+)\.gif/) { ######## PFAM B 

      my $template_file = "/nfs/intweb/doctree/htdocs/Software/Pfam/WebSite/temp/$domain";
      open(GIF,$template_file) ||  die print STDERR  "Canna do that $! \n";;
      $img_copy = newFromGif GD::Image(GIF);
      my $blee_len = $len;
      $im->copyResized($img_copy, $len_count, 8,  0, 33, $len, 8, 112, 43);
      close(GIF);

    } elsif ($domain =~ /~/)  {  ## PFAM A : update for smart etc

     
      my $template_file = "/nfs/intweb/doctree/htdocs/Software/Pfam/WebSite/temp/new_gifs/$domain";
      open(GIF,$template_file) ||  die print STDERR  "Canna do that $! \n";;
      $img_copy = newFromGif GD::Image(GIF);
      my($junk, $junk1, $img_len, $junk2, @junk3) = split(/~/, $domain);

      $im->copyResized($img_copy, $len_count, 0,  0, 0, $len , 23, $img_len, 23);
    #  print "PFAMA WIDTH: $len :: ALL: $_  :::: LEN: $len , ACT: $img_len  <BR>";

#  $im->copy($img_copy, $len_count, 0, 0, 0, $len , 23);  ### COPY RESIZED ! 

  
  close(GIF);

  #    print "PFAM A <BR>";
    } else {

      my $template_file = "/nfs/intweb/doctree/htdocs/Software/Pfam/WebSite/gifs/$domain";
      open(GIF,$template_file) ||  die print STDERR  "Canna do that $! \n";;
      $img_copy = newFromGif GD::Image(GIF);
      my $blee_len = $len;
 #     $im->copyResized($img_copy, $len_count, 8,  3, 37, $len + 1, 8, 106, 37);
 $im->copyResized($img_copy, $len_count, 8,  0, 34, $len + 1, 8, 112, 43);
      close(GIF);
    #  print "OTHER : $domain :: $_ <BR>";

      
    }
    
    $len_count = $len_count + $len;
#    print "$_ <P>";
    
  }
 


  ### PRINT OUT THE GIF!

$im->transparent($white);

  my $full_gif_dir = $RfamWWWConfig::tempdir . "/new_gifs/" . $tempname; ## TEMP

  open(OUTFILE, ">$full_gif_dir") or print "Canna print out file  $! ";
  
  binmode OUTFILE;
  # Convert the image to GIF and print it on standard output
  print OUTFILE $im->gif;

 
  close(OUTFILE);
 # print "GIF: $full_gif_dir <BR>";
  return $tempname;

}


1;
