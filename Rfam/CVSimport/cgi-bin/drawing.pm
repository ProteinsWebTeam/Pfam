#! /usr/local/bin/perl

use GD;
#use SVG;



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


sub generate_svg {
  my( $file, $tmp_coord, $tmp_all_coord, $tmp_region_coord, $tmp_region_label, $tmp_region_details, $tmp_circular_key, $unit) = @_;
#  print "FILE: $file <P>";
  my (@coord) = @{$tmp_coord};
  my (@all_coord) = @{$tmp_all_coord};
  my (@region_coord)  = @{$tmp_region_coord};
  my (@region_label) = @{$tmp_region_label};
  my (@region_details) = @{$tmp_region_details};
  my (@circular_key) = @{$tmp_circular_key};

#  foreach (@region_details) {
#    print "$_ <BR>";
#  }

  open(_SVG, ">>$file") or die "canna open $file as $! <P>";

  

  print _SVG qq(<?xml version=\"1.0\"?>


                <svg xml:space=\"preserve\" width=\"14000pt\" height=\"1700pt\" viewBox=\"0 0 300 300\">
             
	

		
	       );
  #	onload=\"init(evt)\">
  print _SVG qq(<script language=\"JavaScript1.2\" >
<![CDATA[
 var storeSequences = new Array();
 );
 
   my $for_count = 0;
   foreach (@region_details) {
     my($label, $version, $newpoints, $existingpoints, $rfam_id) = split(/~/, $_);
     print _SVG "storeSequences[$for_count] = new addSeq(\"$label\", \"$version\", \"$newpoints\", \"$existingpoints\", \"$rfam_id\");\n";
     $for_count++;
   }
   #	 storeSequences[0] = new addSeq("Cobalamin 66246-66476", "AL603642.1", "red"); 
   #	 storeSequences[1] = new addSeq("Cobalamin 10383-10598 ", "AL603644.1",  "blue"); 
   #	 storeSequences[2] = new addSeq("Intron_gpII 106834-106910 ", "AL603644.1" , "blue"); 

   print _SVG qq(
		 function addSeq (label, version, newpoints, existingpoints, rfamid) {
		   this.label = label;
		   this.version = version;
		   this.newpoints = newpoints;
		   this.existingpoints = existingpoints;
		   this.rfamid = rfamid;
		 }
		 
		 function parseSeq (verID) {
		   //  regexp = /verID/;
		   
		   //var match_str = storeSequences[2].color;
		   //			   return regexp;
		   for (var i=0; i < storeSequences.length; i++) {
		     var match_str = storeSequences[i].version;
		     //			     return match_str;
		     var match=  match_str.search();
		     if (match > 0) {
		       return storeSequences[i].color;
		       
		     }
		   }
		   
		 }
		 
		 
		 
		 function enlarge_circle(evt) {
		   var circle=evt.getTarget();
		   //	   circle.setAttribute(\"r\", 50);
		   
		   var blee;
		   regexp = circle.getAttribute("id");
		   for (var i=0; i < storeSequences.length; i++) {
		     var match_str = storeSequences[i].version;
		     
		     var match =  match_str.search(regexp);
		     //blee = match_str + " " +  regexp;
		     if (match >= 0) {
		       
		       var line = svgDocument.getElementById(storeSequences[i].label);
		       //  line.setAttribute("points", storeSequences[i].newpoints);
		       line.setAttribute("style", "fill:red; stroke:red;");
		     }
		   }
		   
		   var newTextID = svgDocument.createTextNode( circle.getAttribute("id"));
		   
		   //	  var blee =  parseSeq(circle.getAttribute("id"));
		   //   var newText = svgDocument.createTextNode(blee );
		   var newText = svgDocument.createTextNode( circle.getAttribute("id"));
		   
		   circle = svgDocument.getElementById("pct0");
		   
		   circle.replaceChild(newText, circle.getFirstChild() );
		 }
		 
		 function shrink_circle(evt) {
		   var circle = evt.getTarget();
		   var newText = svgDocument.createTextNode( " ");
		   //  circle.replaceChild(newText, circle.getFirstChild() );
		   
		   // REST TO EXISTING POINTS
		     regexp = circle.getAttribute("id");
		   for (var i=0; i < storeSequences.length; i++) {
		     var match_str = storeSequences[i].version;
		     
		     var match =  match_str.search(regexp);
		     //blee = match_str + " " +  regexp;
		     if (match >= 0) {
		       // blee = "WOW " + match + " " + match_str;
		       
		       var line = svgDocument.getElementById(storeSequences[i].label);
		       // line.setAttribute("points",storeSequences[i].existingpoints );
		       line.setAttribute("style", "fill:black; stroke:black;");
		     }
		   }
		   
		   circle = svgDocument.getElementById("pct0");
		   
		   circle.replaceChild(newText, circle.getFirstChild() );
		   
		   //  var circle=evt.getTarget();
		   //   circle.setAttribute(\"r\", 25);
		 }
		 // ]]>
		</script>
	       );

  ## Guide circle to show the size of the genome!
  print _SVG "<circle cx=\"160\"  cy=\"160\" r=\"130\" style=\"stroke: blue; fill:white;\"  />\n";
  # point of origin
 # print _SVG "<line x1=\"160\"  y1=\"10\" x2=\"160\" y2=\"30\" style=\"stroke: blue;\"  />\n";
 # print _SVG "<text id=\"poo\" x=\"160\" y=\"8\" style=\"font-size: 7pt; text-anchor: middle;\">Point of Origin</text>\n";
  
  my @poo_coords = ("258.955525354751 60.965642316592,251.887273543698 68.039525008264", "299.999955610457 159.888514260497,289.999958781139 159.896477527605", "259.11312721078 258.876630274801,252.033618124296 251.814013826601", "160.222971408308 299.999822441856,160.207044879143 289.99983512458", "61.2023275064133 259.191833886918,68.259304113098 252.106702894995", "20.0003995057195 160.334456935719,30.0003709695967 160.310567154597", "60.7295223380897 61.2813479388208,67.8202707425118 68.332680228905");

  my ($count_unit);
  $count_unit = 0;
  foreach my $poo (@circular_key) {
    print _SVG "<polygon  id=\"\" ";
    print _SVG "points =\" ";
    print _SVG "$poo";
    print _SVG "\"  style=\"fill:blue; stroke:blue;\"/> \n";
    my ($start, $end) = split(/,/, $poo);
    my ($tmp_x1, $tmp_y1) = split(/\s+/, $start);
    my $x1 = int $tmp_x1;
    my $y1 = int $tmp_y1;

    my ($tmp_x2, $tmp_y2) = split(/\s+/, $end);
    my $x2 = int $tmp_x2;
    my $y2 = int $tmp_y2;

   
    ## unit label
    my $multi;
    $multi = 100;
    $multi = 10 if (!$unit);
    if (not ($count_unit % 5)) { 
     #  print "$x1 , $y1 , $x2, $y2 [ $poo] <BR>";
      print _SVG "<text id=\"poo2\" x=\"$x1\" y=\"$y1\" style=\"font-size: 4pt; text-anchor: middle;\">" . $count_unit * $multi. "</text>\n";
    }

    #   print "poo: $poo , x: $x , y: $y<BR>";
    #} elsif ( ($unit) && (not ($count_unit % 10)) )  { 
     # my $tmp = $count_unit / 10;
     
     # print _SVG "<text id=\"poo2\" x=\"$x\" y=\"$y\" style=\"font-size: 4pt; text-anchor: middle;\">$tmp Mb</text>\n";
    #}

    $count_unit++;
  }



  ## First outer circle of the genome
  print _SVG "<circle cx=\"160\"  cy=\"160\" r=\"100\" style=\"stroke: black; fill:white;\"  />\n";
 

## Shoes where the outer lines are
#print _SVG "<circle cx=\"160\"  cy=\"160\" r=\"150\" style=\"stroke: black; fill: white; \"/>\n";


# print _SVG "<line x1=\"100\" y1=\"100\" x2=\"100\" y2=\"0\" style=\"stroke: green;\"/>";
  #foreach (@coord) {
  #  my($x, $y) = split(/~/, $_);

   #  print _SVG "<line x1=\"100\" y1=\"100\" x2=\"$x\" y2=\"$y\" style=\"stroke: red;\"/>";
    
 # }
  
  my @col; # = qw (red green blue lavendar yellow grey #DB08BB purple orange #5888E8 brown lilac pink cyan red green blue lavendar yellow grey purple #F266CC  orange brown lilac pink cyan red green blue lavendar #2752AB yellow grey purple orange brown lilac pink cyan);

  open(_COL, "/nfs/WWWdev/TEST_docs/htdocs/Software/Rfam/data/cols.dat");
 # push @col, "#00f8ff";
  while(<_COL>) {
    chop($_);
    push @col, $_;
  #  print "$_<BR>";
  }

  close(_COL);
  my (%regions_cols);
  my $reg_count = 0;
  my $count = 0;
#  foreach (@region_coord) {
  foreach (@all_coord) {

   
    my $version = $coord[$count];
  # print "VER: $version <BR>";
    my $acc = $1 if ($version =~ /^(\S+\.\d+)/);
    print _SVG "<a xlink:href=\"$RfamWWWCongif::WWW_link/cgi-bin/Rfam/seqget.pl?name=$acc\">\n";
   # print "$version<BR>";
    print _SVG "<polygon  id=\"$version\" ";
    print _SVG "points =\" ";
    print _SVG "$_";
    my $col = $col[$count];
    print _SVG "\"  style=\"fill:#CCCCCC; stroke:black;\" onmouseover=\"enlarge_circle(evt)\" onmouseout=\"shrink_circle(evt)\"  /> \n";
 #   print _SVG "</a>";
    $count++;
  }

  $count = 0;
  
  foreach (@region_coord) {
    my($x1, $y1, $x2, $y2);
    if ($_ =~ /(\d+\.\d+)\s+(\d+\.\d+)\,(\d+\.\d+)\s+(\d+\.\d+)/) {
      $x1 = $1; $y1 = $2; $x2 = $3; $y2 = $4;
    }
   # print "$x1, $y1, $x2, $y2 <BR>";
    my $version = $region_label[$count];
#   print "VER: $version <BR>";
    my $acc = $1 if ($version =~ /^(\S+)\s+/);
  #  print "ACC: $acc :: VERSION: $version <BR>";
  #  print "$version <BR>";
    my $url = "http://www.sanger.ac.uk/cgi-bin/Rfam/getfasta.pl";
  #  print _SVG "<a xlink:href=\"$url\">";

  #  print _SVG "<line id=\"$version\" x1=\"$x1\" y1=\"$y1\" x2=\"$x2\" y2=\"$y2\" style= ";
    print _SVG "<polygon  id=\"$version\" ";
    print _SVG "points =\" ";
    print _SVG "$_";
    my $col = $col[$count];
    print _SVG "\" style=\"fill:black; stroke:black;\"   onmouseover=\"enlarge_circle(evt)\" onmouseout=\"shrink_circle(evt)\" /> \n";

   # print _SVG "</a>";
    $count++;
  }

  print _SVG "<text id=\"pct0\" x=\"160\" y=\"160\" style=\"font-size: 9pt; text-anchor: middle;\"> </text>\n"; 


#  print _SVG "<line x1=\"0\" y1=\"100\" x2=\"100\" y2=\"0\" style=\"stroke: red;\"/>";
#  print _SVG "<line x1=\"100\" y1=\"0\" x2=\"200\" y2=\"100\" style=\"stroke: red;\"/>";
#  print _SVG "<line x1=\"0\" y1=\"100\" x2=\"100\" y2=\"200\" style=\"stroke: red;\"/>";
#   print _SVG "<line x1=\"100\" y1=\"200\" x2=\"200\" y2=\"100\" style=\"stroke: red;\"/>";

#  print _SVG "<line x1=\"100\" y1=\"0\" x2=\"100\" y2=\"100\" style=\"stroke: green;\"/>";
#  print _SVG "<line x1=\"100\" y1=\"100\" x2=\"200\" y2=\"100\" style=\"stroke: green;\"/>";

#   print _SVG "<line x1=\"50\" y1=\"50\" x2=\"200\" y2=\"100\" style=\"stroke: yellow;\"/>";

#   my $text = $offset + ($width / 2);
  # print _SVG "<text x=\"$text\" y=\"106\" style=\"font-family: Tahoma; font-size: 8pt; stroke: none; fill: white;\">" . $region_id. "</text>\n";

#  } elsif ($region_type =~ /pfamB/i) {
   
#    print _SVG " <!--- pfamB -->\n";
#    print _SVG "<rect x=\"$offset\" y=\"96\" width=\"$width\" height=\"3\" style= \"fill:green;\" />\n";
#    print _SVG "<rect x=\"$offset\" y=\"99\" width=\"$width\" height=\"3\" style= \"fill:orange;\" />\n";
#    print _SVG "<rect x=\"$offset\" y=\"102\" width=\"$width\" height=\"3\" style= \"fill:cyan;\" />\n";
#    print _SVG "<rect x=\"$offset\" y=\"104\" width=\"$width\" height=\"4\" style= \"fill:purple;\" />\n";
    
#  }elsif ($region_type =~ /smart/i) {
#      print _SVG "<!--- red $region_id domain -->\n";
#   print _SVG "<rect x=\"$offset\" y=\"93\" width=\"$width\" height=\"20\"  style= \"fill:black;\"/>\n";
 
    
#  }



   print _SVG "\n\n";
 print _SVG "</svg>\n";
  close(_SVG);

#  $offset = $offset + $width;
}

sub _poo {
  my $poo1 = "160 10,162.616533956306 10.0228225693806,162.442098359219 20.0213010647552,160 20";
  my $poo2 = "160 10,162.616533956306 10.0228225693806,162.442098359219 20.0213010647552,160 20";
  
  my $poo3 = "160 10,162.616533956306 10.0228225693806,162.442098359219 20.0213010647552,160 20"; 
  
  my $poo4 = "160 10,162.616533956306 10.0228225693806,162.442098359219 20.0213010647552,160 20"; 
  
  my $poo5 = "160 10,162.616533956306 10.0228225693806,162.442098359219 20.0213010647552,160 20"; 
  
  my $poo6 = "160 10,162.616533956306 10.0228225693806,162.442098359219 20.0213010647552,160 20"; 
  
  my $poo7 = "160 10,162.616533956306 10.0228225693806,162.442098359219 20.0213010647552,160 20"; 

}
1;
