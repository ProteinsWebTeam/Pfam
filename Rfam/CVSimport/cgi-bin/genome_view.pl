#! /usr/local/bin/perl -T

use strict;

use lib './';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';

use CGI;
use RfamWWWConfig;
use Tree;
use GetzTax;
#use GIFAlign;
#use Bio::Rfam::SeqRfam;

$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};

my ( $q, $zoom, $family, $name, $print_name, $tree, @list, @ids, $galign );

$q = new CGI;
#$zoom = 0.5;
my $temp_count = $q->param("temp_count");
my $count = $1 if ($temp_count =~  /^([-\@~:\w.]+)$/);

my $temp = $q->param('genome_acc');
my $genome_acc= $1 if ($temp =~  /^([-\@~\w.\s]+)$/);

my $rdb = &RfamWWWConfig::get_database();
my(@get_auto_genome) = $rdb->query("select auto_genome from genome_entry where genome_acc = '$genome_acc'");

my($auto_genome);
foreach my $temp_table (@get_auto_genome) {

   ($auto_genome) = @{$temp_table};

}
#print "Content-type: text/html\n\n"; print "THE ACC: $genome_acc <P>";
$temp = $q->param('svg');
my $svg = $1 if ($temp =~  /^([-\@~\w.\s]+)$/);

my $species;

my ($species) = &RfamWWWConfig::genome_species_name($genome_acc);
#$species =~ s/~/ /g;
#my($first_species) = split(/\s+/, $species);

print $q->header;
my (@buttons) = qw (all selected);
my $selected_button;
foreach (@buttons) {
  my $temp = $q->param($_ . ".x");
  my $blee = $1 if ($temp =~  /^([-\@~:\w.]+)$/);
  if($blee) {
    $selected_button = $_;
  }

}

my (%regs, @all_regions, %species_results);
if ($selected_button =~ /selected/) {
  my $temp_count = $q->param("max_count");

  my $max_count = $1 if ($temp_count =~  /^([-\@~:\w.]+)$/);
  my $count  = 0;
  while ($count <= $max_count)  {  
    my $temp = $q->param($count);
    my $blee = $1 if ($temp =~  /^([-\@~:\w.]+)$/);
    #  $ids{$blee} = $blee if ($blee);
    if ($blee) {
      $regs{$blee} = $blee;
    }
    $count++;
  }

  (@all_regions) = &RfamWWWConfig::genome_regions($auto_genome, %regs);

} elsif ($selected_button =~ /all/) {
 # print "Content-type: text/html\n\n"; print "HERE : $auto_genome<P>";
  (@all_regions) = &RfamWWWConfig::genome_regions($auto_genome);
} else {
  #(%species_results) =   &RfamWWWConfig::genome_category($first_species);
  (%species_results) =   &RfamWWWConfig::genome_category($auto_genome, 1);

}


print &RfamWWWConfig::header("Genome distribution: <font color=#FFFFcc>$species</font>") if ($selected_button);


#print "first: $first_species <P>";
#my(%species_results) =   &RfamWWWConfig::genome_category($first_species);


#print "BUTTON: $selected_button <P>";
if ($selected_button) {



 print qq ( <table><tr><td><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>Rfam id</TD><TD BGCOLOR=#000070 CLASS=whitetableheader>xsome id</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >xsome start</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >xsome end</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >clone id</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >clone start</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >clone end</TD>
	  );
# if ($svg) {
 #  print "<TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >Contig start</TD><TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >Contig end</TD>";
 #  print "<TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >Chromosome start</TD><TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >Chromosome end</TD>";
# } else {
  # print "<TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >Start</TD><TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >End</TD>";
 #}
 #print "SVG: $svg <P>";
 print "</TR>";

my(@sorted_all_regions);
 foreach (@all_regions) {
   
    my($rfam_id, $rfam_acc,  $genome_acc, $clone_start , $clone_end , $offset_start, $offset_end, $strand , $rfamseq_acc, $seq_start , $seq_end) = split(/~/, $_);

    my($final_clone_start, $final_clone_end);
    if ($strand =~ /\+/) {
      # print "EEP:  $clone_start +  $seq_start  - $offset_start ::: $final_clone_start<BR>";
      $final_clone_start = $clone_start +  $seq_start  - $offset_start;
      $final_clone_end = $clone_start +  $seq_end - $offset_start ;
    } else {
      $final_clone_start = $clone_start + $offset_end - $seq_start; #$chrst + $clen - $hit->{'st'};
      $final_clone_end =  $clone_start + $offset_end - $seq_end; ##$chrst + $clen - $hit->{'en'};
    }
    
    my(%temp) = ( 'xsome_start' => $final_clone_start,
		  'xsome_end' => $final_clone_end,
		  'all' => $_
		  );
    push @sorted_all_regions, \%temp;
  }

 @sorted_all_regions = sort { $a->{'xsome_start'} <=> $b->{'xsome_start'} } @sorted_all_regions;

  foreach (@sorted_all_regions) {
    my($rfam_id, $rfam_acc,  $genome_acc, $clone_start , $clone_end , $offset_start, $offset_end, $strand , $rfamseq_acc, $seq_start , $seq_end) = split(/~/,$_->{'all'}); # split(/~/, $_);
  
   # print "" if ($strand =~ /\+/);
    my $link = $RfamWWWConfig::srsserver;
    my $srs_rfamseq_acc = $1 if ($rfamseq_acc =~ /^(\S+)\.\d+/);
    $link =~ s/THEACC/$srs_rfamseq_acc/;
    #    if (length($desc) > 40 ) {
    #  my $temp = substr($desc, 0, 70);
    #  $temp .= "...";
    #  $desc = $temp;
   # }
   # my $clone_start_sub = $clone_start + $seq_start - $offset_start;
  #  my $clone_end_sub =  $clone_start + $seq_end - $offset_start;
 #   print "$rfam_id, $rfam_acc,  $genome_acc, <B>$clone_start , $clone_end</B> , <B>$clone_start, $clone_end</B>, $strand , $rfamseq_acc, $seq_start , $seq_end  <B>XSOME: $clone_sub ::: CLONE: $clone_sub </B><BR>" if ( ($strand =~ /\+/)  && ($clone_sub eq $clone_sub ) );
    my($final_clone_start, $final_clone_end);
    if ($strand =~ /\+/) {
     # print "EEP:  $clone_start +  $seq_start  - $offset_start ::: $final_clone_start<BR>";
      $final_clone_start = $clone_start +  $seq_start  - $offset_start;
      $final_clone_end = $clone_start +  $seq_end - $offset_start ;
    } else {
      $final_clone_start = $clone_start + $offset_end - $seq_start; #$chrst + $clen - $hit->{'st'};

      $final_clone_end =  $clone_start + $offset_end - $seq_end; ##$chrst + $clen - $hit->{'en'};
    }
    print "<TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=left><A href=$RfamWWWConfig::getacc?$rfam_acc>$rfam_id</A></TD><TD class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour   valign=center align=left>$genome_acc</TD><TD class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour   valign=center align=right>$final_clone_start </TD><TD class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour   valign=center align=right>$final_clone_end </TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=left><A HREF=$link>$rfamseq_acc</A></TD><TD class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>$seq_start</TD><TD class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>$seq_end</TD>";
 #   if ($svg) {

    #  print "<TD class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>$xsome_start</TD><TD class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>$xsome_end</TD>";
  #  }

    print "</TR>";
    $count++;
  }

 print "</TABLE></table></form><P>";

} else {
  
  
  ######## PRINT CONTIG INFO !!! 

  

  my $dol = "$$" . ".svg";
  my $file = "$RfamWWWConfig::tempdir/$dol";
 
  my (@contigs, $got_contigs);
  
 # if ($svg) {
    @contigs = $rdb->get_contigs($auto_genome);
 # } 
  if (!$svg) {
    
    @contigs = ();
    $got_contigs = 1;
  }


  
  my $store_max;
  ### IF THIS IS A CIRCULAR XSOME THEN PRINT IT!!!!!!!!!!!!
  if (@contigs) {
   # print "<TR><TD>";
    # print "CONTIGS: @contigs <P>";
   my $max;
    foreach (@contigs) {
      $max = $1 if ($_ =~ /\S+\~\S+\~\S+\~(\S+)~\S+\~\S+~\S+/);
      #  print "$_ <BR>";
    }
  # print "MAX: $max <BR>";
   $store_max = $max;
    my ($the_sum, $the_angle, @coord, @all_coords, @region_coord ,@region_label, @region_details, @circular_key);
    # print "<IMG SRC=http://www.sanger.ac.uk/Software/Pfam/gifs/smart.png width=900 height=5><P>";
    foreach (@contigs) {
      my($auto_rfamseq, $start, $end, $version, $clone_start, $clone_end, $strand);
      if ($_ =~ /(\S+)\~(\S+)\~(\S+)\~(\S+)\~(\S+)\~(\S+)\~(\S+)/) {
#	print "DOL: $_ <BR>";
#	$auto_rfamseq = $1; $version = $2; $start = $3; $end = $4; $clone_start = $5; $clone_end = $6; $strand = $7;
	($auto_rfamseq,$version, $start, $end,  $clone_start, $clone_end, $strand) = split(/~/, $_);
#	print "AUTO: $auto_rfamseq, START: $start, END: $end, VER:  $version, CLOST: $clone_start, CLOEN:  $clone_end, STR: $strand <BR>";
#	print "MAX: $max <BR>";
	my $frag =  ( ($end - $start) / $max) * 100;
	#  print "<B>SEQ: $version </B><BR>";
	$the_sum += $frag;
	#  $frag = int($frag);
	my $prev = $the_angle;
	my $angle = (360 * $frag) / 100;
	$the_angle += $angle;
	my $merge_points = _get_poly_lines($prev, $the_angle, 100,90, 160);
	push @all_coords, $merge_points;
	
	#  print "FRAG: $frag, ANGLE: $angle<IMG SRC=http://www.sanger.ac.uk/Software/Pfam/gifs/smart.png width=$frag height=15 ALT=$version><BR>";
	
	# print "ALL: @all_points <P>";
	
	#      my $rad = ($the_angle/ 180) * 3.14;
	#      my $opposite = sin($rad);
	#      my $adjacent = cos($rad);
	#      $adjacent = $adjacent * 100;
	#      $opposite = $opposite * 100;
	#      print "ADGACENT = $adjacent, OPP: $opposite  $the_angle<BR>";
	#      my $y = 160 - $adjacent;
	#      my $x = 160 + $opposite;
	#push @coord, $x . "~" . $y;
	push @coord, $version;
	#      print "<B>x: $x, y: $y </B><BR>";
	# print "FRAG: $frag <BR>";
	my @contig_regions = $rdb->get_contig_regions($auto_rfamseq);
	foreach (@contig_regions) {
	  #print "$_ <BR>";
	  my ($rfam_id, $rfam_acc, $reg_start, $reg_end) = split(/~/, $_);
	  
	  
	  ######### SET UP COORDINATES FOR THE LINE
	  my $outer_arc = 110;
	  my $inner_arc = 100;
	  
	  my $extended_outer_arc = $outer_arc;
	  my $extended_inner_arc = $inner_arc;
	 
	  my ($final_clone_start, $final_clone_end);

	  $final_clone_start = $start + $reg_start - $clone_start ;
	  $final_clone_end  = $start + $reg_end - $clone_start;

	  if ($final_clone_start > $final_clone_end) {
	#    print "<BR>REVERSE STRAND : $reg_start > $reg_end<BR>";
	    my $tmp = $reg_start;
	    $reg_start = $reg_end;
	    $reg_end = $tmp;

	  }

	  my $actual_reg_start = $start + $reg_start - $clone_start ; #($start + $reg_start) - 1;
	  my $actual_reg_end = $start + $reg_end - $clone_start; #($start + $reg_end) - 1;


	  if ($final_clone_start > $final_clone_end) {
	    #print "<B>LARGER</B><BR>";
	    $outer_arc  = 90;
	    $inner_arc = 80;
	    $extended_inner_arc = $inner_arc - 10;
	    $extended_outer_arc = $outer_arc;
	  } else {
	    $extended_outer_arc = $extended_outer_arc + 10;
	    #print "NORMAL <BR>";
	  }
	 # print "START: $start, REG: $reg_start ($clone_start - $clone_end :: $strand) <BR>";
	 
	  #	print "$_ <BR>";
	  #	print "<B>ACTUAL END : $actual_reg_end  START: $actual_reg_start </B><BR><BR>";
	  my $label = $rfam_id . " ". $final_clone_start . "-" . $final_clone_end;
	  #print "LABEL: $label <BR>";
	  push @region_label, $label;

	  my $reg_frag =  ( ($actual_reg_end - $actual_reg_start) / $max) * 100;
	  my $reg_angle = (360 * $reg_frag) / 100;
	  #	print "REG FRAG: $reg_frag ANGLE: $reg_angle <BR>";
	  my $prev_frag = ( ($actual_reg_start - 1) / $max) * 100;
	  my $prev_angle = (360 * $prev_frag) / 100;
	  #	#print "PREV FRAG: $prev_frag <BR>";
	  
	  my $new_angle = $reg_angle + $prev_angle;
	  my $merge_coords = _get_poly_lines($prev_angle, $new_angle, $outer_arc,$inner_arc, 160);
	  push @region_coord, $merge_coords;
	  
	  my $new_merger_coords = _get_poly_lines($prev_angle, $new_angle, $extended_outer_arc,$extended_inner_arc, 160);
	  
	  push @region_details, $label . "~". $version . "~" . $new_merger_coords . "~" . $merge_coords . "~" . $rfam_id;
	}
      }
      
      
    }

    ## This is code to worl out where the guides go
    my $p_a = 1;
  #my @arr = qw (45 90 135 180 225 270 315);
    
   my $unit;
   my ($count_unit);
   $count_unit = 0;
   while($p_a < $store_max) {
  #foreach (@arr) {
      my $merge_points;
      my $ang = ($p_a * 360) / $store_max;
      if (not ($count_unit % 5)) { 
	$merge_points = _get_poly_lines($ang, $ang + 0.000001 , 140,130, 160);
      } else {
	$merge_points = _get_poly_lines($ang, $ang + 0.000001 , 135,130, 160);
      }

     push @circular_key, $merge_points;
   # $p_a = $_;
      
      if ($store_max > 1000000) {
	$p_a += 100000;
	$unit = 1;
      } else {
	$p_a += 10000;
	$unit = 0;
      }
      $count_unit++;
   # print "$p_a: $merge_points <P>";
  }

    # print "THE SUM: $the_sum :: THE ANGLE : $the_angle <P>";
    my $javascript_code = _generate_jtml(@region_details);
    
    print &RfamWWWConfig::header("Genome distribution: <font color=#FFFFcc>$species</font>", "", "", $javascript_code);
    #print "CODE: $javascript_code <P>";
    &RfamWWWConfig::chromosome($file, \@coord, \@all_coords, \@region_coord, \@region_label, \@region_details, \@circular_key, $unit);

     print "<table border=0 width=100%>";
    print "<tr><td valign=top>";
    print "<P> <embed NAME=TEST src=$RfamWWWConfig::WWW_root/temp/$dol width=\"500pt\" height=\"500pt\">";
    print "</td>";
  } else {
    ### NOT A CIRCULAR XSOME SO PRINT A NORMAL HEADER :-)
    print &RfamWWWConfig::header("Genome distribution: <font color=#FFFFcc>$species</font>");
         if ($got_contigs) {
	   my(@got_circle) = $rdb->query("select * from genome_entry where genome_acc = '$genome_acc'");
	   my $got_circ;
	   foreach (@got_circle) {
	     my($a, $b, $c, $d, $e) = @{$_};
	     $got_circ = $e;
	   }
    print qq (<SPAN class=normaltext>Click <A href=/cgi-bin/Rfam/genome_view.pl?genome_acc=$genome_acc&svg=on>here</A> to view a diagram of the distribution of these Rfam hits in the genome. [ <A hREF=http://www.w3.org/Graphics/SVG/SVG-Implementations>SVG viewer required</A> ]</SPAN> ) if ($got_circ);

  }
  }
#  print "UNIT: $store_max <BR>";

  print "<TD valign=middle>" if (@contigs);
 # print "ACC: $genome_acc <P>";
  print qq(<form name=blee  method=post enctype=\"multipart/form-data\"  action=/cgi-bin/Rfam/genome_view.pl>
	   
	   <input type=hidden name=genome_acc value=$genome_acc>
	   <table>

	   );
    if (@contigs) {
      my $print_unit = "100,000";
      $print_unit = "10,000" if ($store_max < 1000000);
      print "<TR><TD class=normaltext> The representation of the chromosome is split into contigs as defined by EMBL annotation.  The genomic positions of Rfam hits are shown by black ticks - those on the outside are on the forward strand, and those on the inside on the reverse strand.  Ticks on the scale bar are at <font color=#ff0000>" . $print_unit. "</font>kb intervals.  Mouse-over the contigs or entries in the table below to highlight classes of Rfam hits.  Click the family names to view details.</B><P></TD></TR>";

    }

  print qq (<tr><td><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=	#000070 CLASS=whitetableheader>Rfam family</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Type</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Count</TD><TD BGCOLOR=#000070 ALIGN=CENTER CLASS=whitetableheader >View</TD></TR>
	   
	   
	   
	   
	   
	  );
#  print "Content-type: text/html\n\n";
#  print "SVG: $svg <P>";

  if ($svg) {

    print "<Input type=hidden name=svg value=on>";
  }
 
  foreach (sort keys %species_results) {
   # print "key: $_ , val: " . $species_results{$_}. " <BR>";

    my (@res) = @{$species_results{$_}};
    
    my (@blee);
    foreach (@res) {
      my ($rfam_id, $rfam_acc, $sum, $type) = split(/~/, $_);
      my %tmp = ( 'acc' => $rfam_acc,
		  'id' => $rfam_id,
		  'sum' => $sum,
		  'type' => $type
		);
      push @blee, \%tmp;
    }
    @blee = sort { $a->{'sum'} <=> $b->{'sum'} } @blee;

    my @res = reverse @blee;
#ONMOUSEOVER=\"return enlarge_circle('$id')\" ONMOUSEOUT=\"shrink_circle('$id')\" 
    #my (%results) = %{$species_results{$_}};
    my $count = 1;
    foreach (@res) {
      my (%results) = %{$_};
      my $id = $results{id};
      my $acc = $results{acc};
      my $type =  $results{type};
    
      print "<TR><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=left><A href=getacc?$acc ";
     # print "SVG: $svg <P>";
      print " onMouseOver=\"return show_regions('$id')\";return true; onMouseOut=\"return hide_regions('$id')\";return true; " if ($svg);

      print ">$id</A></TD><TD class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=left>$type</TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>" . $results{sum} . "</TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  ALIGN=CENTER   NOWRAP valign=center align=left><input type=checkbox name=$count value=$id> ";

#      if (@contigs) {
#	print "<A href=\"javascript:void()\" ID=\"linkS\" STYLE=\"color:blue\" ONMOUSEOVER=\"return show_regions('$id')\" ONMOUSEOUT=\"return hide_regions('$id')\"  ONCLICK=\"return false;\">View regions in SVG</A>";
#      }
      

      print "</TD></TR>";
      $count++;
    }
    print "<input type=hidden name=max_count value=$count><input type=hidden name=auto_genome value=$auto_genome>\n";
  }
  
  
  
  print "</TABLE>";
  
  print "</TD></TR><TR><TR><TR><TR><TD align=right><input type=image SRC=$RfamWWWConfig::image_link/selected_regions.gif name=\"selected\" value=selected border=0><input type=image SRC=$RfamWWWConfig::image_link/all_regions.gif name=\"all\" value=all border=0></TABLE>";
  print "</TD></TR></TABLE>" if (@contigs);
  print "</form><P>";

}

print &RfamWWWConfig::footer();
&RfamWWWConfig::logs("GENOMEVIEW:$species");
#$galign -> view_all_figures( $zoom, 1, $acc);

#&RfamWWWConfig::logs("Species domain organisation in $name for family $family");

sub  _get_poly_lines {

  my($prev_angle, $new_angle, $outer_arc,$inner_arc, $page_offset) = @_;
  my @combine = poly_lines($prev_angle, $new_angle, $outer_arc, $page_offset);
  my @tmp_points = reverse poly_lines($prev_angle, $new_angle, $inner_arc, $page_offset);

  foreach (@tmp_points) {
    push @combine, $_;
  }
  my $merge_coords = join("," , @combine);
  return $merge_coords;
}

sub poly_lines {

  my ($start_angle, $end_angle, $length, $page_offset) = @_;
  my (@points);

  while ($start_angle <= $end_angle) {
   # print "START: $start_angle <BR>";
    my $rad = ($start_angle/ 180) * 3.14;
    my $opposite = sin($rad);
    my $adjacent = cos($rad);
    $adjacent = $adjacent * $length;
    $opposite = $opposite * $length;
    #print "ADGACENT = $adjacent, OPP: $opposite  $the_angle<BR>";
    my $y = $page_offset - $adjacent;
    my $x = $page_offset + $opposite;
    push @points, $x . " " . $y;
    $start_angle++;
  }
  return @points;
}


sub _generate_jtml {
  my(@region_details) = @_;

  my ($javascript);
   $javascript .= "var storeSequences = new Array();\n";

  my $for_count = 0;
  foreach (@region_details) {
    my($label, $version, $newpoints, $existingpoints, $rfam_id) = split(/~/, $_);
     $javascript .= "storeSequences[$for_count] = new addSeq(\"$label\", \"$version\", \"$newpoints\", \"$existingpoints\", \"$rfam_id\");\n";
    $for_count++;
  }

  $javascript .= qq (

	    
	    function addSeq (label, version, newpoints, existingpoints, rfamid) {
	      this.label = label;
	      this.version = version;
	      this.newpoints = newpoints;
	      this.existingpoints = existingpoints;
	      this.rfamid = rfamid;
	    }
	    
	    function show_regions (id) {
	      var svgdoc = document.TEST.getSVGDocument();
	      
	      for (var i=0; i < storeSequences.length; i++) {
		var match_str = storeSequences[i].rfamid;
		
		var match =  match_str.search(id);
		//blee = match_str + " " +  regexp;
		if (match >= 0) {
		  // blee = "WOW " + match + " " + match_str;
		  
		  var line = svgdoc.getElementById(storeSequences[i].label);
		 // line.setAttribute("points", storeSequences[i].newpoints);
		  line.setAttribute("style", "fill:red; stroke:red;");
		}
	      }
	      
	    }
	    
	   
	    function hide_regions (id) {
	      var svgdoc = document.TEST.getSVGDocument();
	      
	      for (var i=0; i < storeSequences.length; i++) {
		var match_str = storeSequences[i].rfamid;
		
		var match =  match_str.search(id);
		//blee = match_str + " " +  regexp;
		if (match >= 0) {
		  // blee = "WOW " + match + " " + match_str;
		  
		  var line = svgdoc.getElementById(storeSequences[i].label);
		 // line.setAttribute("points", storeSequences[i].existingpoints);
		  line.setAttribute("style", "fill:black; stroke:black;");
		}
	      }
	      
	      
	    }
	
	   );
  
  return $javascript;
}
