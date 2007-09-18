#!/software/bin/perl



#
# This script makes the browse
# pages from the family index
# file, and the diff file

#
# The family index is made by shatterflat.pl
# from the flat files.
#

use Getopt::Long;

&GetOptions('colour=s' => \$pfam_colour);


########### Constants:

$scop_ref = 'http://www.biochem.ucl.ac.uk/bsm/pdbsum/$scop/main.html';
if (not $pfam_colour) {
	$pfam_colour = '$RFAMCOLOUR'; ## external
	
    #$pfam_colour = '#999ACC'; 
    #$pfam_colour = '#C3DFF8'; ## internal 
}

########### Parameters:

$familyindex = shift;
#$diff = shift;
#$genomics = shift;

#if (not defined($familyindex) or not defined($diff)) {
#    die "Usage: makebrowse <family.index file> <diff file>";
#}

if (not defined($familyindex) ) {
    die "Usage: makebrowse <family.index file> <diff file>";
}


open (INDEX, $familyindex) or die "Could not open the given family index $familyindex";
#open (DIFF, $diff) or die "Could not open the given diff file $diff";


########### Work


while( <INDEX>) {
    ($acc,$name,$type,$seed,$full,$avl,$avp,$scop,$desc) = split(/\^/,$_);

    $acc{$name} = $acc;
    $seed{$name} = $seed;
    $type{$name} = $type;
    $full{$name} = $full;
    $avl{$name} = $avl;
    $avp{$name} = $avp;
    $scop{$name} = $scop;
    $desc{$name} = $desc;
    push(@names,$name);
}



#while(<DIFF>) {
#    /^FI\s+\S+\s+(\S+)\s+(\S+)/ && do {
#	$fam_id = $1;
#	$fam_stat = $2;
#	if ($fam_stat =~ /^NEW/) {
#	    $status{$fam_id} = "New ";
#	    push @new_names, $fam_id;
#	}
#	elsif ($fam_stat =~ /^CHANGE/) {
#	    $status{$fam_id} = "Changed ";
#	}
#    };
#}

@names = sort { return lc($a) cmp lc($b) } @names;
@new_names = sort { return lc($a) cmp lc($b) } @new_names;
@twenty_names = (sort { $full{$b} <=> $full{$a} } @names)[0..19];
#print "TWENT: @twenty_names \n";
######################################################
#  first files is numbers
######################################################


#open(OUT,">browse/number.th");
#print OUT "<!-- SET  \$title=\"Browse Rfam - numbers\" -->\n<!-- ADDHEADER -->\n";
#&make_index_bar(\*OUT, "", "", "number.html");
#&make_tsearch_bar(\*OUT);

####print OUT "<table border bgcolor='$pfam_colour'>\n<tr><th>Name</th><th>No. seed</th><th>No. full</th><th>Average Length</th><th>Average %id</th><th>Structure</th><th>Status</th><th>Description</th></tr>\n";
#print OUT "<table border=1 CLASS=normaltext CELLPADDING=5  CELLSPACING=0 bgcolor='$pfam_colour'>\n<tr BGCOLOR=#000070><th CLASS=columnheader NOWRAP>Name</th><th CLASS=columnheader NOWRAP>No. seed</th><th CLASS=columnheader NOWRAP>No. full</th><th CLASS=columnheader NOWRAP>Average Length</th><th CLASS=columnheader NOWRAP>Average %id</th><th CLASS=columnheader NOWRAP>Structure</th><th CLASS=columnheader NOWRAP>Status</th><th CLASS=columnheader NOWRAP>Description</th></tr>\n";
#$prev = "number";


#foreach $key ( @names ) {
#    if( ! ($key =~ /^\d/) ) {
#	$l = substr($key,0,1);
#	$l =~ tr/[a-z]/[A-Z]/;
#	#print STDERR "$l vs $prev\n";
#	if( $l ne $prev ) {
#	    print OUT "</table>\n";
#	    &make_index_bar(\*OUT, "", "", "$prev.html");
#	    print OUT "\n<!-- ADDFOOTER -->\n";
#	    close(OUT);
#	    $prev = $l;
#	    open(OUT,">browse/$prev.th");
#	    print OUT "<!-- SET  \$title=\"Browse Rfam - $prev\" -->\n<!-- ADDHEADER -->\n";
#	    &make_index_bar(\*OUT, "", "", "$prev.html");
#	    &make_tsearch_bar(\*OUT);
	    
####	    print OUT "<table border bgcolor='$pfam_colour'>\n<tr><th>Name</th><th>No. seed</th><th>No. full</th><th>Average Length</th><th>Average %id</th><th>Structure</th><th>Status</th><th>Description</th></tr>\n";
#print OUT "<table border=1  CLASS=normaltext CELLPADDING=5  CELLSPACING=0 bgcolor='$pfam_colour'>\n<tr BGCOLOR=#000070><th CLASS=columnheader NOWRAP>Name</th><th CLASS=columnheader NOWRAP>No. seed</th><th CLASS=columnheader NOWRAP>No. full</th><th CLASS=columnheader NOWRAP>Average Length</th><th CLASS=columnheader NOWRAP>Average %id</th><th CLASS=columnheader NOWRAP>Structure</th><th CLASS=columnheader NOWRAP>Status</th><th CLASS=columnheader NOWRAP>Description</th></tr>\n";
#	}
#    }
#    $acc = $acc{$key};
#    $seed = $seed{$key};
#    $full = $full{$key};
#    $avl = $avl{$key};
#    $avp = $avp{$key};
#    $desc  = $desc{$key};
#    $status = $status{$key}?$status{$key}:"&nbsp;";
#    if ($scop{$key}) {
#	$scop = $scop{$key};
#	eval("\$scoplink = \"$scop_ref\"");
#	$scoplink = "<a href=\"$scoplink\">$scop</a>";
#    }
#    else {
#	$scoplink = "&nbsp;";
#    }
####    print OUT "<tr><td><a href=\"\$GETACC?$acc\">$key</a></td><td>$seed</td><td>$full</td><td>$avl</td><td>$avp</td><td>$scoplink</td><td>$status</td><td>$desc</td></tr>\n";

#print OUT "<tr><td CLASS=normaltext NOWRAP><a href=\"\$GETACC?$acc\">$key</a></td><td CLASS=normaltext  NOWRAP>$seed</td><td CLASS=normaltext NOWRAP >$full</td><td CLASS=normaltext  NOWRAP>$avl</td><td CLASS=normaltext  NOWRAP>$avp</td><td CLASS=normaltext  NOWRAP>$scoplink</td><td CLASS=normaltext  NOWRAP>$status</td><td CLASS=normaltext  NOWRAP>$desc</td></tr>\n";

#}

#print OUT "</table>\n";
#&make_index_bar(\*OUT, "", "", "$prev.html");
#print OUT "\n<!-- ADDFOOTER -->\n";
#close(OUT);


######################################################
# first, lets make the new families page
######################################################


#if ( @new_names ) {
#    open (OUT, ">browse/new.th");
#    print OUT "<!-- SET  \$title=\"Browse Rfam - new families\" -->\n<!-- ADDHEADER -->\n";
#    &make_index_bar(\*OUT, "", "", "new.html");
#    &make_tsearch_bar(\*OUT);


#print OUT "<table border=1 CLASS=normaltext CELLPADDING=5  CELLSPACING=0 bgcolor='$pfam_colour'>\n<tr BGCOLOR=#000070><th CLASS=columnheader NOWRAP>Name</th><th CLASS=columnheader NOWRAP>No. seed</th><th CLASS=columnheader NOWRAP>No. full</th><th CLASS=columnheader NOWRAP>Average Length</th><th CLASS=columnheader NOWRAP>Average %id</th><th CLASS=columnheader NOWRAP>Structure</th><th CLASS=columnheader NOWRAP>Status</th><th CLASS=columnheader NOWRAP>Description</th></tr>\n";

##print OUT "<table border=1  CELLPADDING=5  CELLSPACING=0 bgcolor='$pfam_colour'>\n<tr BGCOLOR=#000070><th CLASS=columnheader>Name</th><th CLASS=columnheader>No. seed</th><th CLASS=columnheader>No. full</th><th CLASS=columnheader>Average Length</th><th CLASS=columnheader>Average %id</th><th CLASS=columnheader>Structure</th><th CLASS=columnheader>Status</th><th CLASS=columnheader>Description</th></tr>\n";
####    print OUT "<table border bgcolor='$pfam_colour'>\n<tr><th>Name</th><th>No. seed</th><th>No. full</th><th>Average Length</th><th>Average %id</th><th>Structure</th><th>Status</th><th>Description</th></tr>\n";
    
#    foreach $key ( @new_names ) {    
#	$acc = $acc{$key};
#	$seed = $seed{$key};
#	$full = $full{$key};
#	$avl = $avl{$key};
#	$avp = $avp{$key};
#	$desc  = $desc{$key};
#	$status = $status{$key}?$status{$key}:"&nbsp;";
#	if ($scop{$key}) {
#	    $scop = $scop{$key};
#	    eval("\$scoplink = \"$scop_ref\"");
#	    $scoplink = "<a href=\"$scoplink\">$scop</a>";
#	}
#	else {
#	    $scoplink = "&nbsp;";
#	}
	
###	print OUT "<tr><td><a href=\"\$GETACC?$acc\">$key</a></td><td>$seed</td><td>$full</td><td>$avl</td><td>$avp</td><td>$scoplink</td><td>$status</td><td>$desc</td></tr>\n";

#	    print OUT "<tr><td CLASS=normaltext  NOWRAP><a href=\"\$GETACC?$acc\">$key</a></td><td CLASS=normaltext NOWRAP >$seed</td><td CLASS=normaltext  NOWRAP>$full</td><td CLASS=normaltext  NOWRAP>$avl</td><td CLASS=normaltext  NOWRAP>$avp</td><td CLASS=normaltext  NOWRAP>$scoplink</td><td CLASS=normaltext  NOWRAP>$status</td><td CLASS=normaltext  NOWRAP>$desc</td></tr>\n";
#    }
#    print OUT "</table>\n";
#&make_index_bar(\*OUT, "", "", "new.html");
#    print OUT "\n<!-- ADDFOOTER -->\n";
#    close OUT;
#}

######################################################
#    Next, the top 20 families
######################################################


system "cp browse/index.th browse/old_index.th" and die;

open (OUT, ">browse/index.th");
print OUT "<!-- SET  \$title=\"Browse by family name     &nbsp;&nbsp;&nbsp;&nbsp;<A href=index.shtml><font color=#FFFFCC>[Browse by type]</font></A>\" -->\n<!-- ADDHEADER -->\n";
&make_index_bar(\*OUT, "", "",  "top_twenty.html");
&make_tsearch_bar(\*OUT);

print OUT "<table border=1  CLASS=normaltext CELLPADDING=5  CELLSPACING=0 bgcolor='$pfam_colour'>\n<tr BGCOLOR=#000070><th CLASS=columnheader NOWRAP>Name</th><th CLASS=columnheader NOWRAP>Type</th><th CLASS=columnheader NOWRAP>No. seed</th><th CLASS=columnheader NOWRAP>No. full</th><th CLASS=columnheader NOWRAP>Average Length</th><th CLASS=columnheader NOWRAP>Average %id</th><th CLASS=columnheader NOWRAP>Description</th></tr>\n";

#foreach $key ( @twenty_names ) { 

   foreach $key ( @names ) {
    $acc = $acc{$key};
    $seed = $seed{$key};
    $full = $full{$key};
    $type = $type{$key};
    $avl = $avl{$key};
    $avp = $avp{$key};
    $desc  = $desc{$key};
    $status = $status{$key}?$status{$key}:"&nbsp;";
    if ($scop{$key}) {
	$scop = $scop{$key};
	eval("\$scoplink = \"$scop_ref\"");
	$scoplink = "<a href=\"$scoplink\">$scop</a>";
    }
    else {
	$scoplink = "&nbsp;";
    }

   print OUT "<tr><td CLASS=normaltext  NOWRAP><a href=\"\$GETACC?$acc\">$key</a></td><td CLASS=normaltext  NOWRAP>$type</td><td CLASS=normaltext  NOWRAP>$seed</td><td CLASS=normaltext  NOWRAP>$full</td><td CLASS=normaltext NOWRAP >$avl</td><td CLASS=normaltext NOWRAP >$avp</td><td CLASS=normaltext NOWRAP >$desc</td></tr>\n";

}
print OUT "</table>\n";
&make_index_bar(\*OUT, "", "",  "top_twenty.html");
print OUT "\n<!-- ADDFOOTER -->\n";
close OUT;


######################################################
#    Next the top hundred families with no structure !
######################################################


#if ($genomics) {
#  my $start = 0;
#  my $end = 99;
  
#  my(%hash_count);
#  my $length = ( @names );
#  my @sort_name = (sort { $full{$b} <=> $full{$a} } @names)[0..$length];
  
  
#  my(@no_scop_names);
  
#  foreach my $name (@sort_name) {
    
#    if($scop{$name}) {
      
#    } else {
#      push @no_scop_names, $name
#    }
    
#  }
  
  
#  my @top_hundred = (sort { $full{$b} <=> $full{$a} } @no_scop_names)[0..99];
  
#  my $filename = "browse/no_structures" . $count . ".th";
#  open (OUT, ">browse/no_structures.th");
  
  
#  print OUT "<!-- SET  \$title=\"Browse Rfam - No 3-d  \" -->\n<!-- ADDHEADER -->\n";
#  &make_index_bar(\*OUT, "", "", "no_structures.html");
#  &make_tsearch_bar(\*OUT);
#  print OUT "<P CLASS=normaltext>TM = Transmembrane region predicted with TMHMM NG = Non-Globular region calculated with SEG </P><P>";
  
#  print OUT "<table border=1 CLASS=normaltext CELLPADDING=5  CELLSPACING=0 bgcolor='$pfam_colour'>\n<tr BGCOLOR=#000070><th CLASS=columnheader NOWRAP>Name</th><th CLASS=columnheader NOWRAP>No. seed</th><th CLASS=columnheader NOWRAP>No. full</th><th CLASS=columnheader NOWRAP>Average Length</th><th CLASS=columnheader NOWRAP>Average %id</th><th CLASS=columnheader NOWRAP>Status</th><th CLASS=columnheader NOWRAP>Feature</th><th CLASS=columnheader NOWRAP>Description</th></tr>\n";
  
#  ###print OUT "<table border bgcolor='$pfam_colour'>\n<tr><th>Name</th><th>No. seed</th><th>No. full</th><th>Average Length</th><th>Average %id</th><th>Status</th><th>Feature</TH><th>Description</th></tr>\n";
  
  
#  foreach $key ( @top_hundred ) {    
#    $acc = $acc{$key};
#    $seed = $seed{$key};
#    $full = $full{$key};
#    $avl = $avl{$key};
#    $avp = $avp{$key};
#    $desc  = $desc{$key};
#    $status = $status{$key}?$status{$key}:"&nbsp;";
    
#    ### print OUT "<tr><td><a href=\"\$GETACC?$acc\">$key</a></td><td>$seed</td><td>$full</td><td>$avl</td><td>$avp</td><td>$status</td> ";
    
#    print OUT "<tr><td CLASS=normaltext  NOWRAP><a href=\"\$GETACC?$acc\">$key</a></td><td CLASS=normaltext NOWRAP >$seed</td><td CLASS=normaltext NOWRAP >$full</td><td CLASS=normaltext  NOWRAP>$avl</td><td CLASS=normaltext  NOWRAP>$avp</td><td CLASS=normaltext  NOWRAP>$status</td>";
    
    
    
#    open(_FILE, "$genomics") or die "cant open the file $!\n";
#    while(<_FILE>) {
#      s/\s+//g;
#      my($struc_acc, $struc_id, $struc_tm, $struc_seg) = split(/;/, $_);
#      if ($struc_acc =~ $acc) {
#	if( ($struc_tm =~ /[a-z]/i) || ($struc_seg =~ /[a-z]/i) ) {
#	  $struc_seg = "NG" if ($struc_seg =~ /SEG/i);
#	  print OUT "<td CLASS=normaltext NOWRAP>$struc_tm $struc_seg</td>";
#	  ##	print OUT "<td>$struc_tm $struc_seg</td>";
#	} else {
#	  print OUT "<td CLASS=normaltext NOWRAP>&nbsp;</td>";
#	  ##	print OUT "<td>&nbsp;</td>";
#	}
	
	
	
	
#      }
      
      
#    }
    
#    close(_FILE) or die "CANNA CLOSE FILE $! \n";
#    print OUT " <td CLASS=normaltext  NOWRAP>$desc</td></tr>\n";
#    ## print OUT "<td>$desc</td></tr>\n";
    
#  }
#  print OUT "</table>\n";
#  &make_index_bar(\*OUT, "", "", "no_structures.html");
#  print OUT "\n<!-- ADDFOOTER -->\n";
#  close OUT;
#} else {
#  print "TM/SEG file missing, normally Rfam/data/genomics_struc.dat Redo makebrowse when this file is available \n";
#}


###############################################################
#   get all families with no structure & avg length < 200 !
###############################################################


#my $start = 0;
#my $end = 99;

#my(%hash_count);
#my $length = ( @names );

#my @sort_name = (sort { $full{$b} <=> $full{$a} } @names)[0..$length];


#my(@no_scop_names);

#foreach my $name (@sort_name) {

#  if($scop{$name}) {
    
#  } else {
#    push @no_scop_names, $name
#  }

#}

#my $new_length = (@no_scop_names);
#my @top_hundred = (sort { $avl{$b} <=> $avl{$a} } @no_scop_names) [0..$new_length];

#my @small_prots = reverse (@top_hundred);



#open (OUT, ">browse/small_no_structures.th");


#print OUT "<!-- SET  \$title=\"Browse Rfam - small families No 3-d  \" -->\n<!-- ADDHEADER -->\n";
#&make_index_bar(\*OUT);
#&make_tsearch_bar(\*OUT);
#print OUT "<P CLASS=normaltext>TM = Transmembrane region predicted with TMHMM NG = Non-Globular region calculated with SEG </P> <P>";

#print OUT "<table border=1 class=normaltext CELLPADDING=5  CELLSPACING=0 bgcolor='$pfam_colour'>\n<tr BGCOLOR=#000070><th CLASS=columnheader NOWRAP>Name</th><th CLASS=columnheader NOWRAP>No. seed</th><th CLASS=columnheader NOWRAP>No. full</th><th CLASS=columnheader NOWRAP>Average Length</th><th CLASS=columnheader NOWRAP>Average %id</th><th CLASS=columnheader NOWRAP>Status</th><th CLASS=columnheader NOWRAP>Feature</th><th CLASS=columnheader NOWRAP>Description</th></tr>\n";

###print OUT "<table border  bgcolor='$pfam_colour'>\n<tr><th>Name</th><th>No. seed</th><th>No. full</th><th>Average Length</th><th>Average %id</th><th>Status</th><th>Feature</TH><th>Description</th></tr>\n";

#foreach $key ( @small_prots ) {    
#  $acc = $acc{$key};
#  $seed = $seed{$key};
#  $full = $full{$key};
#  $avl = $avl{$key};
#  $avp = $avp{$key};
#  $desc  = $desc{$key};
#  $status = $status{$key}?$status{$key}:"&nbsp;";
  
# # print "ACC: $acc, $key \n";
##  if ($acc =~ /pf/i) {
##  print "got acc \n";
#  if ( ($avl < 180) && ( ($desc !~ /motif/i) && ($desc !~ /repeat/i) && ($desc !~ /ribosomal/i) ) ){

# my $out =  "<tr><td CLASS=normaltext  NOWRAP><a href=\"\$GETACC?$acc\">$key</a></td><td CLASS=normaltext NOWRAP >$seed</td><td CLASS=normaltext NOWRAP >$full</td><td CLASS=normaltext  NOWRAP>$avl</td><td CLASS=normaltext NOWRAP >$avp</td><td CLASS=normaltext  NOWRAP>$status</td>";

###  my $out =  "<tr><td><a href=\"\$GETACC?$acc\">$key</a></td><td>$seed</td><td>$full</td><td>$avl</td><td>$avp</td><td>$status</td> ";
  
#  open(_FILE, "/nfs/disk65/mm1/pfam/scripts/pfamrdb/mhairi/genomics/genomics_struc.dat") or die "cant open the file !\n";
#  while(<_FILE>) {
#    s/\s+//g;
#    my($struc_acc, $struc_id, $struc_tm, $struc_seg) = split(/;/, $_);
    
#    if ($struc_acc =~ $acc) {
#      if( ($struc_tm =~ /[a-z]/i) || ($struc_seg =~ /[a-z]/i) ) {
#	$struc_seg = "NG" if ($struc_seg =~ /SEG/i);
##	print OUT "<td>$struc_tm $struc_seg</td>";
#      } else {
#	print OUT "$out <td  CLASS=normaltext>&nbsp;</td>";
#	 print OUT "<td CLASS=normaltext>$desc</td></tr>\n";
#	##print OUT "<td>$desc</td></tr>\n";
#      } #/ end IF
      
      
      
      
#    } #/ end IF
    
    
#  } #/end WHILE
  
#  close(_FILE);
  
 

#} # end IF length < 200

##}
  
#} #/ end foreach
#print OUT "</table>\n";
#&make_index_bar(\*OUT);
#print OUT "\n<!-- ADDFOOTER -->\n";
#close OUT;

 






######################################################
#  lastly, the main browse page
######################################################


#open (OUT, ">browse.th");
#print OUT "<!-- SET  \$title=\"Browse Families\" -->\n<!-- ADDHEADER -->\n";
#&make_index_bar(\*OUT, "browse/");
#&make_tsearch_bar(\*OUT);
#print OUT "\n<!-- ADDFOOTER -->\n";
#close OUT;


sub make_index_bar {
#  my($fh, $dir, $new_names_text, $page) = @_;
  
#  #  my $fh = shift;
#  #    my $dir = shift;
#  #    my $new_names_text = "";
#  #    my $page = shift;
#  #    print "FH: $fh, dir: $dir, new_names_text: $new_names_text , PAGE: $page \n";
#  my @all_letters = ("new.html~New", "number.html~Numbers" , "A.html~A" ,  "B.html~B" , "C.html~C" ,"D.html~D" , "E.html~E" ,"F.html~F" ,"G.html~G" ,"H.html~H" ,  "I.html~I" ,  "J.html~J" , "K.html~K" ,"L.html~L" , "M.html~M" ,"N.html~N" ,"O.html~O" ,"P.html~P" , "Q.html~Q" ,  "R.html~R" , "S.html~S" , "T.html~T" ,"U.html~U" ,"V.html~V" ,"W.html~W" , "X.html~X" ,  "Y.html~Y" , "Z.html~Z" ,"top_twenty.html~Top-twenty" , "no_structures.html~No 3-d" ) ;
  
#  # if (@new_names) {
#  #	$new_names_text = "<a href=\"${dir}new.html\">New</a> ";
#  #	push @all_letters "new.html~New";
#  #   } 
  
  
  
  
#  print $fh "\n<center><pre>";
  
#  foreach (@all_letters) {
#    my($href, $title) = split(/~/, $_);
#    #      print "href: $href, page: $page \n";
#    if ($href eq $page) {
#      #	print "equals !! \n";
#      print $fh " <font color=#E9D000><B>$title</B></font>";
#    } else {
#      print $fh " <a href=\"${dir}$href\">$title</a>";
#    }
    
    
#  }
#  #    print $fh "$new_names_text<a href=\"${dir}number.html\">Numbers</a> <a href=\"${dir}A.html\">A</a> <a href=\"${dir}B.html\">B</a> <a href=\"${dir}C.html\">C</a> <a href=\"${dir}D.html\">D</a> <a href=\"${dir}E.html\">E</a> <a href=\"${dir}F.html\">F</a> <a href=\"${dir}G.html\">G</a> <a href=\"${dir}H.html\">H</a> <a href=\"${dir}I.html\">I</a> <a href=\"${dir}J.html\">J</a> <a href=\"${dir}K.html\">K</a> <a href=\"${dir}L.html\">L</a> <a href=\"${dir}M.html\">M</a> <a href=\"${dir}N.html\">N</a> <a href=\"${dir}O.html\">O</a> <a href=\"${dir}P.html\">P</a> <a href=\"${dir}Q.html\">Q</a> <a href=\"${dir}R.html\">R</a> <a href=\"${dir}S.html\">S</a> <a href=\"${dir}T.html\">T</a> <a href=\"${dir}U.html\">U</a> <a href=\"${dir}V.html\">V</a> <a href=\"${dir}W.html\">W</a> <a href=\"${dir}X.html\">X</a> <a href=\"${dir}Y.html\">Y</a> <a href=\"${dir}Z.html\">Z</a> <a href=\"${dir}top_twenty.html\"><font color=#E9D000><B>Top-twenty</B></font></a> <a href=\"${dir}no_structures.html\">No 3-d</a> \n";
  
#  print $fh "</pre></center>\n";


}


sub make_tsearch_bar {
    my $fh = shift;

    print $fh "<p CLASS=normaltext>If the family you are interested in does not appear here, then try searching Rfam for it.";
    print $fh "The name of the family may have changed or you may know the family by a different name than Rfam.<p>\n";
    print $fh "<form method=get action=\$TSEARCH>\n";
    print $fh "<B CLASS=normaltext> Enter query word(s): </B><input type=text name=\"terms\">\n";
    print $fh "<input type=submit value=\"Search\"><p>\n";
    print $fh "</form>\n";
}

