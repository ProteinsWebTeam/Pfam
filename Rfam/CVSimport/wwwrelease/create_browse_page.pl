#!/usr/local/bin/perl
# Script to create Rfam browse page for web-release - 21/03/05 sm3

use strict;
use Rfam;
use RfamRCS;
use RfamQC;
my @accs;
my %tpline;
my $db = Rfam::default_db();
my $maxdepth =-1;
my %seen =();
my %menus;
my @output;
# Get all families + TP lines
foreach my $acc ( $db->get_allacc() ) {
chomp $acc;
my $id = $db->acc2id($acc);
chomp $id;
my $i=-1;
	push (@accs, $id);
	my $entry = $db->get_Entry_by_acc($acc);
	my @TPline = split (/;/, $entry->entry_type());
	foreach my $element (@TPline){
	#Need to reformat a few as Javascript doesn't seem to like '-'s and '/'s
	chomp $element;
		if($element eq "Cis-reg"){
		$element = "Cis_reg";
		}
		if ($element eq "H/ACA-box"){
		$element = "HACA_box";
		}
		if ($element eq "C/D-box"){
		$element = "CD_box";
		}
	$i++;
	push (@{$tpline{$id}}, $element);
	}
	if ($i > $maxdepth){
	$maxdepth = $i;
	}
}
my $count_total = 0;
&print_header;
my $pass=1;

#Go through and initialise Javascript menus/submenus
while ($count_total <= $maxdepth){
	foreach my $new (sort keys %tpline){
	chomp $new;
		if($tpline{$new}->[$count_total]){
			if (!$tpline{$new}->[$count_total+1]){
				if ($count_total == 0){
					unless ($seen{$tpline{$new}->[$count_total]}){
					push (@{$menus{$count_total}}, "menu.addItem(\"<font color=#000000><B>$tpline{$new}->[$count_total]</B></font>\");\n");
					push (@{$menus{$count_total}}, "var $tpline{$new}->[$count_total] = null;\n");
					push (@{$menus{$count_total}}, "$tpline{$new}->[$count_total] = new MTMenu();\n");
					$seen{$tpline{$new}->[$count_total]}++;
					push (@{$menus{$count_total}}, "menu.makeLastSubmenu($tpline{$new}->[$count_total]);\n");
					next;
					}	
				}
				
				else {
					unless ($seen{$tpline{$new}->[$count_total]}){
					push (@{$menus{$count_total}}, "$tpline{$new}->[$count_total-1].addItem(\"<font color=#000000><B>$tpline{$new}->[$count_total]</B></font>\");\n");
					push (@{$menus{$count_total}}, "var $tpline{$new}->[$count_total] = null;\n");
					push (@{$menus{$count_total}}, "$tpline{$new}->[$count_total] = new MTMenu();\n");
					$seen{$tpline{$new}->[$count_total]}++;
					push (@{$menus{$count_total}}, "$tpline{$new}->[$count_total-1].makeLastSubmenu($tpline{$new}->[$count_total]);\n");
					next;
					}
				}
				
			}
			
		}	
	}
	$count_total++;
	
}
my $count =0;
# Have to do this again as misses some the first time around if a submenu goes directly to further submenus rather than containing entries
# Eg. Gene; snRNA; guide goes to HACA_box and CD_box and contains no entries at present - probably a better way to do this though.
while ($count <= $maxdepth){
	foreach my $new (sort keys %tpline){
	my $id = $new;
	my $acc = $db->id2acc($id);
	chomp $acc;
	my $entry = $db->get_Entry_by_acc($acc);
	my $desc = $entry->description();
		if ($tpline{$new}->[$count]){
			if (!$seen{$tpline{$new}->[$count]}){
			push (@{$menus{$count}}, "$tpline{$new}->[$count-1].addItem(\"<font color=#000000><B>$tpline{$new}->[$count]</B></font>\");\n");
			push (@{$menus{$count}}, "var $tpline{$new}->[$count] = null;\n");
			push (@{$menus{$count}}, "$tpline{$new}->[$count] = new MTMenu();\n");
			$seen{$tpline{$new}->[$count]}++;
			push (@{$menus{$count}}, "$tpline{$new}->[$count-1].makeLastSubmenu($tpline{$new}->[$count]);\n");
			}
			if (!$tpline{$new}->[$count+1]){
			push (@output, "$tpline{$new}->[$count].addItem(\"<B>$id</B> - $desc\", \"/cgi-bin/Rfam/getacc?$acc\", \"_top\");\n");
			}
		}
	}
	$count++;
}
# All menus are stored in this hash - the Javascript seems to be picky about the order these are declared. This sorts them by tree depth + prints in that order
my $outcount = 0;
while ($outcount <= $maxdepth){
	foreach my $out (sort keys %menus){
	chomp $out;
		if ($out == $outcount){
			foreach my $thing (@{$menus{$outcount}}){
			print $thing;
			}
		}
	}
$outcount++;
}

# Now can print the individual entries
foreach my $line (@output){
print $line;
}

&print_footer;
sub print_header{
print <<EOF;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
<title>[insert your title here]</title>

<script type="text/javascript">
// Framebuster script to relocate browser when MSIE bookmarks this
// page instead of the parent frameset.  Set variable relocateURL to
// the index document of your website (relative URLs are ok):
//var relocateURL = "/";
var relocateURL = "http://www.sanger.ac.uk";

if(parent.frames.length == 0) {
  if(document.images) {
    location.replace(relocateURL);
  } else {
    location = relocateURL;
  }
}
</script>

<script type="text/javascript" src="mtmcode.js">
</script>

<script type="text/javascript">
// Morten's JavaScript Tree Menu
// version 2.3.2-macfriendly, dated 2002-06-10
// http://www.treemenu.com/

// Copyright (c) 2001-2002, Morten Wang & contributors
// All rights reserved.

// This software is released under the BSD License which should accompany
// it in the file "COPYING".  If you do not have this file you can access
// the license through the WWW at http://www.treemenu.com/license.txt

// Nearly all user-configurable options are set to their default values.
// Have a look at the section "Setting options" in the installation guide
// for description of each option and their possible values.

MTMDefaultTarget = "text";

/******************************************************************************
* User-configurable list of icons.                                            *
******************************************************************************/

var MTMIconList = null;
MTMIconList = new IconList();
MTMIconList.addIcon(new MTMIcon("menu_link_external.gif", "http://", "pre"));
MTMIconList.addIcon(new MTMIcon("menu_link_pdf.gif", ".pdf", "post"));

/******************************************************************************
* User-configurable menu.                                                     *
******************************************************************************/

// Main menu.
var menu = null;
menu = new MTMenu();
EOF
}

sub print_footer{
print <<EOF;
</script>

</head>
/<body onload="MTMStartMenu(true)" bgcolor="#000033" text="#ffffcc" link="yellow" vlink="lime" alink="red">

</body>
</html>
EOF
exit;
}
