
#
# Perl Module for GIFAlign
#
# Cared for by Ewan Birney <birney@sanger.ac.uk>
#
#Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE

package MEMAlign;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;

use RfamWWWConfig;
use Paging;
#use AnnSeq2WWW;

use Rfam::RfamAlign;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance.
#

@ISA = ('Export',  'Rfam::RfamAlign' );


sub view_all_figures {
 
    my ($self, $header, $verbose, $acc,  $dom_name, $all_params, %new_select_order) = @_;
   my ($disname,$seq,$name, $annseq);
    my $entry;
#    my $number;    

    my ($p,$h,$f,$famname);
  
#    # make a new paging module
  
#    $famname = $self->id();
    $p = new Paging;
    $p->dir($RfamWWWConfig::tempdir);
    $p->chunk(500);
    $p->maxkbytes(2000); #2 Mbytes in the cache system
    $p->print(&RfamWWWConfig::header("$header", $dom_name, $acc));
    $h = &RfamWWWConfig::header("$header head", $dom_name, $acc);
    $f = "<P><P>" . &RfamWWWConfig::footer();
 #   $p->print(&RfamWWWConfig::header("$header", $dom_name, $acc));
    $p->print("<CENTER><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD></TR>");

    $p->header("$h <p>$header (cont...)  <P><CENTER><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD></TR>");
#$p->print("</TABLE></CENTER><P>$f");
    $p->footer("</TABLE></CENTER><P><hr>Next page <a href=\\\"$RfamWWWConfig::image_temp/\$next_file\\\">here</a><p>\Q$f\E");
    
#    $self->set_displayname_count(); # Count to avoid multiple displays

#    # $paging should be passed into the following construtor if used
#    my $pictures = AnnSeq2WWW->new( undef );
 
#	# count the number of times the key is printed
#    my($key_count) = 1;   
#    my $reselect_buttons = 1;
#    $reselect_buttons = 0 if(!$all_params);


   # $p->print("<P><CENTER><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD></TR>"
#);

    foreach $seq ( $self->eachSeq() ) {#
#	$name = $seq->id();
#	my $acc = $seq->acc();

#	print "ACC: $acc <P>";
#	print "<A HREF=$link>$name</A><BR>";
#	next if ($name eq "SS_cons");
#	next if ($name eq "SA_cons");
	$disname = $self->get_displayname($seq->get_nse());
#	print "DISNAME: $disname <P>";

	my ($acc, $start, $end);
	if ($disname =~ /(\S+)\/(\d+)-(\d+)/) {

	  $acc = $1;
	  $start = $2;
	  $end = $3;
	  my $link = $RfamWWWConfig::srsserver;
	  $link =~ s/ACC/$acc/;
	  $p->print ("<TR><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour ><A HREF=$link>$acc</A></TD>");
	  $p->print("<TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>$start</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>$end</TD></TR>");
	  $p->break;
#	  print "$acc : $start : $end <BR>";
	}

#	$disname=~ /_(\d+)$/;
#        $number= $1; 
		   
#	# note that print statements in the following code should be replaced with
#	# $paging->print if the $paging is used

#	if ($number==1) { # Only show protein the first time

#	  my $string;
#	     $string .="<p>";
#	    if ($key_count =~ /5/) {
#		$pictures->print_key();
#		$key_count = 1;
#	    } else {
#		$key_count++;
#	    }
	    

#	    $string .= "<A HREF=$PfamWWWConfig::swisspfam?name=" . $name ."&acc=$acc>$name</A>&nbsp;";
	    
#	    for( my $i=0; $i < (11 - length($name)); $i++) {
#	    	$string .= "\&nbsp;";
#	    }
	    
#	    if ($verbose) {
#		# we need to print the extra annotation
#		my $entry = &PfamWWWConfig::protein_sequence( $name );
#		if (! defined $entry ) {
#		     $string .="Organism and Description information could not be retrieved";
#		}
#		else {
#		    my ($org, $des);
		    
#		    $org = $entry->organism();
#		    $org = ucfirst( $org );
#		    $des =  "[$org]&nbsp;".$entry->desc();
#		    $des =~ s/\s/&nbsp;/g;

#		     $string .= "<SPAN class=normaltext>$des</SPAN>";
#		}
#	    }

#	  print $string if (!$reselect_buttons);
#	    $annseq = &PfamWWWConfig::annotated_sequence_by_name($name);


	
#	  #  print "<BR> ALL : $all_params :::: DOM: $dom_name <BR>";

#	    if($reselect_buttons) {
#	      my @overlaps = ("1");
#	      my $val = 1;
#	      foreach (sort keys %new_select_order) {
#		if ($new_select_order{$_} =~ /[0-9]|[a-z]|[A-Z]/) {
#		  $val = 0;
#		}
#		#  print "KEY: $_ , val: " .$new_select_order{$_} . " <BR>";
#	      }
#	      if ($val) {
#		#print "VAL: $val <BP>";
#		%new_select_order = undef;
#		%new_select_order = $pictures->_default_other_menu();
#	      }
	      
#	      $pictures->_print_reselect_buttons($annseq, $dom_name . "~" . $acc, \@overlaps, "", $all_params, %new_select_order); 
#	      $reselect_buttons = 0;
	      
#	      print $string;
	      
#	    }
	    
#	    $pictures->display_domains_gif( $annseq, $zoom_factor, 0, 0, %new_select_order );
#	    print "<BR><P>\n";
#        }
    }



$p->print("</TABLE></CENTER><P>$f");
}


1;  # says use was ok
__END__

