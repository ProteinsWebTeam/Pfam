#
# Module for AnnSeq2WWW
#
# Cared for by Mhairi Marshall<mm1@sanger.ac.uk>
#
# Copyright Mhairi Marshall & Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

AnnSeq2WWW - DESCRIPTION

This package contains methods for producing web-representations of
an AnotatedSequence, i.e. the Pfam domain structure of a sequence

It was adapted from code by Mats Johnson at the Karlinska institute
with Erik Sonnhammer. <esr@kisac.cgr.ki.se>

Mhairi Marshall <mm1@sanger.ac.uk>  is
the closest person to the code at the moment. 

Main problem with this module is that it is too tightly coupled to 
the PfamWWWconfig module to allow its use elsewhere

Instances can be created with a Paging instance; in that case, all output
gets written to the Paging object, otherwise output goes to stdout    

=head1 SYNOPSIS

    use AnnSeq2WWW;

    my $drawer = AnnSeq2WWW->new();

    # get a Bio::Pfam::AnnotatedSequence from somewhere, $annseq

    $drawer->display_domains_gif( $annseq, $zoom_factor, 1 );   # or ...

    $drawer->display_domains_table( $annseq );

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut


# Let the code begin...


package Bio::Pfam::Web::AnnSeq2WWW;
use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use strict;

use Bio::Pfam::Web::PfamWWWConfig;
#use GIFGenerator;


@ISA = qw(Exporter);


# constants 

my $imageA="colorsA";
my $imageB="colors";

my $space_name=$PfamWWWConfig::space;      # GIF for the desolate regions between domains
my $overlap_name=$PfamWWWConfig::image_names{'overlap'};  # GIF to show where domains overlap
my $blank_name=$PfamWWWConfig::blank;      # A blank GIF, for formatting purposes

my $overlap_picture_width=59;
my $overlap_picture_height=30;
my $scheme_height=23;                      # the height of each GIF making up the image
my $pfamB_scheme_height = 23;




=head2 new
  Title   : new
  Usage   : 
  use AnnSeq2WWW;

my $ref = AnnSeq2WWW->new();  # or ....
my $ref = AnnSeq2WWW->new( $paging );

Function:
If the object is created with a Paging object reference, then that is used
  to do the printing, otherwise all printing is dome to stdout
  Returns :  A reference to AnnSeq2WWW
  Args    :  A Paging reference (optional)


=cut

sub new {
  my($class, $paging) = @_;
  
  my $self =  { 'paging' => $paging,
		'next_colour_A'  => 1, 
		'next_colour_B' => 1,
		'colour_map' => {} };

  bless $self, $class;
    
  return $self;
}





=head2 display_domains_gif


=cut

sub display_domains_gif {
   my ($self, $annseq, $zoom_factor, $show_overlaps, $show_domains_text, $other_order, $hmm_scop, $print_basic) = @_;
   my %other_order = %{$other_order};
 
  my ($protein_length, $protein_name);

  my @domains;            # the basic list of all domains on the sequence
  my $adjusted_domains;   # Stores a ref to a list of domain segments that will be drawn
  my $overlap_positions;  # A ref to a list of positions for where the overlap images will be shown
  
    # here we go...

  if ($zoom_factor < 0.1) {
    $zoom_factor = 0.1;
  }
  
  if ($zoom_factor > 10) {
    $zoom_factor = 10;
  }
  
  $protein_name = $annseq->id();
  $protein_length = $annseq->length();
  
  my($dom) = $self->sort_the_regions($annseq, 1);
  @domains = @{$dom};
  
  $self->_map_images_to_domains( @domains );

  
  
  my $hmm_scop_empty = 0;
  
  if (   defined($other_order{0}) ||  defined($other_order{1}) ||  defined($other_order{2}) ||  defined($other_order{3}) ||  defined($other_order{4}) ||  defined($other_order{5}) ||  defined($other_order{6}) ||  defined($other_order{7}) || defined($other_order{8}) ||  defined($other_order{9}) ||  defined($other_order{10}))  {
  } 
  else {
	if (defined $hmm_scop){
    #if ($hmm_scop =~ /[A-Z]/i) {
      $hmm_scop_empty = 1;
      %other_order  =  _hmm_other_menu();
    } else {
      %other_order  =  _default_other_menu();
    }
    
  }
  
  
  @domains = sort { $a->{'reg'}->from() <=> $b->{'reg'}->from() } @domains;

  ($adjusted_domains, $overlap_positions) = $self->_resolve_overlaps( \@domains, %other_order );
  
#  print "Content-type: text/html\n\n"; print "MEAOW $hmm_scop <P>";
  if ($show_overlaps) {
#    $self->_print_overlap_images( $overlap_positions, $zoom_factor, $protein_length );
  }
  
  if( ($hmm_scop) &&  ($hmm_scop =~ /[0-9]/) ) {
    $self->_print_hmm_domain($hmm_scop,  $zoom_factor, $protein_length);
    
  }
 
  # next, draw the actual domain segments themselves
  
  #$self->_print_domains_images( $adjusted_domains, $protein_length, $zoom_factor, $id); ## download gif
  $self->_print_domains_images( $adjusted_domains, $protein_length, $zoom_factor, $print_basic, $protein_name);
  
  # print the names of the domains and their extent below the GIF, if requested
  
  if ($show_domains_text)  { 
    $self->_print_domains_text( $zoom_factor, \@domains );
  }
  
  
  if (defined $hmm_scop){
#	if( ($hmm_scop =~ /[A-Z]/i) ) {
    
    ## Reset the default menu to being empty !
    	if ($hmm_scop_empty) {
      		foreach my $key (sort keys %other_order) {
				$other_order{$key} = undef;
	
      			}
      
    		}
    
    
    
 
    
 # 	}
  }
  return ($overlap_positions, $adjusted_domains); ## Need both of these kept in for Rob's structure stuff
  
}




=head2 display_domains_table

Title   : display_domains_table
 Usage   : $annseq2wwwref->display_domains_table
 Function:
    This displays the domain structure of the given AnnotatedSequence in a table, where
    each row contains the domain name (hyperlinked to swisspfam), the start residue
    and end residue
 Args    :
    1. A Bio::Pfam::AnnotatedSequence object, containing a list of Bio::Pfam::PfamRegion
 Returns : 

=cut

#DISPLAY_DOMAINS_TABLE

sub display_domains_table {
    my ($self, $annseq, $id, $temp_other_new,  $overlaps, $no_key) = @_;
    my %other_new;
    %other_new = %{$temp_other_new};

    ## vars for the moment - move later
    my($font_color) = "#000000";
    my($border_color) = "#0000A0";


    my($list) = $self->sort_the_regions($annseq, 1, "nested");

    my @list = @{$list};


   
    print qq(<P><Table border=0 cellpadding=5 cellspacing=0  width=100%><TR><TD valign=top align=left>);
    
 


## PRINT OUT THE TABLE CODE ##########

    print qq(
	     </td><td align=center valign=top>


	     
	     <TABLE border=0 cellpadding=0 cellspacing=0> 

	     \n<TR bgcolor=$PfamWWWConfig::pfamcolour><TD VALIGN=top ALIGN=center>
	     \n<TABLE width=100% border=0 cellpadding=3 cellspacing=0>
	     );

#<TR CLASS=whitetableheader ><Th COLSPAN=3 bgcolor=\#000070 align=center><font color=$font_color>Pfam Domains</Th></TR> 
    print qq (\n<TR bgcolor=\#dfdff7 CLASS=whitetableheader><TH><font color=$font_color NOWRAP>Source</TH><TH><font color=$font_color>Domain</TH><TH><font color=$font_color>Start</TH><TH><font color=$font_color>End</TH></TR>);
	 

    my $got_smart = 0; my $got_other_region = 0;
    my $no_pfamA = 1;

    ## Print out all the PfamA & PfamB domains in the first table
    foreach my $region (@list) {
	my $link;
	if ( ($region->{'region'} =~ /pfamA/) ||  ($region->{'region'} =~ /pfamB/)) {

	  $no_pfamA = 0;

	  if ($region->{'region'} =~ /pfamA/) {
	    $link = "<a href=$PfamWWWConfig::getacc?".$region->{'reg'}->accession().">";
	  }	  else {		# pfam-B
	    $link = "<a href=$PfamWWWConfig::getpfamb?acc=".$region->{'reg'}->accession().">";
	  }


	  ## CODE FOR COLOURING PFAMA NAME IN TABLE
	  my $font_col = "000000";
	  my ($start_b, $slash_b); 

	  if ($region->{'region'} =~ /pfamA/) {
	    $font_col = &PfamWWWConfig::get_hex_colour($self->_colour_map($region->{'reg'}->accession()));
	    $start_b = "<B>";
	    $slash_b = "</B>";
	  }
	    
	  $link .= "<FONT COLOR=#$font_col>$start_b" .  $region->{'reg'}->id(). "$slash_b</FONT></A>";
	  $link .= " (Partial) " if($region->{'frag_model'});
	#  print "REGION NEST: " .$region->{'nested'} . " <P>";
	  $link .= " (<A HREF=\"#\" onClick='w=window.open(\"help.pl?type=Nested\", \"helpwindow\", \"width=450, height=410, scrollbars=yes,resizable=yes\");w.focus();'>nested</A>) " if($region->{'nested'});
	  $self->print_html("\n<TR CLASS=normaltext ><td class=normaltext align=center ><B><font color=#0000EF>Pfam</font></b></td><TD NOWRAP>",$link,"</TD><TD class=normaltext>",$region->{'reg'}->from(),"</TD><TD class=normaltext>",$region->{'reg'}->to(), "</TD></TR>\n");
	 
	} elsif ($region->{'reg'} =~ /HMMOtherRegion/i)  {
	  $got_smart = 1;
	  
	} else {
	  $got_other_region = 1;
	}


      } #/ end for each region


    ## Print out an empty row if no pfamA regions
    $self->print_html("\n<TR><TD COLSPAN=3>&nbsp;</TD></TR>\n") if ($no_pfamA);

    ##### PRINT OUT THE SMART DATA 
    if ($got_smart) {
   #   $self->print_html("<TR bgcolor=#000000><td colspan=4 bgcolor=#000000><img src=\"/icons/blank.gif\" width=\"1\" height=\"1\"></td></tr>");
   #   $self->print_html("<TR CLASS=whitetableheader ><Th COLSPAN=3 bgcolor=#000000 align=center><font color=$font_color>Smart Domains</TH></TR><TR bgcolor=#000070 CLASS=whitetableheader><TH><font color=$font_color>Domain</TH><TH><font color=#FFFFFF>Start</TH><TH><font color=$font_color>End</TH></TR>");
     
      foreach my $region (@list) {  
	if ($region->{'reg'} =~ /HMMOtherRegion/i) {
	  
	  my $link =  "<A HREF=http://smart.embl-heidelberg.de/smart/do_annotation.pl?DOMAIN=" . $region->{'reg'}->domain() .  "&BLAST=DUMMY><FONT COLOR=#000000><B>" . $region->{'reg'}->domain() . "</FONT></B></A>" ;
	  
	  $self->print_html("\n<TR CLASS=normaltext ><td class=normaltext align=center><b>Smart</b></td><TD>" , $link  ,"</TD>") ;
	  
	  $self->print_html("<TD  CLASS=normaltext>", $region->{'screen_beg'}, "</TD>");
	  $self->print_html("<TD  CLASS=normaltext>", $region->{'screen_end'}, "</TD>");
	  
	  
	}
      }
    } #/ end got SMART REGIONS



   # $self->print_html("</TABLE>");  ## end of Pfam/SMART DOMAINS TABLE !

   # $self->print_html("</TD>");

    if ($got_other_region) {
    
  #  $self->print_html("</TD><TD bgcolor=#000000>&nbsp;</TD><TD  VALIGN=TOP ALIGN=center>
#\n<TABLE  width=100% VALIGN=top  border=0 cellpadding=3 cellspacing=0>
#\n<TR VALIGN=TOP CLASS=whitetableheader >
#<Th COLSPAN=5 bgcolor=#000000 VALIGN=top align=center><font color=$font_color>Other Regions</Th></TR>
#\n <TR bgcolor=#000070  CLASS=whitetableheader><TH><font color=$font_color>Type</TH><TH><font color=$font_color>Source</TH><TH><font color=$font_color>Start</TH><TH><font color=$font_color>End</TH> ");
  

  ## CONTEXT STUFF 

    
   # $self->print_html("</TR>");
    
    my($list) = $self->sort_the_regions($annseq);
    
    my @list = @{$list};
    

    ## Print out the other regions in the 2nd table
    foreach my $region (@list) {  
      if ( ($region->{'region'} !~ /pfamA/) &&  ($region->{'region'} !~ /pfamB/)) {

	if ( ($region->{'region'} !~ /Context/i) &&($region->{'reg'} !~ /HMMOtherRegion/i)  ){
	 
	  $self->print_html("\n<TR CLASS=normaltext ><TD NOWRAP CLASS=normaltext  align=center NOWRAP><A HREF=$PfamWWWConfig::region_help>", $region->{'reg'}->source(), "</TD>") ;
	  $self->print_html("<TD  CLASS=normaltext NOWRAP>",   _full_sequence_names($region->{'reg'}->type()), "</A></TD>") ;
	} elsif ($region->{'reg'} =~ /HMMOtherRegion/i) {

	  next;

	} else {

	 # next;  ### CONTEXT REGION SO SKIP FOR THE MOMENT

	  my $link = "<a href=$PfamWWWConfig::getacc?".$region->{'reg'}->accession()."><Font color=#CB04F5><B>";
	  
	  $link .= $region->{'reg'}->id(). "</B></font></A>";
	  $self->print_html("\n<TR CLASS=normaltext ><TD  align=center><A HREF=\"#\" onClick='w=window.open(\"help.pl?type=Context\", \"helpwindow\", \"width=450, height=410, scrollbars=yes,resizable=yes\");w.focus();'> Context</A>  </TD>") ;
	  $self->print_html("<TD>$link</TD>") ;


	}

	
	$self->print_html("<TD  CLASS=normaltext >", $region->{'screen_beg'}, "</TD>");
	$self->print_html("<TD  CLASS=normaltext>", $region->{'screen_end'}, "</TD>");
	
	if ( ($region->{'score'} =~ /[1-9]/) && ($region->{'region'} !~ /Context/i   ) ){

#	  $self->print_html("<TD  CLASS=normaltext>", $region->{'score'}, "</TD>") ;
	} else {
#	$self->print_html("<TD>", , "</TD>");
	}
	

	$self->print_html("</TR>");
  
      }
	
      
    }



    
    

    }

    $self->print_html("</TABLE>");
    $self->print_html("</TD></TR></TABLE> </td><td valign=top align=center>");
   # print "PARAMS: $params <P>";
   ## PRINT OUT THE BUTTON TO ALLOW THE DOMAIN ORDER TO CHANGE ######

    $self->_print_reselect_buttons($annseq, $id,   $overlaps, "" ,"",  $no_key, %other_new);
    $self->print_html("</td></tr></table> <BR><BR>\n\n");

    
} #/ end SUB


#### _PRINT_RESELECT_BUTTONS


sub _print_reselect_buttons {
  ## vars for the moment - move later
    my($font_color) = "#FFFFFF";
    my($border_color) = "#0000A0";
    my ($self,  $annseq, $id,  $overlaps, $hmm_scop, $all_proteins_params,  $no_key,  %other_new) = @_;
 
     # print "$annseq, $id,  OVER: $overlaps, $hmm_scop, ALL: $all_proteins_params, %other_new <P>";
    my($list) = $self->sort_the_regions($annseq);
 #   print "Content-type: text/html\n\n";
 #   print "ALL: $all_proteins_params <P>";
   # print "NO: $no_key <P>";
#    foreach (sort keys %other_new) {
#      print "key: $_ , val: ".$other_new{$_} . " \n";
#    }


    my @list = @{$list};
#    print "LIST: @list <P>";
    my(%valid_select_domains) = ();

    
    ### for get all proteins need to use ALL the domains as lots of sequences
    if ($all_proteins_params) {
     my  %temp = $self->_default_other_menu();
      
     foreach (sort keys %temp) {
       $valid_select_domains{$temp{$_}} = 1;

     }
    } else {

      foreach my $region (@list) {
	$valid_select_domains{$region->{'region'}} = 1;
      }

    }

    
    
    #set the form submission var to: swisspfamget.pl
    
    
    ## Count the number of ACTUAL keys we have
    my($valid_domain_count) = 0;
    foreach my $key (%valid_select_domains) {
      $valid_domain_count++ if ($valid_select_domains{$key});
    }
    
    $valid_domain_count--;
    
    my($no_value)= 1;
    
    foreach my $key (0..$valid_domain_count) {
      $no_value = 0 if($other_new{$key} =~ /[A-Z]|[a-z]/);
    }
    if ($no_value) {
      if ($hmm_scop) {
	(%other_new) = _hmm_other_menu();
      } else {
	(%other_new) = _default_other_menu();
      }
      
    }				# end if NO value
        
  #  $self->print_html("<TR><TD NOWRAP>");
    my $tot = 0;

    if (  @$overlaps > 0 )   {
   #  print "eep :$all_proteins_params <P>";
      if (!$hmm_scop) {
	if ($all_proteins_params) {
	  if ($all_proteins_params =~ /~name=/) {
	    $self->print_html("\n<!--FORM STUFF--> <form name=\"pfam_shuffle\"  method=get action=\"".$PfamWWWConfig::viewspecies . "\"> ");
	  } else {
	    $self->print_html("\n<!--FORM STUFF--> <form name=\"pfam_shuffle\"  method=get action=\"".$PfamWWWConfig::getall . "\"> ");
	  }

	} else {
	  $self->print_html("\n<!--FORM STUFF--> <form  name=\"pfam_shuffle\"   method=get action=\"".$PfamWWWConfig::swisspfam . "\"  > ");
	}

      }
      $self->print_html("<input type=hidden name=no_key value=$no_key>\n");
      my (%store_pfam_doms);

##action=\"".$PfamWWWConfig::swisspfam . "\"
      foreach my $key (sort keys %other_new) {
	
	my $other_value = $other_new{$key};
	
	if ($other_value =~ /[A-Z]/i) {
	  my $not_present = 1;
	  foreach my $valid_key (sort keys %valid_select_domains) {
	    if ($valid_key =~ /$other_value/) {
	      $not_present = 0;
	      $store_pfam_doms{$other_value} = $valid_key;
	      $tot++; ## total number of domains that overlap, used for select box size
	    }	
	    
	  }
	  
	  if ($not_present) {
	    #### NEW CODE
	  #  print "BOO <P>";
	  #  $other_new{$key} = "not_valid";
	  #  print "delete $key: " . $other_new{$key}. " <BR>";
	    delete $other_new{$key} if ($other_new{$key} !~ /hidden/);
	    $tot++  if ($other_new{$key} =~ /hidden/); ## total number of domains that overlap, used for select box size
	  }
	}
	
      }
                        
#<TR><TD colspan=2 CLASS=whitetableheader bgcolor=#000070>Domain Order: Change the domain order using the drop down menus and click the 'Change order' button</TD></TR>

      if($all_proteins_params) {
	my (@all) = split(/~/, $all_proteins_params);
	foreach (@all) {
	  my($name, $param) = split(/=/, $_);
	  $self->print_html("<input type=hidden name=$name value=\"$param\">\n");
	}

      } else {

      $self->print_html("<input type=hidden name=name value=$id>\n<LEFT>");

    }
   
      $self->print_html("<TABLE border=0 cellpadding=5 cellspacing=0><tr><td bgcolor=#dfdff7 valign=top colspan=2 WRAP span class=normaltext><B><A name=over_align>Overlapping</A> Domains:</B> Change the domain order using the ^ and v buttons. View the changes by clicking the 'Change order' button.</span></td></tr>

<TR><TD NOWRAP bgcolor=#ffffcc valign=top >
<table cellpadding=5 cellspacing=0><td><td bgcolor=#ffffcc valign=top >
   ");
      
      
   #   print "TOT: $tot <P>";
      $self->print_html("
<input type=\"hidden\" name=\"test_ret\" value=\"\"><span class=boldtextsmall>high priority</span> <br>
 <select name=test size=$tot width=20>");
      foreach my $valid_doms (sort keys %other_new) {
	my ($tem, $print_text);
	if ($other_new{$valid_doms} =~ /hidden/) {
	  $tem = $other_new{$valid_doms};
	  $print_text = $tem;
	  $print_text =~ s/~hidden/ (hidden)/;
	  $print_text =~ s/sig_p/signal peptide/;
	  
	} else {
	  $tem = $store_pfam_doms{$other_new{$valid_doms}};
	  $print_text = _full_sequence_names($other_new{$valid_doms});
	}
	$print_text =~ s/\_/ /;
	$self->print_html("<OPTION value=$tem>$print_text &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</OPTION>");
      }
      $self->print_html("</select><br><span class=boldtextsmall>low priority</span><P><INPUT  TYPE=SUBMIT value=\"Change order\"> </td>");



      $self->print_html("
</select>
<td align=left valign=center bgcolor=#ffffcc align=left class=normaltext> 

 <input type=\"button\" value=\" ^ \" onClick=\"pfam_shuffle_test_act('up')\"> Increase priority <br><input type=\"button\" value=\" v \" onClick=\"pfam_shuffle_test_act('down')\"> Decrease priority
<br>
<input type=\"button\" value=\" hide domain\" onClick=\"pfam_shuffle_test_act('remove')\"> <br>
<input type=\"button\" value=\" display domain\" onClick=\"pfam_shuffle_test_act('display')\">

</td>


    ");


#	$self->print_html("<A HREF=http://www.sanger.ac.uk/Software/Pfam/help/demo><img src=http://www.sanger.ac.uk/Software/Pfam/gifs/demo_images/small_question_mark.gif border=0></A>") if (!$hmm_scop);

	$self->print_html("</TD></TR>");

} else {

$self->print_html("<A name=over_align><font color=#ff0000><B>*</b></font> No <b>Overlapping domains</b> detected<P><P>") if ($all_proteins_params);

}

$self->print_html("</FONT></I></CENTER></form></TABLE></td></tr></table><P> ");
}





# the following is a wrapper method for printing to the Paging object,
# if there is one

sub print_html {
  my ($self, @lines) = @_;

  my $paging = $self->_paging();
  
  my $line = join('',@lines);
  if( defined $paging ) {
    $paging->print($line);
  } else {
    print STDOUT "$line";
  }
}





# internal methods...

=head2 map_images_to_domains




Title   : map_images_to_domains
  Usage   : $annseq2htmlref->map_images_to_domains( @domains );
Function:
Assigns a colour gif image file to each domain in the list 
  Args    :
  1. A list of hash references
  Returns : 
  Notes :
  This function is tightly coupled to display_domains_gif, and is only (and should
									only) be called from that function. 

=cut



sub _map_images_to_domains {
  my ($self, @domains) = @_;
  
  for (my $counter=0; $counter < @domains; $counter++) {  
    my $colour_number;
    if ($domains[$counter]->{'region'} =~ /pfamA/)  { # pfam-A
      $domains[$counter]->{'pfamb'} = 0;
      if (! defined($self->_colour_map($domains[$counter]->{'reg'}->accession()))) {
	# we have not already assigned a colour to this domain
	$self->_colour_map($domains[$counter]->{'reg'}->accession(), $self->_next_colour_A());
      }
      
      
      $colour_number = $self->_colour_map( $domains[$counter]->{'reg'}->accession() );
      $domains[$counter]->{'image'} = $imageA . $colour_number . $PfamWWWConfig::image_ext;
      my($ima) = $domains[$counter]->{'image'};
      
      my $my_file = $PfamWWWConfig::tempdir . "/" . $domains[$counter]->{'image'};
      if (!(-e $my_file)) {	# have we created this GIF already?
	&GIFGenerator::generate_2stripedGIF( $colour_number ); 
      }
    } elsif  ($domains[$counter]->{'region'} =~ /pfamB/)  { # i.e pfamB
      $domains[$counter]->{'pfamb'} = 1; 
      if (! defined($self->_colour_map($domains[$counter]->{'reg'}->accession()))) {
	# we have not already assigned a colour to this domain
	$self->_colour_map($domains[$counter]->{'reg'}->accession(), $self->_next_colour_B());
      }
      $colour_number = $self->_colour_map($domains[$counter]->{'reg'}->accession());
      
      $domains[$counter]->{'image'} = $imageB . $colour_number . $PfamWWWConfig::image_ext;
      
      my $my_file = $PfamWWWConfig::tempdir . "/" . $domains[$counter]->{'image'};
      if (!(-e $my_file)) {
	&GIFGenerator::generate_3stripedGIF($colour_number);
      }
    } elsif ($domains[$counter]->{'region'} =~ /SCOP:/i)  {
    my $my_file = $PfamWWWConfig::tempdir . "/scop_domain". $PfamWWWConfig::image_ext;
      if (!(-e $my_file)) {	# have we created this GIF already?
			&GIFGenerator::generate_SCOPGIF("SCOP");
			}
		$domains[$counter]->{'image'} = "scop_domain". $PfamWWWConfig::image_ext;
	 } elsif ($domains[$counter]->{'region'} =~ /CATH:/i)  {
    my $my_file = $PfamWWWConfig::tempdir . "/cath_domain". $PfamWWWConfig::image_ext;
      if (!(-e $my_file)) {	# have we created this GIF already?
			&GIFGenerator::generate_CATHGIF("CATH");
			}
		$domains[$counter]->{'image'} = "cath_domain". $PfamWWWConfig::image_ext;
		}elsif ($domains[$counter]->{'region'} =~ /disordered/i)  {
	my $my_file = $PfamWWWConfig::tempdir . "/disordered". $PfamWWWConfig::image_ext;
      if (!(-e $my_file)) {	# have we created this GIF already?
			&GIFGenerator::generate_1stripedGIF( "disordered");
			}
		 $domains[$counter]->{'image'}= "disordered". $PfamWWWConfig::image_ext	
	}
	$domains[$counter]->{'image'} = $PfamWWWConfig::image_temp . "/" . $domains[$counter]->{'image'} ;
    # the 'image' index now holds the URL of the correct gif
  }
}
  

=head2 _print_domains_images

 Title   : _print_domains_images
 Usage   : $self->_print_domains_images( $non_overlapping_domains_list_ref, 
					 $protein_length,
					 $zoom );
 Function:
    This function actually prints an image representation of the given list
    of non-overlapping domins (overlaps should have already been resolved
    with a call to _resolve_overlaps)
 Args    :
    1. Ref. to a list of hash references, each encapsulating a seq. seqment 
         + some other stuff
    2. The length of the protein
    3. Zoom factor
 Returns : 
 Notes :
    This function is tightly coupled to display_domains_gif, and is only (and should
    only) be called from that function. 

=cut

sub  _print_domains_images{
  my ($self, $adjusted_domains, $protein_length, $zoom_factor,  $print_basic, $id) = @_;
  my $currentpos = 1;
  my $error = 0.0;
     
  #$self->print_html("<form name=taxonomy>");
  $self->print_html("<BR><BR>");
  
  my $complete_gifs;
  my $gif_length = 0;
  
  
  foreach my $region (@$adjusted_domains) {
    if ($region->{'screen_beg'} > $currentpos) {
      # we need to insert a spacer
      my $pixel_width = ($region->{'screen_beg'} - $currentpos) * $zoom_factor;
      $error += $pixel_width - int($pixel_width);
      if ($error >= 1.0) {
	$pixel_width += 1.0;
	$error -= 1.0;
      }
      $self->print_html("<IMG SRC=$space_name ALIGN=LEFT HSPACE=0 BORDER=0 HEIGHT=",$scheme_height);
      $self->print_html(" WIDTH=", int($pixel_width), ">\n");
    #  print " SPACE: " .int($pixel_width) .  " ";

     
      $pixel_width = 1 if(int($pixel_width) == 0);
   #   print " pix: $pixel_width ";
      $complete_gifs .= "space:" . int($pixel_width) . "!";
      $gif_length = $gif_length + int($pixel_width);
    }
    
    my $imagewidth = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
    
    if  ($region->{'region'} =~ /pfamA/)  { # we have a PfamA region
      my $box_text = $region->{'reg'}->id()." " . $region->{'screen_beg'} . "-" .$region->{'screen_end'} . " : ".$region->{'reg'}->annotation();
      my $window_text = $box_text;
      $box_text =~ s/\'/\\\'/g;
      
      my $pixel_width = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
      $error += $pixel_width - int($pixel_width);
      if ($error >= 1.0) {
	$pixel_width += 1.0;
	$error -= 1.0;
      }
      $self->print_html("<A HREF=$PfamWWWConfig::getacc?", $region->{'reg'}->accession());
      $self->print_html(" onMouseOver=\"window.status='",$box_text,"';return true\" onMouseOut=\"window.status=''\";return true; > ");
     # print "Context-type: text/html\n\n";
     # print "REGION: ".$region->{'image'} . " <P>";
      my $new_gif;

  
      if ($print_basic) {
	if ($region->{'image'}=~ /(colorsA\d+.\w\w\w)$/) {
	  $new_gif = $1;
	  $self->print_html("<IMG SRC=", $PfamWWWConfig::image_temp , "/$new_gif", );
	}
      } else {
	$new_gif = _get_new_pfamA_gif( $region->{'reg'}->id() , $region->{'image'} , int($pixel_width),$region->{'screen_beg'} - $region->{'original_screen_beg'} , $region->{'original_screen_end'} - $region->{'screen_end'} , $region->{'frag_model'});
	$self->print_html("<IMG SRC=", $PfamWWWConfig::image_temp , "/new_gifs/$new_gif", );
	##$self->print_html("<IMG SRC=", $region->{'image'});

      }


      $self->print_html(" HSPACE=0 ALIGN=LEFT BORDER=0 HEIGHT=",$scheme_height);
      $self->print_html(" WIDTH=", int($pixel_width) );
      $self->print_html(" ALT=\"$window_text\"></A>\n");	   
      
      $complete_gifs .= "$new_gif:" . int($pixel_width) . "!";
      $gif_length = $gif_length + int($pixel_width);
      
    }  elsif ($region->{'region'} =~ /pfamB/){ ## PfamB region
      if ($imagewidth >= 1) {
	my $box_text = $region->{'reg'}->id();
	my $window_text = $box_text;
	$box_text =~ s/\'/\\\'/g;
	
	my $pixel_width = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
	$error += $pixel_width - int($pixel_width);
	if ($error >= 1.0) {
	  $pixel_width += 1.0;
	  $error -= 1.0;
	}
	$self->print_html("<A HREF=$PfamWWWConfig::getpfamb?acc=",$region->{'reg'}->accession());
	$self->print_html(" onMouseOver=\"window.status='",$box_text,"';return true\" onMouseOut=\"window.status=''\";return true; >");
	$self->print_html("<IMG SRC=", $region->{'image'});
	$self->print_html(" HSPACE=0 ALIGN=LEFT  BORDER=0 HEIGHT=",$pfamB_scheme_height);
	$self->print_html(" WIDTH=", int($pixel_width));
	$self->print_html(" ALT=\"$window_text\"></A>\n");
	
	
	if ($region->{'image'} =~ /\/(colors(\d+)\.\w\w\w)$/) {
	  $complete_gifs .= "$1:" . int($pixel_width) . "!";
	  $gif_length = $gif_length + int($pixel_width);
	}
	
      }
      
      ######### UPDATE LATER, CANT BE BOTHERED !
    } elsif ( $region->{'reg'} =~ /HMMOtherRegion/i)  { # we have another HMM  region
      my $box_text = $region->{'region'}. ": " .  $region->{'reg'}->domain() . " " . $region->{'screen_beg'} . "-" . $region->{'screen_end'};
      my $window_text = $box_text;
      $box_text =~ s/\'/\\\'/g;
      
      my $pixel_width = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
      $error += $pixel_width - int($pixel_width);
      if ($error >= 1.0) {
	$pixel_width += 1.0;
	$error -= 1.0;
      }
      
      if ($region->{'region'} =~ /smart/) {
	$self->print_html("<A HREF=http://smart.embl-heidelberg.de/smart/do_annotation.pl?DOMAIN=",$region->{'reg'}->domain(), "&BLAST=DUMMY");
      } elsif ($region->{'region'} =~ /tigr/) {
	$self->print_html("<A HREF=http://www.tigr.org/tigr-scripts/CMR2/hmm_report.spl?user=access&password=access&acc=",$region->{'reg'}->domain(), "");
      } else {
	$self->print_html("<A HREF=\"javascript:void()\" ");
      }
      
      $self->print_html(" onMouseOver=\"window.status='",$box_text,"';return true\" onMouseOut=\"window.status=''\";return true; > ");

      my $new_gif = _get_new_pfamA_gif( $region->{'reg'}->domain() , $PfamWWWConfig::image_link . "/" .$region->{'region'} . $PfamWWWConfig::image_ext  , int($pixel_width) , 1 , 1, 0);
          
      $self->print_html("<IMG SRC=", $PfamWWWConfig::image_temp , "/new_gifs/$new_gif", );

      $self->print_html(" HSPACE=0 ALIGN=LEFT BORDER=0 HEIGHT=",$scheme_height);
      $self->print_html(" WIDTH=", int($pixel_width) );
      $self->print_html(" ALT=\"$window_text\"></A>\n");
      $complete_gifs .= "SMARTDOM$new_gif:" . int($pixel_width) . "!";
     
    }  elsif  ($region->{'region'} =~ /Context/i) { # we have a Context  region

      my $box_text = "Context Domain : ". $region->{'reg'}->id().": ".$region->{'reg'}->annotation();
      my $window_text = $box_text;
      $box_text =~ s/\'/\\\'/g;
      
      my $pixel_width = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
      $error += $pixel_width - int($pixel_width);
      if ($error >= 1.0) {
	$pixel_width += 1.0;
	$error -= 1.0;
      }
      
      $self->print_html("<A HREF=$PfamWWWConfig::getacc?", $region->{'reg'}->accession());
      $self->print_html(" onMouseOver=\"window.status='",$box_text,"';return true\" onMouseOut=\"window.status=''\";return true; > ");
      
#      my $new_gif = _get_new_pfamA_gif( $region->{'reg'}->id() , $region->{'image'} , int($pixel_width),$region->{'screen_beg'} - $region->{'original_screen_beg'} , $region->{'original_screen_end'} - $region->{'screen_end'} , $region->{'frag_model'});
      my $new_gif = "$PfamWWWConfig::image_link/context.gif";

    #  $self->print_html("<IMG SRC=", $PfamWWWConfig::image_temp , "/new_gifs/$new_gif", );
     # $self->print_html("<IMG SRC=", $PfamWWWConfig::image_temp , "$new_gif", );
      $self->print_html("<IMG SRC=$new_gif", );
      
      #  $self->print_html("<IMG SRC=", $region->{'image'});
      $self->print_html(" HSPACE=0 ALIGN=LEFT BORDER=0 HEIGHT=",$scheme_height);
      $self->print_html(" WIDTH=", int($pixel_width) );
      $self->print_html(" ALT=\"$window_text\"></A>\n");	   
      
      $complete_gifs .= "context.png:" . int($pixel_width) . "!";
      $gif_length = $gif_length + int($pixel_width);
      
    } elsif  ($region->{'region'} =~ /SCOP/i)  { # we have a scop region
      my $box_text = $region->{'region'};
      my $window_text = $box_text;
      $box_text =~ s/\'/\\\'/g;
      
      my $pixel_width = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
      $error += $pixel_width - int($pixel_width);
      if ($error >= 1.0) {
	$pixel_width += 1.0;
	$error -= 1.0;
      }
      $self->print_html("<A HREF=http://scop.mrc-lmb.cam.ac.uk/scop/search.cgi?sunid=".$region->{'reg'}->source);
      $self->print_html(" onMouseOver=\"window.status='",$box_text,"';return true\" onMouseOut=\"window.status=''\";return true; > ");
      my $new_gif = _make_scop_gif( $region->{'region'} , $region->{'image'} , int($pixel_width),$region->{'screen_beg'}, $region->{'screen_end'},$region->{'original_screen_beg'}, $region->{'original_screen_end'} );
	$self->print_html("<IMG SRC=", $PfamWWWConfig::image_temp , "/new_gifs/$new_gif", );
		#my $new_gif = "$PfamWWWConfig::image_link/scop.gif";
      #$self->print_html("<IMG SRC=$new_gif", );
      unlink($PfamWWWConfig::image_temp , "/new_gifs/$new_gif");
      $self->print_html(" HSPACE=0 ALIGN=LEFT BORDER=0 HEIGHT=",$scheme_height);
      $self->print_html(" WIDTH=", int($pixel_width) );
      $self->print_html(" ALT=\"$window_text\"></A>\n");
      $complete_gifs .= "$new_gif:" . int($pixel_width) . "!";
      $gif_length = $gif_length + int($pixel_width);
      
    } elsif  ($region->{'region'} =~ /CATH/i)  { # we have a CATH region
      
		my $box_text = $region->{'region'};
     	my $window_text = $box_text;
      	$box_text =~ s/\'/\\\'/g;
      	
      	my $pixel_width = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
      	$error += $pixel_width - int($pixel_width);
      	if ($error >= 1.0) {
			$pixel_width += 1.0;
			$error -= 1.0;
    		}
		my $loc = $region->{'reg'}->type;
		$loc =~ s/CATH://;
		$loc =~ s/\./\//g;
      	$self->print_html("<A HREF=http://www.biochem.ucl.ac.uk/bsm/cath/class".$loc."/index.html");
      	$self->print_html(" onMouseOver=\"window.status='",$box_text,"';return true\" onMouseOut=\"window.status=''\";return true; > ");
      	my $new_gif = _make_CATH_gif( $region->{'region'} , $region->{'image'} , int($pixel_width),$region->{'screen_beg'}, $region->{'screen_end'},$region->{'original_screen_beg'}, $region->{'original_screen_end'} );
	$self->print_html("<IMG SRC=", $PfamWWWConfig::image_temp , "/new_gifs/$new_gif", );
      	unlink($PfamWWWConfig::image_temp , "/new_gifs/$new_gif");
      	$self->print_html(" HSPACE=0 ALIGN=LEFT BORDER=0 HEIGHT=",$scheme_height);
      	$self->print_html(" WIDTH=", int($pixel_width) );
      	$self->print_html(" ALT=\"$window_text\"></A>\n");	   
      
      	$complete_gifs .= "$new_gif:" . int($pixel_width) . "!";
      	$gif_length = $gif_length + int($pixel_width);
      
    }elsif  ($region->{'region'} =~ /disordered/i)  { # we have a disordered srtuctural region
      my $box_text = "Disordered structure";
      my $window_text = $box_text;
      $box_text =~ s/\'/\\\'/g;
      
      my $pixel_width = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
      $error += $pixel_width - int($pixel_width);
      if ($error >= 1.0) {
	$pixel_width += 1.0;
	$error -= 1.0;
      }
	 
      $self->print_html("<IMG SRC=". $region->{'image'} );
      $self->print_html(" HSPACE=0 ALIGN=LEFT BORDER=0 HEIGHT=",$scheme_height);
      $self->print_html(" WIDTH=", int($pixel_width) );
      $self->print_html(" ALT=\"$window_text\"></A>\n");	   
      
      $complete_gifs .= "disordered.png:" . int($pixel_width) . "!";
      $gif_length = $gif_length + int($pixel_width);
      
    }else {			## Other region
      my $box_text = $region->{'reg'}->type();
      $box_text =~ s/_/ /;
      my $window_text = $box_text . ": " . $region->{'reg'}->from(). " to " . $region->{'reg'}->to();
      
      my $pixel_width = ($region->{'screen_end'} - $region->{'screen_beg'} + 1) * $zoom_factor;
      $error += $pixel_width - int($pixel_width);
      if ($error >= 1.0) {
	$pixel_width += 1.0;
	$error -= 1.0;
      }
      $self->print_html("<A HREF=\"$PfamWWWConfig::region_help\"");
      $self->print_html(" onMouseOver=\"window.status='",$window_text,"';return true\" onMouseOut=\"window.status=''\";return true; > ");
      $self->print_html("<IMG SRC=" ,  $PfamWWWConfig::image_names{$region->{'region'}}  );
      $self->print_html(" HSPACE=0 ALIGN=LEFT BORDER=0 HEIGHT=",$scheme_height);
      $self->print_html(" WIDTH=", int($pixel_width));
      $self->print_html(" ALT=\"$box_text\"></A>\n");
      
      
      if ($PfamWWWConfig::image_names{$region->{'region'}} =~ /\/(\w+\.\w\w\w)$/) {
	$complete_gifs .= "$1:" . int($pixel_width) . "!";
	$gif_length = $gif_length + int($pixel_width);
      }
      
      
    }
    # note that we add 1 to the current position. This is because if the last domain ended at residue 501 say,
    # then the next residue is 502
    $currentpos = $region->{'screen_end'} + 1;
  }
  
  if ( $protein_length >= $currentpos ) {
    my $pixel_width = ($protein_length - $currentpos + 1) * $zoom_factor;
    $error += $pixel_width - int($pixel_width);
    if ($error >= 1.0) {
      $pixel_width += 1.0;
      $error -= 1.0;
    }
    $self->print_html("<IMG SRC=",$space_name," ALIGN=LEFT HSPACE=0 BORDER=0 HEIGHT=",$scheme_height);
    $self->print_html(" WIDTH=", int($pixel_width), ">\n"); 

     $complete_gifs .= "space:" . int($pixel_width) . "!";
      $gif_length = $gif_length + int($pixel_width);
  }
  
  # Add some space at the end to always fit domain text into window 
  if ($gif_length < 200) {
     $gif_length = 300;
   } else {
     $gif_length = $gif_length + 100;
   }
# print "GIF: $gif_length <P>";
  #$self->print_html("<IMG SRC=$blank_name ALIGN=LEFT HSPACE=0 BORDER=0 HEIGHT=2 WIDTH=10>\n");
 # my $id;
  my $window_width = ($protein_length / 2) + 40;
  $window_width = "600" if ($window_width < 600);
 # print "THE LEN: " . length($complete_gifs) . " <P>";
  if (length($complete_gifs) > 2000) {
    my $dol = "$$";
    $dol .= "DIS_" if ($complete_gifs =~ /disordered/);
    $dol .= "SCOP" if ($complete_gifs =~ /SCOP~SCOP/);
    $dol .= "CATH" if ($complete_gifs =~ /CATH~CATH/);
    my $download_file = "$PfamWWWConfig::tempdir/new_gifs/download/$dol" . "_" . $id  .  "_download";
    open(_FILE, ">$download_file") ;
    print _FILE "$complete_gifs\n";
    close(_FILE);
    $complete_gifs = $dol;
  }
my  $ahref = "\"#\" onClick='w=window.open(\"$PfamWWWConfig::site/cgi-bin/Pfam/download_gif.pl?val=$complete_gifs&id=$id\", \"helpwindow\", \"width=$window_width, height=250, scrollbars=yes,resizable=yes\");w.focus();'"; 
 $self->print_html("<font size=2><NOBR><span class=normaltext>[$protein_length residues]  <A Href=$ahref><img src=$PfamWWWConfig::image_link/save_image.gif border=0></A></SPAN></NOBR></font><P> ");
#  $self->print_html("<font size=2><NOBR><span class=normaltext>[$protein_length residues] </NOBR></font> <P><BR>");


}






=head2 _print_domains_text

 Title   : _print_domains_text
 Usage   : $self->_print_domains_text( $ordered_domains, $zoom )
 Function:
    This function prints the name of each PfamA domains and its extent
    at the appropriate place underneath the image
 Args    :
    1. Ref. to a list of hash references
    2. Zoom factor
 Returns : 
 Notes :
    This function is tightly coupled to display_domains_gif, and is only (and should
    only) be called from that function. 

=cut

sub _print_domains_text {
  my ($self, $zoom_factor, $domains) = @_;

#  print "<BR><BR>\n";
    
  my(@domains) = @{$domains};

  my($outer_loop) = 0;
  while ($outer_loop <= $#domains ) {
  
    if ( ($domains[$outer_loop]->{'region'} =~ /pfamA/) || ($domains[$outer_loop]->{'reg'} =~ /ContextPfamRegion/i) || ($domains[$outer_loop]->{'region'} =~ /SCOP/) || ($domains[$outer_loop]->{'region'} =~ /CATH/) ) {

      
      $self->print_html("<IMG SRC=",$blank_name);
      $self->print_html(" HSPACE=0 ALIGN=LEFT BORDER=0 HEIGHT=2 WIDTH=");
      $self->print_html( ($domains[$outer_loop]->{'screen_beg'} -1) * $zoom_factor, ">"); 
      
      my $reg_id;

      if ( $domains[$outer_loop]->{'region'} =~ /pfamA/ ) {
	
	$reg_id =  $domains[$outer_loop]->{'reg'}->id();
	
      } elsif (  $domains[$outer_loop]->{'region'} =~ /Context/i ) {
	$reg_id =  "<Font color=#CB04F5>Context: " . $domains[$outer_loop]->{'reg'}->id();

      } else {
	if ($domains[$outer_loop]->{'region'} =~ /smart/) {
	  
	  $reg_id = "<font color=#FF0000>". $domains[$outer_loop]->{'reg'}->domain()."&nbsp;</FONT>";
	  
	} elsif( $domains[$outer_loop]->{'region'} =~ /tigr/ ) {
	  
	  $reg_id = "<font color=#E87205>" . $domains[$outer_loop]->{'reg'}->domain() ."&nbsp;</FONT>";
	  
	}
	elsif( $domains[$outer_loop]->{'region'} =~ /SCOP/ ) {
	  $reg_id = "<font color=#66CCFF>".$domains[$outer_loop]->{'region'}."&nbsp;</FONT>";
	}
	elsif( $domains[$outer_loop]->{'region'} =~ /CATH/ ) {
	  $reg_id = "<font color=#FF99CC>".$domains[$outer_loop]->{'region'}."&nbsp;</FONT>";
	}

	
      }
      
      my $beg = $domains[$outer_loop]->{'screen_beg'};
      my $end = $domains[$outer_loop]->{'screen_end'};
      
      ## CODE FOR COLOURING PFAMA NAME IN TABLE
      my $font_col = "000000";
      my ($start_b, $slash_b); 
      
      if ($domains[$outer_loop]->{'region'} =~ /pfamA/) {
	$font_col = &PfamWWWConfig::get_hex_colour($self->_colour_map($domains[$outer_loop]->{'reg'}->accession()));
	$start_b = "<B>";
	$slash_b = "</B>";

      }

	

      my $total = "<NOBR class=normaltext><B><font color=#$font_col>$reg_id</font></B>&nbsp;$beg-$end</NOBR>";
      $self->print_html("$total");
      $self->print_html("\n<BR>\n");
    }
    
    $outer_loop++;
  }
  $self->print_html("<p>\n");
}


sub _print_hmm_domain {

  my($self, $start_end, $zoom_factor, $protein_length) = @_;

  my($start, $end) = split(/~/, $start_end);

  if ($start_end =~ /[0-9]/) {
 $self->print_html( "\n<IMG SRC=$blank_name HSPACE=0 ALIGN=LEFT BORDER=0" );
	    $self->print_html( " WIDTH=", (($start - 1) * $zoom_factor) );
	    $self->print_html( " HEIGHT=5 > \n");

	    $self->print_html( "\n<IMG SRC=$overlap_name HSPACE=0 ALIGN=LEFT BORDER=0");	 
	    $self->print_html(" WIDTH=" . (2 * $zoom_factor) . " HEIGHT=10> ");

	    $self->print_html( "\n<IMG SRC=$overlap_name HSPACE=0 ALIGN=LEFT BORDER=0");	 
	    $self->print_html(" WIDTH=" . (( ($end - 2) - ($start + 2) ) * $zoom_factor) . " HEIGHT=5 >");

	    $self->print_html( "\n<IMG SRC=$overlap_name HSPACE=0 ALIGN=LEFT BORDER=0");	 
	    $self->print_html(" WIDTH=" . ( 2 * $zoom_factor) . " HEIGHT=10 >");


            $self->print_html( "<IMG SRC=$blank_name HSPACE=0 ALIGN=LEFT BORDER=0" );
	    $self->print_html( " WIDTH=",  ( ($protein_length - $end) * $zoom_factor + 10 ));
	    $self->print_html( " HEIGHT=10 > \n");

 
	$self->print_html("<NOBR><SPAN class=normaltext>[Structural Overlap]</SPAN></NOBR>");


            $self->print_html( "<IMG SRC=$blank_name HSPACE=0 ALIGN=LEFT BORDER=0" );
	    $self->print_html( " WIDTH=100");
	    $self->print_html( " HEIGHT=10 > \n\n");


 

#print "<BR><BR>";
} else {

}


}



=head2 _print_overlap_images

 Title   : _print_overlap_images
 Usage   : $self->print_overlap_images( $overlaps, $zoom );
 Function:
    Displays a series of overlap images, one for each element of
    @$overlaps; each element contains the residue position of the 
    centre of the overlap
 Args    :
    1. A ref. to an list of residue poistions of overlaps
    2. Zoom factor
 Returns : 

=cut

sub _print_overlap_images {
    my ($self, $overlaps, $zoom_factor, $protein_length) = @_;

 @$overlaps = sort { $a->{'key'} <=> $b->{'key'} } @$overlaps;

 #   print "Content-type: text/html\n\n";
 #   print "HERE NOO <P>";
   if ( @$overlaps > 0 ) {
 #    print "OVERLAPS <P>";
	my $i;
	my $curpos = 0;
	my($overlap_picture_width) = 2;
	my($overlap_picture_height) = 12;

	for( $i=0; $i < @$overlaps; $i++) {
	  my $box_text = "Overlap";
	  my($a_reg, $a_fr, $a_to, $b_reg, $b_fr, $b_to) = split(/~/, @$overlaps[$i]->{'info'});
	  my $window_text = "Overlap position " . @$overlaps[$i]->{'key'} . " between: " .  _full_sequence_names($a_reg) . " $a_fr - $a_to & ".   _full_sequence_names($b_reg) . " $b_fr - $b_to"; 
	    
	    $self->print_html( "<IMG SRC=$blank_name HSPACE=0 ALIGN=LEFT BORDER=0" );
	    $self->print_html( " WIDTH=", (@$overlaps[$i]->{'key'} - $curpos) * $zoom_factor - $overlap_picture_width/2);
	    $self->print_html( " HEIGHT=10 > \n");

	    $self->print_html("<A HREF=\"javascript:void()\" onMouseOver=\"window.status='",$window_text,"';return true\" onMouseOut=\"window.status=''\";return true;>  ");
	    $self->print_html( "<IMG SRC=$overlap_name HSPACE=0 ALIGN=LEFT BORDER=0");
	 
	    $self->print_html(" WIDTH=2 HEIGHT=12 ALT=\"$box_text\"></A>\n");


	  
	    
	    $curpos = @$overlaps[$i]->{'key'} + $overlap_picture_width / 2;
	}
	$self->print_html( "<IMG SRC=$blank_name HSPACE=0 ALIGN=LEFT BORDER=0" );
	    $self->print_html( " WIDTH=",  ($protein_length - $curpos) * $zoom_factor + 10);
	    $self->print_html( " HEIGHT=10 > \n");

	$self->print_html("<SPAN CLASS=normaltext>[Overlaps]</SPAN><BR>");
	$self->print_html( "<BR><BR>\n");
    }
}




=head2 _resolve_overlaps

 Title   : _resolve_overlaps
 Usage   : ($domlistref, $overlaplistref) = $self->_resolve_overlaps( @domains );
 Function:
    Produces two lists and returns a pair of references to each:
    1. A list of hash references corresponding to the given list, but with overlapping
       domains resolved.
    2. A list of integers indicating the residue positions of the overlaps that were resolved
 Args    :
    A list of hash references, each hash containing a PfamRegion and other stuff
 Returns : 
    A pair of list references.

=cut

sub _resolve_overlaps {
    my ($self, $dom, %other_order) = @_;

    my (@screen_overlaps, @screen_domains);

    my(@domains) = @{$dom};
    @screen_domains = @{$dom};
    my(@new_domain_order) = ();

    ## First we have to remove overlapping regions of the same domain type
    my($outer_loop_tmp) = 0;
    while ($outer_loop_tmp <= $#screen_domains ) {

	my($inner_loop) = $outer_loop_tmp + 1;	
	while ($inner_loop <= $#screen_domains ) {

	    if ( ($screen_domains[$inner_loop]->{'screen_beg'} > $screen_domains[$outer_loop_tmp]->{'screen_end'}) || ($screen_domains[$inner_loop]->{'screen_end'} < $screen_domains[$outer_loop_tmp]->{'screen_beg'}) ) {
		$inner_loop++;
	    } else {
		my $tmp_region = $screen_domains[$inner_loop]->{'region'};
		if ( ($screen_domains[$outer_loop_tmp]->{'region'}) =~ /$tmp_region/){
		  if ($screen_domains[$outer_loop_tmp]->{'reg'} =~ /HMMOtherRegion/i) {
		    $screen_domains[$inner_loop]->{'screen_beg'} = $screen_domains[$outer_loop_tmp]->{'screen_end'} + 1;

		    #### NESTED SMART DOMAINS -- end up with the START > END !! VERY BAD SO SPLICE OUT
		    if ($screen_domains[$inner_loop]->{'screen_beg'} > $screen_domains[$inner_loop]->{'screen_end'}) {
		      splice(@screen_domains, $inner_loop, 1);
		    }

		  #} elsif ($screen_domains[$outer_loop_tmp]->{'reg'} =~ /SCOP/i) {
			
				
		}else {
		  
		    ## Overlapping pfamA domains - check for nesting
		    if ( ($screen_domains[$outer_loop_tmp]->{'region'} =~ /pfamA/i) &&  ($screen_domains[$inner_loop]->{'region'} =~ /pfamA/i)){
		      my $allow_nesting = &PfamWWWConfig::test_nested_regions($screen_domains[$outer_loop_tmp]->{'reg'}->accession(), $screen_domains[$inner_loop]->{'reg'}->accession());
		      if ($allow_nesting) {		
			$screen_domains[$inner_loop]->{'nested'} = 1;
			$inner_loop++; ##dont splice out inner domain
		
		      } else {
			splice(@screen_domains, $inner_loop, 1); ## nesting not allowed so splice out
		      }
		    } else {
		      ## Not nested pfamA region so splice out inner one
		      splice(@screen_domains, $inner_loop, 1);
		    }
		  }  
		} else {
		    $inner_loop++;
		}
	    }
	}
	$outer_loop_tmp++;
    }
	


    ### Get exact positions of all the overlaps that occur
    ## Need to do this first as the next section chops up and removes domains and the exact size is lost.
    my($outer_loop) = 0;
    while ($outer_loop <= $#screen_domains ) {
	
	my($inner_loop) = $outer_loop + 1;	
	while ($inner_loop <= $#screen_domains ) {
	    
	    if (not( ($screen_domains[$inner_loop]->{'screen_beg'} > $screen_domains[$outer_loop]->{'screen_end'}) || 
		 ($screen_domains[$inner_loop]->{'screen_end'} < $screen_domains[$outer_loop]->{'screen_beg'}))) {
		my($overlap);
		if($screen_domains[$outer_loop]->{'screen_end'} < $screen_domains[$inner_loop]->{'screen_end'})   {		    
		    ($overlap) = (($screen_domains[$outer_loop]->{'screen_end'} + $screen_domains[$inner_loop]->{'screen_beg'})/2);
		    
		} else {
		    ($overlap) =  (($screen_domains[$inner_loop]->{'screen_end'} + $screen_domains[$inner_loop]->{'screen_beg'})/2);
		} #/ end IF what type of overlap
		
		my($outer_reg) = $screen_domains[$outer_loop]->{'region'} ;
		if ($outer_reg =~ /pfam/) {
		    $outer_reg = $screen_domains[$outer_loop]->{'reg'}->id();
		}
		
		my($inner_reg) = $screen_domains[$inner_loop]->{'region'} ;
		if ($inner_reg =~ /pfam/) {
		    $inner_reg = $screen_domains[$inner_loop]->{'reg'}->id();
		}
		
		my($rest) = "$outer_reg~" . $screen_domains[$outer_loop]->{'screen_beg'} . "~" . $screen_domains[$outer_loop]->{'screen_end'} . "~$inner_reg~" . $screen_domains[$inner_loop]->{'screen_beg'} . "~" . $screen_domains[$inner_loop]->{'screen_end'};
		my($regions) = $screen_domains[$outer_loop]->{'region'} . "~" . $screen_domains[$inner_loop]->{'region'};
		
		push @screen_overlaps, {
		    'key' => $overlap,
		    'info' => $rest,
		    'regions' => $regions,
		};
		
	    } #/ end IF overlap
	    $inner_loop++;
	} #/ end inner while
	
	@screen_domains = sort { $a->{'screen_beg'} <=> $b->{'screen_beg'} } @screen_domains;

	$outer_loop++;
    }


## so now we have worked out the EXACT overlap position and 
## removed any domains that overlap with domains of the same type.
	    

## Sort of the domains by priority and start choping and removing any low priority domains
## Go through each priority lowest -> highest

 foreach my $priority (reverse("0".."10")) {
	
	my($region) = $other_order{$priority};
	## Iterate through all the domains ....
	my($outer_loop) = 0;
	
	while ($outer_loop <= $#screen_domains ) {
	  ## have domain for the correct priority .
	  
	  my $con = 0;	  	  
	  if( ($other_order{$priority}) eq ($screen_domains[$outer_loop]->{'region'}) ) {
	    $con = 1;	    
	  } elsif ( ($region =~ /SCOP/) || ($region =~ /CATH/) || ($region =~ /disordered/)   ) {
	    if  (($screen_domains[$outer_loop]->{'region'}) =~ /$region/ ) {
	      $con = 1;
	    }

	  }

	  if ($con) {
	# my $reg = $screen_domains[$outer_loop]->{'region'};
	# if($screen_domains[$outer_loop]->{'region'}  =~ /$other_order{$priority}/) {
	#  print "<B>EQUALS</B><P>";
	  #if  (($screen_domains[$outer_loop]->{'region'}) =~ /$region/ )  { ## Rob's old code - dosent work

	    # already domains in new hash so do comparison
		if(defined($new_domain_order[0])) {

		    ## Check that doesn't overlap with any existing domains
		    my($outer_start) = $screen_domains[$outer_loop]->{'screen_beg'};
		    my($outer_end) = $screen_domains[$outer_loop]->{'screen_end'};
		    
		    ## iterate through inner loop
		    my($inner_loop) = 0;
		    
		    ## Dont need to check existing domain 
		    ## with all domains added as may be way down the string. If the end of 
		    ## the current domain is much less than the start of the next then dont 
		    ## need to check as there will be no overlaps !

		    my($continue) = 1;

		    while ( ($inner_loop <= $#new_domain_order ) && ($continue) ) {
			
			## Initialise that always update inner loop counter
			my($update_inner) = 1;
			
			my($inner_start) = $new_domain_order[$inner_loop]->{'screen_beg'};
			my($inner_end) = $new_domain_order[$inner_loop]->{'screen_end'};
			
			if($inner_end < $outer_start) {
			    
			} else {
			    
			    if ($inner_start > $outer_end) {				
				$continue = 0;				
			    } else {				
				## OVERLAP OCCURS !!
    
				if($inner_start >= $outer_start) {
				    
				    if($outer_end <= $inner_end) {
					# -----------         outer
					#       ---------     inner
					
					
					$inner_start = $outer_end + 1;
					$new_domain_order[$inner_loop]->{'screen_beg'} = $inner_start;
					
					@new_domain_order = sort { $a->{'screen_beg'} <=> $b->{'screen_beg'} } @new_domain_order;
					
					
					
				    } else {
					# -----------------    outer
					#       --------       inner
					
					
					splice(@new_domain_order, $inner_loop, 1);
					$update_inner = 0;
					@new_domain_order = sort { $a->{'screen_beg'} <=> $b->{'screen_beg'} } @new_domain_order;
					
				    } #/ end if outer_end < inner start
				    
				    
				} else {
				    
				    # this detects both of the following so need to distinguish further
				    
				    ## need to further detect: overlap on outer left
				    #          ------------    outer
				    #   ---------              inner
				    
				    #                           and split
				    #     ----------               outer
				    # -------------------          inner
				    
				    
				    if ($inner_end > $outer_end) {
					
					my($blee) = ($outer_end + $outer_start)/2;
					
					my($old_inner_end) = $inner_end;
					
					## ist split segment
					$inner_end = $outer_start - 1;
					$new_domain_order[$inner_loop]->{'screen_end'} = $inner_end;
					
					## 2nd split segment
					
					my($new_begin) = $outer_end + 1;
					
					my   %new_seg = (
							 'reg' => $new_domain_order[$inner_loop]->{'reg'},
							 'screen_beg' => $new_begin,
							 'screen_end' => $old_inner_end,
							 'region' => $new_domain_order[$inner_loop]->{'region'},
							 'image' => $new_domain_order[$inner_loop]->{'image'},
							 'original_screen_beg' => $new_domain_order[$inner_loop]->{'original_screen_beg'}  ,
							 'original_screen_end' =>$new_domain_order[$inner_loop]->{'original_screen_end'} ,
							 'frag_model' =>$new_domain_order[$inner_loop]->{'frag_model'} ,
							 'nested' =>$new_domain_order[$inner_loop]->{'nested'} ,
							 );
				#	print "NES: %new " .$new_domain_order[$inner_loop]->{'nested'} . " <P>";
					push @new_domain_order, \%new_seg;
					@new_domain_order = sort { $a->{'screen_beg'} <=> $b->{'screen_beg'} } @new_domain_order;
					
				    } else {
					
					$inner_end = $outer_start - 1;
					$new_domain_order[$inner_loop]->{'screen_end'} = $inner_end;
					@new_domain_order = sort { $a->{'screen_beg'} <=> $b->{'screen_beg'} } @new_domain_order;
				    } #/ end if inner_end > outer_end
				    
				}    
				
			    } # end IF overlap: inner_start > outer_end
			    
			}  #/ end IF overlap: inner_end < outer start
			
			
			## if start >= end then splice out the array element
			
			
			if ( $new_domain_order[$inner_loop]->{'screen_beg'} >= $new_domain_order[$inner_loop]->{'screen_end'}) {
			    splice(@new_domain_order, $inner_loop, 1);
			    $update_inner = 0;
			}
	       
	       
			## when total overlap we splice out the existing one: dont want to update the 
			# array counter as will miss the next one.
			# Only update counter if we want to go to next element
			$inner_loop++ if ($update_inner);	
			
			
		    } #/ end  INNER WHILE
		    
		} # end if domains already in the new array so check for overlaps
		
		
		# Resolved all overlaps so add the domain
		my %new_seg = (
			       'reg' => $screen_domains[$outer_loop]->{'reg'},
			       'screen_beg' => $screen_domains[$outer_loop]->{'screen_beg'},
			       'screen_end' => $screen_domains[$outer_loop]->{'screen_end'},
			       'image' => $screen_domains[$outer_loop]->{'image'},
			       'region' => $screen_domains[$outer_loop]->{'region'},
			       'original_screen_beg' => $screen_domains[$outer_loop]->{'original_screen_beg'}  ,
			       'original_screen_end' =>$screen_domains[$outer_loop]->{'original_screen_end'} ,
			        'frag_model' =>$screen_domains[$outer_loop]->{'frag_model'} ,
			       'nested' =>$screen_domains[$outer_loop]->{'nested'} ,
			       );
	#	print "ID: " .$screen_domains[$outer_loop]->{'region'}     . " NEST: " .$screen_domains[$outer_loop]->{'nested'} . " <P>";

		
		push @new_domain_order, \%new_seg;
		@new_domain_order = sort { $a->{'screen_beg'} <=> $b->{'screen_beg'} } @new_domain_order;
		
		##  Current domain added so remove from OUTER list 
		splice(@screen_domains, $outer_loop, 1);
		
	    } else {
		$outer_loop++;
	    } #/ end IF got correct domain priority
	    
	} #/ End outer WHILE
    } #/ end FOR

    my $nested = 1;
    
    while($nested) {
      
      my $count = 0;
      my $splice_count = undef;
      my $dom_count = @new_domain_order;
      foreach my $domain (@new_domain_order) {
	
	if ($domain->{'region'} =~ /pfamA/i) {
	  if ($count > 0) {
	    if (!$domain->{'nested'}) {
	      my $minus = $count - 1;
	      my $plus = $count + 1;
	      if ($plus < $dom_count) {
		if ( ($new_domain_order[$minus]->{'nested'}) && ($new_domain_order[$plus]->{'nested'}   )  ) {
		  $splice_count = $count;
		  last;
		  
		}
	      }


	    }
	  }
	}

	$count++;


      }
      
   
      splice(@new_domain_order, $splice_count, 1) if ($splice_count);
      $nested = 0 if (!$splice_count);
    
    }

    return (\@new_domain_order, \@screen_overlaps);
}




=head2 _next_colour_A

 Title   : _next_colour_A
 Usage   : my $col = $self->_next_colour_A();
 Function:
    The function returns the next free colour number for Pfam A domains,
    incrementing the next free number
 Args    : None
 Returns :  A colour number

=cut

sub _next_colour_A {
   my ($self) = @_;

   ## COLOR 14 to pale so skip!
   if ( ($self->{'next_colour_A'} ) eq 14) {
     $self->{'next_colour_A'}++;
   }
 
   return $self->{'next_colour_A'}++;
}




=head2 _next_colour_B

 Title   : _next_colour_B
 Usage   : my $col = $self->_next_colour_B();
 Function:
    The function returns the next free colour number for Pfam B domains,
    incrementing the next free number
 Args    : None
 Returns :  A colour number

=cut

sub _next_colour_B {
   my ($self) = @_;

   return $self->{'next_colour_B'}++;
}




=head2 _colour_map

 Title   : _colour_map
 Usage   :
 Function:
    Gets or Sets the given entry of the colour map maintained by the object
 Args    : A colour number (optional)
 Returns : A colour number (or undefined if none exists)

=cut

sub _colour_map{
   my ($self, $key, $value) = @_;
  # print "KEY: $key :: val: $value <BR>";
   if (defined($value) ) {
       $self->{'colour_map'}->{ $key } = $value;
   }
   if (exists( $self->{'colour_map'}->{$key})) {
       return $self->{'colour_map'}->{$key};
   }
   else {
       return undef;
   }

}




=head2 _paging

 Title   : _paging
 Usage   : my $page = $self->_$paging();
 Function:
    To get the Paging reference contained in the object
 Returns : A ref. to a Paging object (or undef if none present)
 Args    : 

=cut

sub _paging{
   my ($self, $value) = @_;

   if (defined $value) {
       $self->{'paging'} = $value;
   }
   return $self->{'paging'};
}





=head2 print_key


 Title   : print key for the domains
 Usage   : $self->_print_key();
 Function:
    Prints domains names and gifs as a key. Prints them in order of priority 
    and if order already selected then they are printed first with remaining 
    defaults printed after
 Args    : None
 Returns :  None


=cut

sub print_key {
  
  my($self, $params, %new_select_order) = @_;
  my $center;
  my($font_color) = "#FFFFFF";
  my($bg_color) = "#0000A0";
#  print "HERE BOO : $params<P>";
  #foreach (sort keys %new_select_order) {
  #  print "KEY: $_ , VAL: " . $new_select_order{$_} . "<BR>";
  #}

  my (%form_select_order);
  
 %form_select_order = %new_select_order;
  my(%set_select_order) = _default_other_menu(); 
  %set_select_order = reverse %set_select_order;

  my(%blee);
  foreach my $key (%new_select_order) {
    if ($new_select_order{$key} =~ /[A-Z]|[a-z]/) {
      $new_select_order{$key} =~ s/~hidden//;
      $blee{$new_select_order{$key}} = 1;
  
     
    }
    
  }

  my($key_num) = 0;
  foreach my $key (keys %set_select_order) {
    #print "key: $key <P>";
  ;
    if(defined($blee{$key})) {
      delete($set_select_order{$key});
      $key_num++
    } elsif ( ($key =~ /CATH/i) || ($key =~ /DISORDERED/i) || ($key =~ /SCOP/i)   ) {
      delete($set_select_order{$key});
    } else {
       $key_num++
    }
  }



  %set_select_order = reverse %set_select_order;

  $self->print_html("<P><P><P><P>");
  
  $self->print_html("<CENTER>") if $center;
  $self->print_html("<form>
<TABLE border=0 cellpadding=3 cellspacing=0>
<TR><TD bgcolor=#000070 CLASS=whitetableheader colspan=". ($key_num + $key_num + ($key_num - 1)) . ">Key </TD></TR> ");
  my(@params) = split(/~/, $params);
  foreach (@params) {
    my ($name, $val) = split (/=/, $_);
  #  print "NAME:$name, VAL:$val<BR>";
    print "\n<input type=hidden name=$name value=\"$val\">";
  }
  print "\n<input type=hidden name=no_key value=1>";
  my $test_ret;
  foreach (sort keys %form_select_order) {
    #print "EEP: $_ :: " . $form_select_order{$_} . " <P>";
    #print "\n<input type=hidden name=$_ value=" .$form_select_order{$_}. ">"
    if (!$test_ret) {
      $test_ret = $form_select_order{$_};
    } else {
      $test_ret .=  "," . $form_select_order{$_};
    }
  }
  print "\n<input type=hidden name=test_ret value=$test_ret>";

  $self->print_html("<TR bgcolor=#dfdff7 > ");
  
  foreach my $key (sort keys %new_select_order) {
    my $ahref = "$PfamWWWConfig::region_help";
    $ahref = "\"#\" onClick='w=window.open(\"help.pl?type=Context\", \"helpwindow\", \"width=450, height=450, scrollbars=yes,resizable=yes\");w.focus();'" if (_full_sequence_names($new_select_order{$key}) =~ /Context/i);
    
    if  ($new_select_order{$key} =~ /[A-Z]|[a-z]/ )  {
      
      $self->print_html("<TD NOWRAP colspan=2 valign=top align=center  ><A HREF=$ahref ><font size=-10 color=#000000 >". _full_sequence_names($new_select_order{$key}) . ":</font></A><BR><A HREF=$ahref ><IMG SRC=$PfamWWWConfig::image_link/" . $new_select_order{$key} . "_small.gif border=0></A></TD>") ;
      
      $self->print_html("<TD NOWRAP valign=center align=center class=boldtextsmall><B> ></B> </TD>") if ($key_num > 1);
      $key_num--;
      
      
      
    }
  }

  foreach my $key (sort keys %set_select_order) {
    #print "HERE : $key<P>";
     my $ahref = "$PfamWWWConfig::region_help";
     $ahref = "\"#\" onClick='w=window.open(\"help.pl?type=Context\", \"helpwindow\", \"width=450, height=450, scrollbars=yes,resizable=yes\");w.focus();'" if (_full_sequence_names($set_select_order{$key}) =~ /Context/i);

    $self->print_html("<TD NOWRAP valign=top align=center><A HREF=$ahref > <font size=-10 color=#000000 >" .  _full_sequence_names($set_select_order{$key})   . ":</font></A><BR><A HREF=$ahref ><IMG SRC=$PfamWWWConfig::image_link/" . $set_select_order{$key} . "_small.gif  border=0></A></TD>") ;
    
    $self->print_html("<TD NOWRAP valign=center align=centre  class=boldtextsmall><B> &nbsp; > &nbsp; </B></TD>") if ($key_num > 1);
    $key_num--;
    
 }
 
  $self->print_html("<td align=right><input type=submit value=\"Hide key\"></td></tr></TABLE></form>");
  $self->print_html("</CENTER>") if $center;
  $self->print_html("<P><P><P><P>");


}


=head2 _default_other_menu

 Title   : Returns hash array with the default domain order
 Usage   : %domain_order = $PfamWWWConfig::default_other_menu();
 Function:
        Returns the default domain order as a hash array
 Returns :
        1. Hash of domain order
 Args    :

=cut


sub _default_other_menu {
  my(%other_values) = (
		       "0" => "sig_p",
		       "1" => "pfamA" ,
		       "2" => "Context",
		       "3" => "smart",
		       "4" => "transmembrane",
		       "5" => "low_complexity",
		       "6" => "coiled_coil",
		       "7" => "pfamB",
		       "8" => "disordered",
		       "9" => "SCOP",
		       "10"=> "CATH"
                   );

#  my(%other_values) = (
#		       "0" => "sig_p",
#		       "1" => "pfamA" ,
#		       "2" => "Context",
#		       "3" => "smart",
#		       "4" => "transmembrane",
#		       "5" => "low_complexity",
#		       "6" => "coiled_coil",
#		       "7" => "pfamB"
#                    );

  return (%other_values);

}



sub _hmm_other_menu {

  my(%other_values) = (
		       "0" => "pfamA",
		       "1" => "smart",
		       "2" => "tigr",
		       "3" => "pfamB"      
		      );


  return (%other_values);

}



=head2 _full_sequence_names

 Title   : Returns full names
 Usage   : $full_name = $PfamWWWConfig::full_sequence_names($sequence_type);
 Function:
        Gets passed a shortcut domain name and if it has a prop-er extension then
this is returned, else the shortcut is passed back
 Returns :
        1. The full region name
 Args    :
   1. A regions abbreviation

=cut



sub _full_sequence_names {


  my($hash_value) = @_;

  my(%full_values) = (
                      "sig_p" => "signal peptide",
                      "low_complexity" => "low complexity",
                      "coiled_coil" => "coiled coil",
                      "transmembrane" => "transmembrane"
                      );

  if (exists($full_values{$hash_value})) {
    return  $full_values{$hash_value} ;
  }  else {
  return $hash_value;
         }

}




sub sort_the_regions {


  ## The pfama/pfamb objects have slighty different methods from other region object so have to do a bit of tweaking

  my($self, $annseq, $model_frag, $find_nested) = @_;
  
  my(@domains);
   ### Get all the model frags
  my %model_acc;
  %model_acc = $self->_get_model_frags($annseq) if($model_frag);

  foreach my $region ($annseq->eachAnnotatedRegion()) {
    
      my($add_array) = 1;
      my(%temp);
	if ( $region =~ /HMMOtherRegion/i  ) {
	  	      %temp =  ( 'reg' => $region,
			 'screen_beg' => $region->from(),
			 'screen_end' => $region->to(),
			 'region' => $region->hmm_db(),
			'domain' => $region->domain(),	 
			
			 );

      } elsif ($region =~ /Other/) {
	if (($region->source() =~ /tmpred/) && ($region->score() < 1500) ) {
	  $add_array = 0;
		}
	elsif ($region->type() =~ /SCOP:/i){
		%temp =  ( 'reg' => $region,
			 'screen_beg' => $region->from(),
			 'screen_end' => $region->to(),
			 'original_screen_beg' => $region->from(),
			 'original_screen_end' => $region->to(),
			 'region' => $region->type(),
			 'source' => $region->source(),
			 );
		}
	elsif ($region->type() =~ /CATH/i){
		%temp =  ( 'reg' => $region,
			 'screen_beg' => $region->from(),
			 'screen_end' => $region->to(),
			 'original_screen_beg' => $region->from(),
			 'original_screen_end' => $region->to(),
			 'region' => $region->type(),
			 'source' => $region->source(),
			 );
		}
	elsif ($region->type() =~ /disordered/){
		%temp =  ( 'reg' => $region,
			 'screen_beg' => $region->from(),
			 'screen_end' => $region->to(),
			 'region' => $region->type(),
			 'source' => $region->source(),
			 );

	} else {
	      %temp =  ( 'reg' => $region,
			 'screen_beg' => $region->from(),
			 'screen_end' => $region->to(),
			 'region' => $region->type(),
			 'score' => $region->score(),
			 );
	    }


      } elsif ($region =~ /ContextPfamRegion/i) {

	my($region_type) = "Context";
	  
	  %temp = ( 'reg' => $region,
		    'screen_beg' => $region->from(),
		    'screen_end' => $region->to(),
		    'region' => $region_type,
		    'score' => $region->domain_score()
		    );
      }
      

      else {
	  my($region_type);
	  if ($region->accession() =~ /^PF/) {    # pfam-A
	      $region_type = "pfamA";


	  } 
	  else {
	      $region_type = "pfamB";
	  }
	  
	  %temp = ( 'reg' => $region,
		    'screen_beg' => $region->from(),
		    'screen_end' => $region->to(),
		    'region' => $region_type,
		    'original_screen_beg' => $region->from(),
		    'original_screen_end' => $region->to(),
		    'frag_model' => $model_acc{$region->from()},
		    'nested' => 0,
		    );
      }
      
      if ($add_array) { 
	  push @domains, \%temp;
      }

      
  }
  
  @domains = sort { $a->{'screen_beg'} <=> $b->{'screen_beg'} } @domains;
 	
  if ($find_nested) {
    
    my @pfamA_nested_store;
    foreach my $reg  (@domains) {
      my $nested;
      if ($reg->{'region'} =~ /pfamA/i) {
	my $max_end;
	foreach my $prev (@pfamA_nested_store) {
	  my($start, $end) = split(/~/, $prev);
	  $max_end = $end  if ($end > $max_end);
	  if ( ($reg->{'original_screen_beg'} > $end) || ($end  < $reg->{'original_screen_beg'} ) ) {
	  } else {
	    $reg->{'nested'} = 1;
	    $nested = 1;
	  }
	  
	}
	
	@pfamA_nested_store = () if  ($reg->{'original_screen_beg'} > $max_end);
	push @pfamA_nested_store,  $reg->{'original_screen_beg'}. "~" . $reg->{'original_screen_end'} ;
      }
    }
  }
  
  
  return \@domains;


}


=head2 _get_model_frags

 Title   : _get_model_frags
 Usage   : 
 Function: Finds which PfamA regions are frags and if so then puts into a hash which is used later
       
 Returns :
        1. hash array of pfamA frag regions
 Args    :
   1. Annseq object

=cut

sub _get_model_frags {
  
  my ($self, $annseq) = @_;
  my %model_acc;
  foreach my $region ($annseq->eachAnnotatedRegion()) {
    
    
    if ( ($region =~ /PfamRegion/i) && ($region !~ /Context/i) ) {
      if ( $region->accession() =~ /^PF/   ) {
	my $frag_model;
	my $model_length = &PfamWWWConfig::model_length_by_acc($region->accession());
	
	
	my $start = "n";
	my $end = "n";
	if ($region->model_from() != 1) {
	  $start = "f";
	}
	if ($region->model_to() != $model_length) {
	  $end = "f";
	}

	if (  (!$region->model_from()) && (!$region->model_to()) ) {
	  $end = $start = "n";
	}

	if ( ($start =~ /f/) && ($end =~ /f/) ) {
	  $model_acc{$region->from()} = "both";
	} elsif  ( ($start =~ /n/) && ($end =~ /f/) ) {
	  $model_acc{$region->from()} = "end";
	} elsif ( ($start =~ /f/) && ($end =~ /n/) ) {
	  $model_acc{$region->from()} = "start";
	} else {
	  $model_acc{$region->from()} = undef;
	  
	}
	
	
	
      }
    }
    
  }
  
  return %model_acc;

 
}



=head2 _get_new_pfamA_gif

 Title   :_get_new_pfamA_gif
 Usage   : 
 Function: Passed a lot of parameterd for what the gif should look like. Checks to see if one already exists within 20% size length and if so uses it else creates a brand new one.       
 Returns :
        1. Gif object to be printed for that domain.
 Args    :
   1. Template file width, begin & end shape, frag.

=cut


sub  _get_new_pfamA_gif {
 
  my ($region ,  $colours , $width , $tmp_beg_shape, $tmp_end_shape, $frag_model) = @_;
  my $colour_gif;
  if ($colours =~ /(colors)(A\d+)(.\w\w\w)/) {
    $colour_gif = $2;
  }
  ### beginging and end shapes !
  my $beg_shape =  "n";
  my $end_shape = "n";

    $beg_shape = "l" if ($tmp_beg_shape eq "0");
    $end_shape = "r" if ($tmp_end_shape eq "0");
  

  if ($frag_model) {
    if ($frag_model =~ /start/) {
      $beg_shape = "n";
      $end_shape = "r";
    } elsif ($frag_model =~ /end/) {
       $beg_shape = "l" ;
	 $end_shape = "n";
    } elsif ($frag_model =~ /both/) {
       $beg_shape = "n" ;
	 $end_shape = "n";
    } else {
       $beg_shape = "l" ;
       $end_shape = "r";
    }
   
  }

  ##  mim size for text to appear

  #my $min_region_width =  (( (length($region) - 1) * 7) + 10); ## pfamA
   my $min_region_width;
  
  if ($colours =~ /smart\.\w\w\w$/) {
    if (  length($region) > length("Smart:") ) {
      $min_region_width =  (( (length($region) - 1) * 4) + 10);  ### smart
    } else {
      $min_region_width =  (( (length("Smart:") - 1) * 4) + 10);  ### smart
    }
  } else {
    $min_region_width =  (( (length($region) - 1) * 7) + 10); ## pfamA
  }
  
  my $gifs_dir = $PfamWWWConfig::tempdir . "/new_gifs";
  my $dir = $PfamWWWConfig::tempdir . "/new_gifs";
  
  my $diff =  int(($width/100) * 20);
  
  my($min) = $width - $diff;
  my($max) = $width + $diff;
  
  my $pfam_width = 1;
    
  my $return_gif = 0;
  
  my $temp_file;
  opendir(DIR, $dir) or die "canna open dir";
  
  while(defined(my $file = readdir(DIR))) {
    
    next if $file =~ /^\.\.?$/;
    
    my($existing_dom , $existing_col, $existing_width, $existing_l, $existing_r);
    if ($file =~ /(\S+)~(\w+)~(\d+)~(\w)~(\w).\w\w\w/) {
      $existing_dom = $1;
      $existing_col = $2;
      $existing_width = $3;
      $existing_l = $4;
      $existing_r = $5;
      
    }
	next if (!$existing_col);	
    if ( ( $region eq $existing_dom) && ($colour_gif eq $existing_col) ) {      
      if ( ( $existing_width >= $min) && (   $existing_width <= $max   ) ) {
	if ( ($existing_l =~ /$beg_shape/) && ($existing_r =~ /$end_shape/) ) {
	
	  $return_gif = $file;
	
	  last;
	}
      }
    }
  }
  


  ### Cant find a gif the correct size so make a new one !
  if ($return_gif eq "0") {

    ## new gif name
    $return_gif = $region . "~" . $colour_gif  . "~" . $width . "~" . $beg_shape ."~" . $end_shape . $PfamWWWConfig::image_ext;
  
    my $width_offset = int(  ($width - $min_region_width) /2 );
    $width_offset = 1 if($width_offset < 1);
    

    ## image name to long for the gif so dont print in then
   $region = "" if ($width < $min_region_width);
    my $new_image =  &PfamWWWConfig::drawing_generate_gif( $width, $colours, $region, $return_gif, $width_offset, $beg_shape, $end_shape);
    #my $new_image =  &generate_domain_gif( $width, $colours, $region, $return_gif, $width_offset, $beg_shape, $end_shape);
      
 



  }


  return $return_gif;
}


=head2 _make_scop_gif

 Title   :_make_scop_gif
 Usage   : 
 Function: 
 Returns :
        1. Gif object to be printed for that domain.
 Args    :
   1. Template file width, begin & end shape, frag.

=cut


sub  _make_scop_gif {
 
  my ($region ,  $image , $width , $tmp_beg_shape, $tmp_end_shape, $ori_beg, $ori_end) = @_;
  my $colour_gif = "SCOP";
  ### beginging and end shapes !
  my $beg_shape =  "n";
  my $end_shape = "n";

    $beg_shape = "l" if ($tmp_beg_shape eq $ori_beg);
    $end_shape = "r" if ($tmp_end_shape eq $ori_end);
  



  ##  mim size for text to appear
	my $min_region_width;
	$min_region_width =  (( (length($colour_gif) - 1) * 7) + 10); ## min length to print scop
  
    ## new gif name
    my $return_gif = $colour_gif."~". $colour_gif  . "~" . $width . "~" . $beg_shape ."~" . $end_shape . $PfamWWWConfig::image_ext;
  
    my $width_offset = int(  ($width - $min_region_width) /2 );
    $width_offset = 1 if($width_offset < 1);
    
    ## image name to long for the gif so dont print in then
	$colour_gif = " " if ($width < $min_region_width);

    ## image name to long for the gif so dont print in then
   	#$region = "" if ($width < $min_region_width);
	#print "******$min_region_width, $width, $image, $colour_gif, $return_gif, $width_offset, $beg_shape, $end_shape*************";
    my $new_image =  &PfamWWWConfig::drawing_generate_gif($width, $image, $colour_gif, $return_gif, $width_offset, $beg_shape, $end_shape);

  	return $return_gif;
}

=head2 _make_CATH_gif

 Title   :_make_CATH_gif
 Usage   : 
 Function: 
 Returns :
        1. Gif object to be printed for that domain.
 Args    :
   1. Template file width, begin & end shape, frag.

=cut


sub  _make_CATH_gif {
 
  my ($region ,  $image , $width , $tmp_beg_shape, $tmp_end_shape, $ori_beg, $ori_end) = @_;
  my $colour_gif = "CATH";
  ### beginging and end shapes !
  my $beg_shape =  "n";
  my $end_shape = "n";

    $beg_shape = "l" if ($tmp_beg_shape eq $ori_beg);
    $end_shape = "r" if ($tmp_end_shape eq $ori_end);
  ##  mim size for text to appear
	my $min_region_width;
	$min_region_width =  (( (length($colour_gif) - 1) * 7) + 10); ## min length to print scop
  
    ## new gif name
    my $return_gif = $colour_gif."~". $colour_gif  . "~" . $width . "~" . $beg_shape ."~" . $end_shape . $PfamWWWConfig::image_ext;
  
    my $width_offset = int(  ($width - $min_region_width) /2 );
    $width_offset = 1 if($width_offset < 1);
    
    ## image name to long for the gif so dont print in then
	$colour_gif = " " if ($width < $min_region_width);

    my $new_image =  &PfamWWWConfig::drawing_generate_gif($width, $image, $colour_gif, $return_gif, $width_offset, $beg_shape, $end_shape);

  	return $return_gif;
}


1;

