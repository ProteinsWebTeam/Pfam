#
# Perl Module for WWWAlign
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
#Copyright Genome Research Limited (1999). Please see information on licensing in LICENSE


package WWWAlign;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use Carp;
use strict;

#
# Place functions/variables you want to *export*, ie be visible from the caller package into @EXPORT_OK
#

@EXPORT_OK = qw();

#
# @ISA has our inheritance
#

use RfamWWWConfig;

@ISA = ( 'Exporter');


sub new {
    my ($class, %args) = @_;

    my ($alignment, $type, $acc) = (($args{'-alignment'}||$args{'-ALIGNMENT'}),
				    ($args{'-aln_type'}||$args{'-ALN_TYPE'}),
				    ($args{'-accession'}||$args{'-ACCESSION'}));

    my $self = { 'internal_align' => $alignment,
		 'accession' => $acc,
		 'aln_type' => $type };

    bless $self, $class;

    return $self;
}



=head2 alignment

 Title   : alignment
 Usage   : $wwwalignref->_alignment( $al );
 Function: 
   Gets/sets the internal aligment field
 Returns : 
 Args    :

=cut


sub alignment {
    my ($self, $value) = @_;

    if (defined $value) {
	$self->{'internal_align'} = $value;
    }
    return $self->{'internal_align'};
}




=head2 generate_html_alignment

 Title   : generate_html_alignment
 Usage   : $wwwalignref->generate_html_alignment( $cgiRef )
 Function: 
    Process the alignment option stroed in $cgiRef argument, sets up the alignment
    to accord, and then prints it to $file
 Returns : 
 Args    :

=cut

sub generate_html_alignment {
    my ($self, $file, $query, $header, $acc, $file_num) = @_;
   # print "Content-type: text/html\n\n";
   # print " $file, $query, $header, $acc, $file_num <P>";
    my %hash;

    my $aln = $self->alignment();

    my $aln_name = $query->param('align');
    my $aln_type = $query->param('type');
    $acc = $query->param('acc') if (!$acc);

    my $name = $aln->id();
   # print "NAME: $name , type: $aln_type, $aln_name<P>";
    # Process other options on the alignment

    # gap characters

    $_ = $query->param('gap');
    SWITCH : {
	/point/ && do {
	    $aln->map_chars('\-', '.');
	    last SWITCH;
	};
	/dash/ && do {
	    $aln->map_chars('\.','-');
	    last SWITCH;
	};
	/none/ && do {
	    $aln->map_chars('\.',"");
	    $aln->map_chars('\-', "");
	    last SWITCH;
	};
    }


    # names 

    $_ = $query->param('nametype');
    SWITCH : {
	/bydomain/ && do {
	    $aln->set_displayname_count();
	    last SWITCH;
	};
	/byflat/ && do {
	    $aln->set_displayname_flat();
	    last SWITCH;
	};
    }

    # sequence case

    $_ = $query->param('case');
    SWITCH : {
	/uppercase/ && do {
	    $aln->uppercase();
	    last SWITCH;
	};
	/asgiven/ && do {
	    last SWITCH;
	};
    }

    # order (!)

    $_ = $query->param('order');
    SWITCH : {
	/alpha/ && do {
	    $aln->sort_alphabetically();
	    last SWITCH;
	};
    }


    # ok - the major switch around the formats
    
    $_ = $query->param('format');

    SWITCH : {
	/jalview/ && do {
	    print $file "Content-type: text/html\n\n";
	    $self->write_jalview( $file );
            last SWITCH;
        };
        /belvu/ && do {
            print $file "Content-type: application/x-alignment-stockholm\n\n";
	    print $file "# STOCKHOLM 1.0\n";
            $aln->write_stockholm($file);
            last SWITCH;
	};
	/belold/ && do {
            print $file "Content-type: application/x-alignment-selex\n\n";
            $aln->write_rfam($file);
            last SWITCH;
	};
	(/^linkswisspfam$/ || /^link$/ ) && do {
	    # link to swisspfam
	    print $file "Content-type: text/html\n\n";
#	    print $file "Content-type: text/html\n\n";
#	print "$header , $name, $acc <BR>";
            print $file &RfamWWWConfig::header( $header , $name, $acc);

            print $file "<pre>";
	    $self->write_stockholm_hyper($file, $acc, $file_num);
            print $file "</pre>";
            print $file &RfamWWWConfig::footer();
	    last SWITCH;
	};
	/mul/ && do {
            # Plain rfam format
# print $file "Content-type: text/plain\n\n";
	    print $file "Content-type: text/html\n\n";
	    print $file "<pre>\n";
	    $aln->write_rfam($file);
	    print $file "</pre>";
	    last SWITCH;
	};
	/fal/ && do {
            # fasta format
#	    print $file "Content-type: text/plain\n\n";

	    print $file "Content-type: text/html\n\n";
	    print $file "<pre>\n";
	    $aln->write_fasta($file,1);
	    print $file "</pre>";
	    last SWITCH;
	};
	/msf/ && do {
            # MSF format
#	    print $file "Content-type: text/plain\n\n";
	    print $file "Content-type: text/html\n\n";
	    print $file "<pre>\n";
	    $aln->write_MSF($file);
	    print $file "</pre>";
	    last SWITCH;
	};
	/stock/ && do {
#	    print $file "Content-type: text/plain\n\n";
	    print $file "Content-type: text/html\n\n";
	    print $file "<pre>\n";
	    $aln->write_stockholm($file);
	    print $file "</pre>";
	    last SWITCH;
	};
    }
}


=head2 write_jalview

 Title   : write_jalview
 Usage   : $align->write_jalview( $file );
 Function:
    Generates the necessary html to create the jalview java applet with the 
    correct aligmnent as the value
 Returns : 
 Args    :

=cut

sub write_jalview{
    my $self = shift;
    my $file  = shift;

    my ($name, $namestr, $seq, $seqstr, $numseqs, $count);

    my $aln = $self->alignment();

    # map gap chars to dashes
    $aln->map_chars('\.','-');
    $name = $aln->id();

    print $file "<html><title>Alignment for $name</title><body>Please wait while Alignment viewer loads from the site<p>\n";
    print $file "<APPLET archive=\"$RfamWWWConfig::jaljar\" code=\"jalview.ButtonAlignApplet\" width=250 height=40>" ;
    print $file "<param name=\"mailServer\" value=\"$RfamWWWConfig::mailserver\">";
    print $file "<param name=\"srsServer\" value=\"$RfamWWWConfig::srsserver\">";
    print $file "<param name=\"database\" value=\"$RfamWWWConfig::basedb\">";

    $numseqs = $aln->each_seq();
    print $file "<param name=\"numseqs\" value=$numseqs>";

    $count = 1;
    foreach $seq ( $aln->each_seq() ) {
	$namestr = $aln->displayname($seq->get_nse());
	$seqstr = $seq->seq();
	print $file "<param name=seq$count value=$seqstr>";
	print $file "<param name=\"id$count\" value=$namestr>";
	$count++;	
    }

    # <param name=input value="$PfamWWWConfig::getalign?name=$aln_name&acc=$acc&type=$aln_type&format=fal&gap=dash"> 
    # <param name=type value="URL"> 
    # <param name=format value="FASTA"> 
    print $file "</APPLET>\n";
    print $file <<EOF;
<p>
[<a href=\"$RfamWWWConfig::WWW_root\">Back to Rfam Home Page</a>]
<p>
Jalview is a java-based alignment viewer. To use it you need Internet Explorer 4 or greater or
Netscape 4.05 or greater.<p>
To start the viewer, press the button above and wait while the alignment loads. Large alignments
may take a while to load
<p>
When the viewer is running you have a number of options
<ul>
<li>To use a larger font, choose the font menu
<li>There are a number of colouring schemes in the colour menu
<li>Right mouse click on the names brings up a new browser window with annotation on that sequence
</ul>
<p>
Jalview can do local analysis of the alignment on your machine. Beware
- if your machine is struggling in displaying the alignment then it
will take a while to do these analysis locally. However, if you have a
fast machine with alot of memory, then this is a very useful way of
gaining more information about your family.
<p>
The analysis routines are laid out in the calculate menu
<p>
Here is a suggested analysis
<ul>
<li>Make a average distance tree using PID (percent idenity) [calculate menu]
<li>Click on 'sort by tree order' [calculate menu]
<li>Right click in the tree window to drop a line to colour sub-families 
<li>Choose levels of conservation in the tree to see what residues are conserved
in some subfamilies but not others
</ul>
<p>
Finally the principle component analysis looks really cool (though we are not completely
sure what it means, nor if it gives more information than the tree). Once you have the 
tree window, click on calculate principle components, and have some fun.
<p>
The following features are not working well in the current version of jalview
<ul>
<li>Getting the sequence features work, but its display can be confusing
<li>Editing the alignment works, but ideally one wants
<ul>
<li>Editing by cursors movement
<li>Good group based editing
</ul>
</ul>
Any other comments are very welcome. Get in touch with <a href="mailto:michele\@ebi.ac.uk">Michele Clamp</a>
</body>
EOF

}



sub write_stockholm_hyper {
  my ($self, $out, $acc, $file_num) = @_;

  my $file_want = $acc . ".full." . $file_num . ".gz";
  my $input_dir = "$RfamWWWConfig::data_root" . "/markup_align/";

  $input_dir = $input_dir . $self->{'aln_type'};
#print " INOUT: $input_dir :: $acc, $file_num <P>";
   # print "OUT: $input_dir \n";


  my $count = 0;

  my $complete_file;
  opendir(_WHOLE, "$input_dir") || die("Could not open $input_dir $!");

  foreach my $file ( readdir(_WHOLE) ) {

 #   print "$file eq $file_want <br>";

    if ($file eq $file_want) {
      $complete_file = $input_dir . "/" . $file_want;
 #     print "FILE: $file <BR>";    
    }


    if ($file =~ /$acc/) {
    #  print "FILE: $file <BR>";
      $count++;

    }


  }
  

# $count = 6;

  if ($count > 1) {

    print $out "<form METHOD=\"GET\" ACTION=getalignment.pl>";

    print $out "<input type=hidden name=acc value=$acc>";
    print $out "<input type=hidden name=type value=" . $self->{'aln_type'}. ">";
    print $out "<input type=hidden name=format value=link>";
  #  print $out "<input type=hidden name=file_num value=$file_num>";

    my $first = 1;
    my $end = 500;
    print $out "To view other sequencs: <select name=file_num>";
    my $start = 1;


    
    while($start <= $count) {
      #   print "START: $start :: end: $end ";
      my $selected = "";
      $selected = " SELECTED " if ($start eq $file_num);
      print $out "<OPTION value=$start $selected>Sequences: " .$first . "-". $end ."</OPTION>";
      $first = $first + 500;
      $end = $end + 500;
      
      $start++;
    }
    
    print $out "COUNT: $count <P>";

    print $out "</SELECT><input type=submit value=\"View Sequences\"></form>";
    print $out "<P><P>\n\n";

  }


  open(_FILE, "gunzip -c $complete_file |");
  while(<_FILE>) {
    print $out $_;
    
  }
  
  close(_FILE);
  


}



=head2 write_stockholm_hyper

 Title   : write_stockholm_hyper
 Usage   : $wwwalign->write_stockholm_hyper( $file );
 Function:
    Generates the necessary html to create the jalview java applet with the 
    correct aligmnent as the value
 Returns : 
 Args    :

=cut

sub old_write_stockholm_hyper {
    my ($self, $out, $acc) = @_;

    my $aln = $self->alignment();
    my $type = $self->{'aln_type'};
    print "TYPE: $type <P>";
#     print "LEN: " . $aln->maxdisplayname_length(). " <P>";
    my $len = $aln->maxdisplayname_length() + 3;
   
    foreach my $seq ( $aln->eachSeq() ) {
	my $name = $seq->id();
	my $disname = $aln->get_displayname($seq->get_nse());
#	my $namelink = sprintf("<a href=$PfamWWWConfig::swisspfam?name=%s&acc=$acc><font face=courier>%s</font></a>",
 
#	my $namelink = sprintf("<a href=$PfamWWWConfig::swisspfam?name=%s&acc=$acc STYLE=\"font-family: courier; font-size: 11pt;\">%s</a>",       
       
	my $namelink = sprintf("<a href=$RfamWWWConfig::swisspfam?name=%s&acc=$acc>%s</a>",		
			       $seq->id,
			       $disname );

#	if ($namelink =~ /UserSeq/i) {
#		$namelink = sprintf("<font face=courier>%s</font>", 
#		$seq->id,
#	$disname );
#	}

#	my $acclink = sprintf("<a href=$PfamWWWConfig::swisspfam?name=%s&acc=$acc><font face=courier>%s</font></a>", 
#	my $acclink = sprintf("<a href=$PfamWWWConfig::swisspfam?name=%s&acc=$acc STYLE=\"font-family: courier; font-size: 11pt;\">%s</a>", 

	my $acclink = sprintf("<a href=$RfamWWWConfig::swisspfam?name=%s&acc=$acc>%s</a>", 
			      $seq->acc,
			      $seq->acc);
	my $spaces = " " x ($len - length($disname)); 

#	print $out sprintf("%s%s%s %s\n", $namelink, $spaces, $seq->str(), $acclink);


	my $sec_struc;
	my $new_seq_str = $seq->str();
	if ($seq->sec_struct) {
	   $sec_struc = $seq->sec_struct->display;

	  my @colours = ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j");

	   my @font_colours;
	  open(_COL, "/nfs/WWW/htdocs/Pfam/data/hex_colours");
	  while(<_COL>) {
	    my($num, $color, @junk) = split(/~/, $_);
	    push @font_colours, $color;

	  }

	  close(_COL);
	   
		
	  my $old = $sec_struc;
	  
	  ## work out loops and covarence
	  my $length = length $sec_struc ;
#	  print "LEN: $length <P>";
	  my $start = 0;
	  my $count = 0;
	  my $for_prev = 0;
	  my $back_prev = 0;
	  my %new_block_track;
	  my (%for_storage, %back_storage);
	  my $new_block = 0;

	  my %text_colour;

	   my $font_color = 0;

	   my @arr;
	   my @txt_arr;
	   my $first = 1;

	    while($start <= $length) {
	      my $sub = substr($sec_struc, $start, 1);

	      my $txt_sub =  substr($new_seq_str, $start, 1);

	      if ($sub eq ">") {
		$count++ if(!$back_prev);
		$for_storage{$count} = $start;
	#	print "$sub: $start :: count: $count :: stor: " .$for_storage{$count} .  " <BR>";

		if ( ($back_prev) || ($first) ) {
		  $new_block_track{$count} = $count;
	#	  print "TRACK: " .$new_block_track{$count}  ." <BR>";
		  $first = 0;
		}

		if ($back_prev) {
		  $new_block++;
		  $font_color = 0;
		 # $new_block{$count} = $count;
		  
		}
		$for_prev = 1;
		$back_prev = 0;

	      }	elsif ($sub eq "<") {
		$count-- if (!$for_prev);
		$back_storage{$count} = $start;
		

		if ($for_prev) {
		#  $new_block = 1;
		}


#		$arr[$for_storage{$count} ] =  "<font color=#" . $colours[$new_block] . "><B>></B></font>";
		$arr[$for_storage{$count} ] =  "<b ID=\"" . $colours[$new_block] . "\">" .$arr[$for_storage{$count} ] . "</b>";

#		$sub = "<font color=#" . $colours[$new_block] . "><b><</b></font>";
		$sub = "<b ID=\"" . $colours[$new_block] . "\"><</b>";


	#	$txt_sub = "<font color=#" . $colours[$new_block] . "><b>$txt_sub</b></font>";
		$txt_sub = "<b ID=\"" . $colours[$new_block] . "\">$txt_sub</b>";

#		$txt_arr[$for_storage{$count}] =  "<font color=#" . $colours[$new_block] . "><B>" . 	$txt_arr[$for_storage{$count}]. "</B></font>";
		$txt_arr[$for_storage{$count}] =  "<b ID=\"" . $colours[$new_block] . "\">" . 	$txt_arr[$for_storage{$count}]. "</b>";


		$font_color++;

	#	print "NEW: $sub <BR>";
		if (defined($new_block_track{$count})) {
		  $new_block_track{$count} = undef;
		   $new_block++;
		  $font_color = 0;
		}
		$for_prev = 0;
		$back_prev = 1;
	      } else {
		## default
		$sub = "<b>$sub</b>";
		
	      }
	      

	      push @arr, $sub;
	      push @txt_arr, $txt_sub;

	      $start++;

	    }
	  


	  my $tmp;
	  foreach (@arr) {
	    $tmp = $tmp . "$_";
	    
	  }
	  $sec_struc = $tmp;

	   my $tmp;
	   foreach (@txt_arr) {
	     if ($_ !~ /ID/) {
#	       print "$_ :: ";
	       $_ = "<b>$_</b>";
	     }

	     $tmp = $tmp . "$_";
	   }

	   $new_seq_str = $tmp;
	  
	  
	#  print $out sprintf("%-${len}s%s %s\n", "SS", $sec_struc, "SS");
	  #  print $out sprintf("%-${len}s%s %s\n", "SS", $old, "SS");
	#  exit(0);
	  
	}


	
	print $out sprintf("%s%s%s %s<br>", $namelink, $spaces, $new_seq_str, $acclink);
#	print $out "$namelink $spaces $new_seq_str $acclink<BR>";
	
	if ($seq->sec_struct) {
	  
#	  print $out sprintf("%-${len}s%s %s<br>", "SS", $sec_struc, "SS");

	  }
	
	
	if ($seq->surf_access) {
	  print $out sprintf("%-${len}s%s %s\n", "SA", $seq->surf_access->display, "SA");
	}
	if ($seq->active_site) {
	  print $out sprintf("%-${len}s%s %s\n", "AS", $seq->active_site->display, "AS");
	}
      }
    if ($aln->cons_sec_struct) {
      print $out sprintf("%-${len}s%s %s\n", "SS_cons", $aln->cons_sec_struct->display, "SS_cons");
    }
    if ($aln->cons_surf_access) {
      print $out sprintf("%-${len}s%s %s\n", "SA_cons", $aln->cons_surf_access->display, "SA_cons");
	}
    if ($aln->cons_active_site) {
      print $out sprintf("%-${len}s%s %s\n", "AS_cons", $aln->cons_active_site->display, "AS_cons");
    }
    
      }
    
    
    1;
