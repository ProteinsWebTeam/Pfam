#
# Module for Entry2WWW
#
#
#
# 
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME




=cut


package EntryWWW;
use vars qw(@ISA @EXPORT_OK);
use strict;
use Carp;

use RfamWWWConfig;
use Text::Wrap;

@ISA = qw(Exporter);



########### constants...

my $entrycolumns = 100;
my $medlinelink = "http://www3.ncbi.nlm.nih.gov/htbin-post/PubMed/wgetart";


sub new {
  my($class, %params) = @_;
  my($entry) = ( ($params{'-entry'}||$params{'-ENTRY'}));

  my $self = { 'the_entry' => $entry };

  bless $self, $class;


  return $self; # success - we hope!
}



=head2 entry

 Title   : entry
 Usage   : $en = $en2www->entry();
 Function: 
    Every Entry2WWW instance has an associated Entry, and here is how to get it
 Returns : Bio::Pfam::Entry
 Args    : Bio::Pfam::Entry (optional)


=cut

sub entry {
    my ($self, $value) = @_;

    if (defined $value) {
	$self->{'the_entry'} = $value;
    }

    return $self->{'the_entry'};
}



sub write_table {
    my $self = shift;
    my $fh = shift;
    my $entry = $self->entry();
#	print "ENTRY DEAD: " . $entry->is_dead(). " <BR>";

    if (not defined $entry) {
	&RfamWWWConfig::exit_html("Error: There is no entry to write");
    }


    ## Get the pdb stuff
    my @tmp_pdb;
 

     print "<table border=1 bgcolor=$RfamWWWConfig::rfamcolour cellpadding=5 cellspacing=0 cols=2 width='100%'>\n";
      

     
      print $fh "<TR VALIGN=TOP>";
     
   
      print $fh "<TD WIDTH=200 CLASS=normaltext  ALIGN=LEFT VALIGN=TOP><A href=$RfamWWWConfig::image_link/families/" .$entry->acc() . ".jpg><img src=$RfamWWWConfig::image_link/families/tn_" .$entry->acc() . ".jpg height=200 width=200 border=0></A><BR>Consensus secondary structure for family <B>" . $entry->id(). "</B>.  Click the picture for an enlarged image, or <a href=\"$RfamWWWConfig::WWW_root/help/software.shtml#ViennaRNA\">here</a> for more information.</TD>" ;



    print "<td><table border=0 bgcolor=$RfamWWWConfig::rfamcolour cellpadding=3 cols=1 width='100%'>\n";
  
	my $source = "RCS";
	if($entry->from_rdb()) {
		$source = "RDB " . $RfamWWWConfig::rdb_name;
	} 
    print "<tr><td COLSPAN=2 ALIGN=CENTER bgcolor=#000070 class=whitetableheader><B><I>Accession number:</B></I> <B>", $entry->acc(), "</B></td></tr>\n";

    print "<table border=0 bgcolor=$RfamWWWConfig::rfamcolour cellpadding=3 cols=1 width='100%'>";
    if (not $entry->is_dead()) {
      if (defined $entry->previous_ids()) {
	    print $fh "<tr><td>Previous identifiers: ", $entry->previous_ids(), "</td></tr>\n";
	}

      print $fh "<tr><TD><B CLASS=normallargetext>", $entry->description(), "</B></td></tr>\n"; 
    
    }	
    else {

	print $fh "</tr><td><b>This family has been removed from Rfam</b></td></tr>\n";
	print $fh "</TABLE>";
	print $fh "</td></tr></TABLE>";
	print $fh "<P><P><P><P>";
	return 1;
	
    }


print $fh "<TR><TD VALIGN=MIDDLE CLASS=normaltext>\n" ;

    if ($entry->ann()->get_Annotations('comment')) {
      
     
      foreach my $line ( $entry->ann()->get_Annotations('comment')) {
	print $fh $line->text()  ;
      } 
          
      print $fh "</TD></TR>";

      
      

 

  } #/ end of PFAM annotation

      



    
  
    print "</table>\n";
   print"</TD></TR></TABLE>" if (!@tmp_pdb);
    # next the main user interaction point


    if (not $entry->is_dead() ) {
      print "<p>";

#### New table design !! 
	print $fh "
	<table CLASS=normaltext align=center border=1 cellpadding = 5 cellspacing=0 cols=2 bgcolor=$RfamWWWConfig::rfamcolour >

<tr CLASS=whitetableheader>  

<td valign=top CLASS=whitetableheader CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Alignment </TD>
<td valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Member sequences</TD>
<td valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Species Distribution</TD>
</TR>";

	$self->write_user_interaction_table( $fh );

	$self->write_species_table( $fh );

	print $fh "</tr></table>";


	$self->write_links_table( $fh );
	

	print $fh "<P><TABLE border=0 WIDTH=100%
           <TR>";
	# now, the references and links
	$self->write_references_table( $fh);


	# now the Rfam specific fields, but only for entries that are alive
	
	$self->write_Rfam_table( $fh );

	print $fh "</TR></TABLE>";

	print $fh <<"EOF";
<p>
<span class=normaltext>For help on making stable links to this page <a href="$RfamWWWConfig::stable_link">click here</a></span>
EOF
    }
}



sub write_Rfam_table {
    my ($self, $fh) = @_;
    
    my $entry = $self->entry();
  
 #   print "<center><h3>Pfam specific information</h3></center>\n";
	print $fh "<TD VALIGN=TOP>";
    print $fh "<table WIDTH=100% border=1 CLASS=normaltext bgcolor=$RfamWWWConfig::rfamcolour cellpadding=5 cellspacing=0 cols=2 >\n";
	print $fh "<TR><TD colspan=2 valign=top align=center CLASS=whitetableheader bgcolor=000070>Rfam specific information</TD></TR>";
    print $fh "<tr><td >Author of entry</td><td>", $entry->author(), "</td></tr>\n";
      print $fh "<tr><td >Type</td><td>", $entry->entry_type(), "</td></tr>\n";
    print $fh "<tr><td>Source of seed alignment</td><td>", $entry->source(), "</td></tr>\n";
    print $fh "</td></tr>";
    print $fh "<tr><td >Gathering cutoff</td><td NOWRAP> ", $self->chop_cutoff($entry->gathering_cutoff()), "</TD></tr>\n";
   

    print $fh "<tr><td>Trusted cutoff</td><td NOWRAP>", $self->chop_cutoff($entry->trusted_cutoff()), "</TD></tr>\n";
    print $fh "<tr><td>Noise cutoff</td><td NOWRAP>", $self->chop_cutoff($entry->noise_cutoff() )," </TD></tr>\n";
    print $fh "<tr><td valign=top NOWRAP>Build method of CM</td><td NOWRAP valign=top >";

### TAKEN OUT TEMP
    my $count = 0;
   foreach my $build ($entry->each_build_line()) {
    last if ($count =~ /2/);
	print $fh "$build<br>";
	$count++;
    }
    print $fh "</td></tr>";
    print $fh "</table>";


    print $fh "</TD>";

}


sub chop_cutoff {
  my($trimmed_cutoff);
  my ($self, $cutoff ) = @_;

#  chop($cutoff);
#  chop($cutoff);
#  chop($cutoff);

#  print $fh " CUTOFF: $cutoff ";

  return $cutoff;
}



=head2 write_user_interaction_table

 Title   : write_user_interaction_table
 Usage   :
 Function: 
    Writes the alignment and get all proteins forms in 
    funky new format
 Example :
 Returns : 
 Args    :

=cut

sub write_user_interaction_table {
  my $self = shift;
  my $fh = shift;
  
  my ($id, $acc, $fcount,$scount);
  
  my $entry = $self->entry();
  if (not defined $entry) {
    &RfamWWWConfig::exit_html("Error: There is no entry to write");
  }

  $id = $entry->id();
  $acc  = $entry->acc();
  
  $scount = $entry->num_seqs_in_seed();
  $fcount = $entry->num_seqs_in_full();
  
  # print $fh <<EOF;


print $fh qq (
<TR ><TD VALIGN=TOP>
<table border=0 cellpadding=0 cellspacing=0 cols=1>


<tr>
<td CLASS=normaltext>
<form METHOD="GET" ACTION="$RfamWWWConfig::getalignment">
<input name="name" type="hidden" value="$id">
<input name="acc" type="hidden" value="$acc">
<center>
<input name=type type=radio checked value=seed> Seed ($scount)&nbsp;
<input name=type type=radio value=full> Full ($fcount)<br><br>
</center>
Format <select name=format>
<option value=link> Coloured Blocked alignment
<option value=mul> Plain Rfam format
<option value=stock> Plain Stockholm format
<option value=fal> Fasta format (with gaps)
<option value=msf> MSF format
<option value=belvu> Belvu (unix only)
<option value=belold> Belvu (old version unix only)
<option value=jalview> Jalview (Java)
</select>
<P>
<input type=submit value="Get alignment">
</form>
</td>
</tr>

<tr valign=top>
<td  CLASS=normaltext>
Help relating to Rfam alignments <a href="$RfamWWWConfig::align_help">here</a>
</td>
</tr>
</table>
</td>

<td valign=top>
<table  border=0 cellpadding=0 cellspacing=0 cols=1>


<tr>
<td  CLASS=normaltext VALIGN=TOP>
<form METHOD="GET" ACTION="$RfamWWWConfig::membersequences">
<input name="name" type="hidden" value="$id">
<input name="acc" type="hidden" value="$acc">
<input name="verbose" type="hidden" value="true">
<center>
<input name=type type=radio checked value=seed> Seed ($scount)&nbsp;
<input name=type type=radio value=full> Full ($fcount)


);

#my $context_count;
#$context_count = &RfamWWWConfig::context_regions($acc, 1);
#if ($context_count) {
#print qq(
#<input name=type type=radio value=context><A HREF=\"#\" onClick='w=window.open(\"help.pl\", \"helpwindow\", \"width=450, height=410, scrollbars=yes,resizable=yes\");w.focus();'> Context</A> ($context_count)
#);
#}

print $fh qq (


<br>

<input type=submit name="list" value="View Members">
</center>
</form>

</td>
</tr>
</table>
</td>
<P>

);

#EOF

}


=head2 write_references_table

 Title   : write_references_html
 Usage   : $ann2www->write_references_table( $file );
 Function: Prints out literature references in swanky new table format
 Args    : Destination file handle


=cut

sub write_references_table {
    my ($self, $fh) = @_;

    my $entry = $self->entry();
 
    if ($entry->ann->get_Annotations('reference'))  {
     
      print $fh "<TD VALIGN=TOP >";
      print $fh "<table border=1 CLASS=normaltext bgcolor=$RfamWWWConfig::rfamcolour cellpadding=5 cellspacing=0 cols=1 WIDTH=100% HEIGHT=100%>\n";
      print $fh "<TR ><TD valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>Literature References</TD></TR>";
      my $count = 1;
      foreach my $ref ( $entry->ann()->get_Annotations('reference')) {
	
	print $fh "<tr><td CLASS=boldtextsmall HEIGHT=100%>\n";
	print $fh $self->get_medline_link( $ref->medline(), "$count. "), " ";
	print $fh "<br>\n";
	print $fh " ", $self->get_medline_link( $ref->medline(), $ref->title()), "<br>\n";
	print $fh $ref->authors, "<br>\n";
	print $fh $ref->location(), "\n";
	print "</td></tr>\n";
	$count++;
      }
      print "</table>\n";
      print "</TD>";
    }
}



=head2 write_links_table

 Title   : write_links_html
 Usage   : $ann2www->write_links_table( $file );
 Function: Prints database references in swanky new table format
 Args    : Dest. file handle


=cut

sub write_links_table {
    my ($self, $fh) = @_;

    my $entry = $self->entry();
    
    ### If there is a dblink Annotation object then print it out
    # At the moment they are all urls with no comments
    if ($entry->ann()->get_Annotations('dblink')) {
      
      # now to print ou the table
	
      print $fh "<P><table border=1 CLASS=normaltext cellpadding = 5 cellspacing=0 bgcolor=$RfamWWWConfig::rfamcolour cellpadding=5 cols=2 width='100%'>\n";
      
      print $fh "<tr><td colspan=2 valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>Database References </TD></TR>\n";


      foreach my $link ( $entry->ann()->get_Annotations('dblink')) {
	
	print $fh "<tr>\n<td><SPAN CLASS=normallargetext> ", $link->database(), " </SPAN>";
	
	print $fh "</td>\n<td><code>", $self->get_database_link($link), "</code></td>\n</tr>\n";
	
      }
      
      
      
      print "</table>\n";

    } #/ end if DB_LINK ANN OBJECT
}




=head2 write_species_table

 Title   : write_species_html
 Usage   : $ann2www->write_species_table( $file );
 Function: Prints out 'species distribution' control in swanky new table format
 Args    : Destination file handle


=cut

sub write_species_table {
    my ($self, $fh) = @_;

    my $entry = $self->entry();

 #   print $fh "<P>";
 #   print $fh "<table   border=1 cellpadding = 5 cellspacing=0 bgcolor=$RfamWWWConfig::rfamcolour cellpadding=5 cols=1 width='100%'>\n";
 #   print $fh "<tr><td valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>Species Distribution</TD></TR>";
  #  print $fh "<tr><td CLASS=normaltext>\n";

 print $fh "<td CLASS=normaltext>\n";
    print $fh "<center>\n";
    print $fh "<form method='get' action=\"$RfamWWWConfig::speciesdist\">\n";
    print $fh "<input name=\"acc\" type=\"hidden\" value=\"", $entry->acc, "\">\n";
    print $fh "<input name=\"id\" type=\"hidden\" value=\"", $entry->id, "\">\n";
    print $fh "Tree depth :<select name=depth>\n";
    print $fh "<option value=\"all\"> Show all levels\n";
    print $fh "<option value=\"1\"> 1 level\n";
    print $fh "<option value=\"2\"> 2 levels\n";
    print $fh "<option value=\"3\"> 3 levels\n";
    print $fh "<option value=\"4\"> 4 levels\n";
    print $fh "<option value=\"5\"> 5 levels\n";
    print $fh "</select>\n";
    print $fh "<BR><input type=\"submit\" value=\"View Species Tree\">\n";
    print $fh "</center>\n";
  #  print $fh "</form></td></tr></table>\n";
 print $fh "</form></td>\n";

}



=head2 get_medline_link

 Title   : get_medline_link
 Usage   : $link = $self->get_medline_link( $id, $disname);
 Function:
 Example :
 Returns : 
 Args    :

=cut

sub get_medline_link {
    my $self = shift;
    my $id = shift;
    my $name = shift;

    if (not defined $name) {
	$name = $id;
    }

    $id =~ s/;//g;

    return "<a href=\"$medlinelink?type=medline&display=abstract&id=$id\">$name</a>";
}




=head2 sub_reference_line

 Title   : sub_reference_line
 Usage   : $newline = $self->sub_reference_line($oldline);
 Function:
 Example :
 Returns : 
 Args    :

=cut

sub sub_reference_line {
    my $self = shift;
    my $line = shift;
    my $family_acc = shift;
    my ($id,$acc,$temp,$ef);
    if ( $line =~ /Pfam\:(\w+\d+)/) {
	my $db = &RfamWWWConfig::get_database();
	while( $line =~ /Pfam\:(\w+\d+)/ ) {
	    $acc = $1;
	    eval {$id = $db->acc2id( $acc );};
	    if ($@) {	
		carp "Could not find accession $acc";
		$line =~ s/Pfam:$acc/[Unknown acc] Pfam:$acc/;
		last;
	    } 
	    else {
		$line =~ s/Pfam:$acc/<a href=\"$RfamWWWConfig::getacc?$acc\">$id<\/a>/;
	    }
	}
    }

    while( $line =~ /SWISSPROT\:(\w+\d+)/ ) {
	$acc = $1;
	$ef = &RfamWWWConfig::protein_sequence($acc);

	if (defined $ef and defined $ef->id()) {
	    $id = $ef->id();
	    $line =~ s/SWISSPROT:$acc/<a href=\"$RfamWWWConfig::swisspfam?name=$acc&acc=$family_acc\">$id<\/a>/;
	}
	else {
	    last;
	}
    }


    while( $line =~ /Swiss\:(\w+\d+)/ ) {
	$acc = $1;
	$ef = &RfamWWWConfig::protein_sequence($acc);

	if (defined $ef and defined $ef->id()) {
	    $id = $ef->id();
	    $line =~ s/Swiss:$acc/<a href=\"$RfamWWWConfig::swisspfam?name=$id&acc=$family_acc\">$id<\/a>/;
	}
	else  {
	    last;
	}
    }

    while( $line =~ /EC:(\d+\.\d+\.\d+\.\d+)/) {
	$id = $1;
	my $primary_ec_link = RfamWWWConfig::link_mapper()->{'EC'}->[0]->{'link'};
	eval("\$temp = \"$primary_ec_link\"");
	$line =~ s/EC:$id/EC:<a href=$temp>$id<\/a>/;
    }

    while( $line =~ /PROSITEDOC:(\w+\d+)/) {
	$id = $1;
	my $prosite_link = RfamWWWConfig::link_mapper()->{'PROSITE'}->[0]->{'link'};
	eval("\$temp = \"$prosite_link\"");
	$line =~ s/PROSITEDOC:$id/<a href=$temp>$id<\/a>/;
    }

    while( $line =~ /MEDLINE\:(\d+)/ ) {
	$acc = $1;
	my $link = $self->get_medline_link( $acc, "$acc");

	$line =~ s/MEDLINE:$acc/[$link]/;
    }

	
    return $line;
}





=head2 get_database_link

 Title   : get_database_link
 Usage   : $ref = $self->get_database_link( $link ); 
 Function: This function returns a text string for the given link
 Returns : A text string combining possible several ursl
 Args    : 
    A link object
 Notes:

=cut
	
sub get_database_link {
    my ($self, $ref, $pdb) = @_;
    my ($db, $id, $add, $otherid, $url,$line,$mail,$refstring);

    $db = $ref->database();
    $id = $ref->primary_id();
  # print "HERE : $id <P>";
  #  ($add) = $ref->each_additional();
#print "BOO <P>";
    if( $db eq 'SCOP' ) {
	$id =~ tr/[A-Z]/[a-z]/;
    }
    
    if ( $db eq 'PFAMA' ) {
	my ($link, $database);
	eval {
	    $database = &RfamWWWConfig::get_database();
	    $otherid = $database->acc2id( $id );
	};
	if ($@) {	
	    carp "Could not find accession $id, $@";
	    $link = "Unknown id";
	} 
	else {
	    $link = "<a href=\'$RfamWWWConfig::getacc?$id\'>$otherid</a>";
	}
	# $refstring =  "$id\&nbsp;[$link]";
	$refstring =  "$link";
    }
    elsif ( $db eq 'PFAMB') {
      my $href;
      my $link;
      if ($id =~ /\s/) {
	my(@arr) = split(/\s+/, $id);
	foreach (@arr) {
	  $href = "$RfamWWWConfig::getpfamb?acc=$_";
	  $link = $link ." ". "<a href=\'$href\'>$_</a>";
	}
      } else {
	$href = "$RfamWWWConfig::getpfamb?acc=$id"; 
	$link = "<a href=\'$href\'>$id</a>";
      }    

	# $refstring =  "$id\&nbsp;[$link]";
	$refstring =  "$link";
    }
    elsif ( $db eq 'URL') {
      $refstring =  "<a href=\'$id\'>$id</a>";
    }
    elsif ( $db =~ /Expert/i ) {
      if( $db =~ /<(\S+)>/ ) {
	$mail = $1;
      } 
      else {
	$mail = $id;
      }
      $refstring =  "<a href=\'mailto:$mail\'>$mail</a>";

    } elsif ( ($db =~ /HOMSTRAD/i) || ($db =~ /COGS/i) ){

      my ($nline);
      my $mapper = &RfamWWWConfig::link_mapper();

      my @list = @{$mapper->{$db}};
      my $first = shift @list;
      my $url = $first->{'link'};

      if ($id =~  /\s/) {
	my(@arr) = split(/\s+/, $id);
	foreach my $id (@arr) {
	  next if  ($id !~ /[0-9]|[A-Z]|[a-z]/);
	  eval("\$nline = \"$url\"");
	  $refstring .= "  <a href=\"$nline\">$id</a>";
	    
	}

      } else {
	eval("\$nline = \"$url\"");
	$refstring = "<a href=\"$nline\">$id</a>";

      }


     
     

   } else {
      my ($nline);
      my $mapper = &RfamWWWConfig::link_mapper();

	if(not exists($mapper->{$db}) ) {
	    warn("An undefined database reference");
	    return "An undefined database reference [$db, $id, $add]";
	}
	
      my @list = @{$mapper->{$db}};
      my $first = shift @list;
      my $url = $first->{'link'};
      eval("\$nline = \"$url\"");

      $nline =~ s/PDBID/$id/g if( ( $db eq "SCOP")  && ($pdb !~ /[A-Z]/i)   ) ;


      if (not @list) {
;
	    $refstring = "<a href=\"$nline\">$id</a>";
	}
	else {
	    my $ref = $first->{'id'};
	    $refstring = "$id\&nbsp;"  if( ( $db ne "SCOP")  || (( $db eq "SCOP") && ($pdb !~ /[A-Z]/i) )   ) ;

	    defined($add) and $refstring .= "($add)\&nbsp;"  if( ( $db ne "SCOP"  && $db ne "PROSITE")  ||  (( $db eq "SCOP") && ($pdb !~ /[A-Z]/i) )   ) ;
	   
	    $refstring .= "$ref~$nline;"   if( ( $db eq "SCOP")  && ( $pdb =~ /[A-Z]/i)  ) ;
	    $refstring .= "[<a href=\'$nline\'>$ref</a>"   if( ( $db ne "SCOP")  || (( $db eq "SCOP") && ($pdb !~ /[A-Z]/i) )   )   ;
	
	    
	    foreach my $item ( @list ) {
		$url = $item->{'link'};
		eval("\$nline = \"$url\"");
		$ref = $item->{'id'};
	     	
		$nline =~ s/PDBID/$id/g if( ( $db eq "SCOP")  && ($pdb !~ /[A-Z]/i)   ) ;

		$refstring .= "|<a href=\'$nline\'>$ref</a>" if( ( $db ne "SCOP")  || (( $db eq "SCOP") && ($pdb !~ /[A-Z]/i) )   )  ;
		$refstring .= "$ref~$nline;" if( ( $db eq "SCOP")  && ( $pdb =~ /[A-Z]/i)  )  ;
	      }
	    $refstring .= "]"  if( ( $db ne "SCOP")  || (( $db eq "SCOP") && ($pdb !~ /[A-Z]/i) )   ) ;
	}
    }
		
    return "$refstring";
}




