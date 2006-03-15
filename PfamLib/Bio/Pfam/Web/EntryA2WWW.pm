
#
# BioPerl module for EntryA2WWW
#
# Author: Kevin Howe <pfam@sanger.ac.uk>
#
# Copyright Mhairi Marshall &  Kevin Howe
#
# You may distribute this module under the same terms as perl itself

# POD documentation - main docs before the code

=head1 NAME

EntryAWWW - Functionality to write an html version of an Entry

=head1 SYNOPSIS
    use EntryAWWW;

    $ent = $db->get_EntryA_by_acc($acc);
    $ent2www = EntryA2WWW->new( '-entry' => $ent );
    $ent2www->write_table(\*STDOUT);

=head1 DESCRIPTION

This class takes an Bio::Pfam::EntryA in construction, and provides
methods for generating html versions of the entry.

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries

=head1 APPENDIX

The rest of the documentation details each of the object methods. Internal methods are usually preceded with a _

=cut



package Bio::Pfam::Web::EntryA2WWW;

use vars qw(@ISA);
use Carp;
use Exporter;
use strict;
#use Data::Dumper;

use Bio::Pfam::Web::PfamWWWConfig;
use Bio::Pfam::Web::Entry2WWW;
#use Bio::Interpro::DB;

use Bio::Pfam::DB_Wrapper;
use CGI;

@ISA = qw(Bio::Pfam::Web::Entry2WWW Exporter);



=head2 write_table

 Title   : write_table
 Usage   : $en2www->write_table( $fh );
 Function: Writes an exciting family page to the given file handledqyy
 Example :
 Returns :
 Args    :

=cut

sub write_table {
    my $self = shift;
    my $fh = shift;
    my $pdb_selected = shift;
    my $entry = $self->entry();
   
    if (not defined $entry) {
	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Error: There is no entry to write");
    }
    # first, the key information




#### NEW BIOPERL 
    ## Get the pdb stuff
    my @tmp_pdb;
   
    my $entry_ann = $entry->ann();

    foreach my $link ( $entry->ann()->get_Annotations('dblink') ) {
      if  ($link->database =~ /PDB/)   {
	  push @tmp_pdb, $link;
	}
  
    }
    

#### OLD BIOPERL

#    if ($entry->ann->each_link()) {
#      foreach my $link ($entry->ann->each_link) {
#	if  ($link->database =~ /PDB/)   {
#	  push @tmp_pdb, $link;
#	}
#      }


#    }

	#---------------------------------------------------------------------------
	# add a page title
	# jt6 20060202 WTSI

	my $q = new CGI;

	print $q->div( { -id => "pageTitle" },
				   $q->h2( "Pfam entry ", $entry->id ) );

	#---------------------------------------------------------------------------

    #### PDB INTERFACE
    if(@tmp_pdb) {

      print "<table border=1 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=5 cellspacing=0 cols=2 width='100%'>\n";

      my $shuffle_pdb = &Bio::Pfam::Web::PfamWWWConfig::pdb_gif_shuffle(\@tmp_pdb);
      my @shuffle_pdb = @{$shuffle_pdb};

      my $link = $shuffle_pdb[0];

      my $prim_id =  $link->primary_id();
      my ($pid, $chain) = ($prim_id =~ /^(\S+)\s*(\S*)$/);
     # print "PID: $pid , PID: $pdb_selected <BR>";
      $pid = $pdb_selected if ($pdb_selected);
     # print "PID: $pid <BR>";
      my ($pdbst, $pdben);
      if ($link->optional_id()) {
	($pdbst, $pdben) = split(/;/, $link->optional_id());
	#  foreach my $add ($link->each_additional()) {  
	#	 print "ADD: $add <BR>";
	#          print "EACH:  ".$link->optional_id() . "<BR>";
	#     }
      }
      #print "ST: $pdbst, EN: $pdben <BR>";
      #my ($pdbst, $pdben) = $link->each_additional();
      if (not defined($pdbst) or not defined($pdben)) {
	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Tried to get start and end for pdb id ".
				  $link->primary_id." and failed\n");
      }

      print $fh "<TR VALIGN=TOP>";
      #print "HERE <P>";
      my $pdb_valid = 0;
      my (%valid_pdbs);
     # if ($pdb_selected) {
#	$pdb_valid = 0;
	foreach my $pdb_got  (@shuffle_pdb) {
	  my($tmp_id, $junk) = ($pdb_got->primary_id() =~ /^(\S+)\s*(\S*)$/);
	 # print "TMP: $tmp_id <BR>";
	  if ($tmp_id eq $pdb_selected) {
	    $pdb_valid = 1;
	    #last;
	  } elsif ($tmp_id eq $pid) {
	   # print "EQUALS!! <P>";
	  } else {
	    $valid_pdbs{$tmp_id} = $tmp_id;
	  }
	}
      $pdb_valid = 1 if (!$pdb_selected);
     # }
      #print "VALID: $pdb_valid <BR>";
      my $pdb_count = 0;
      foreach (sort keys %valid_pdbs) {
	#print "$_ <BR>";
	$pdb_count++;
      }
      #print "COUNT: $pdb_count <P>";
      if ($pdb_valid) {
	
	my ($header, $title) = &Bio::Pfam::Web::PfamWWWConfig::get_pdb_title($pid);
	
	my ($pdb_gif, $pfam_markup);
#	my $jpg = "$Bio::Pfam::Web::PfamWWWConfig::data_root/statics/$pid" . ".jpg";
	
	my $RDB = &Bio::Pfam::Web::PfamWWWConfig::get_database();
	my $jpg = $RDB->query( "select pdb_image.auto_pdb from pdb, pdb_image where pdb.auto_pdb = pdb_image.auto_pdb and pdb.pdb_id = \"$pid\"" );

	my (@jpeg_markup);
	if ( $jpg) {
	  (@jpeg_markup) = &Bio::Pfam::Web::PfamWWWConfig::jpeg_key_info($pid);
	  #$pdb_gif =
	  #"http://www.sanger.ac.uk/Software/Pfam/data/statics/$pid.jpg";
	  $pdb_gif = $Bio::Pfam::Web::PfamWWWConfig::cgibin."/getimage.pl?id=$pid";
	  $pfam_markup = "<P><B><font color=#FF0000>*</font></B>This image has been generated to show the Pfam domains. The <b>" .  $entry->id() . "</b> domain is coloured <font color=#1fc01f><B>green</B></font>"; 
	} else {
	  $pdb_gif = "http://www.ebi.ac.uk/thornton-srv/databases/pdbsum/$pid/traces.jpg";
	}
	if ($pid =~ /[A-Z]|[a-z]|[0-9]/) {
	  print $fh "<TD WIDTH=200 CLASS=normaltext  ALIGN=LEFT VALIGN=TOP><A HREF=http://www.ebi.ac.uk/thornton-srv/databases/cgi-bin/pdbsum/GetPage.pl?pdbcode=$pid><IMG SRC=$pdb_gif border=0 height=192 width=192></A><BR><B>Figure 1: $pid<BR><NOBR>$header</NOBR></B><BR>$title ";
	  
	  if (@jpeg_markup) {
	    print $fh "<P><table border=0 cellpadding=3 cellspacing=0><tr><td colspan=5 class=normalmediumtext>Key:</td></tr><tr><td class=normaltext><B>Domain</B></td><td class=normaltext><B>Chain</B></td><Td class=normaltext ><B>Start Residue</B></td><td  class=normaltext><B>End Residue</B></td></tr>";
	    
	    foreach (@jpeg_markup) {
	      my($chain, $pdb_start_res, $pdb_end_res, $pfamA_acc, $pfamA_id, $hex_colour) = split(/~/, $_);
	      print "<TR><TD  align=center><B><A href=$Bio::Pfam::Web::PfamWWWConfig::getacc?$pfamA_acc><font color=#$hex_colour>$pfamA_id</font></A></B></TD><TD class=normaltext align=center>$chain </TD><TD align=center  class=normaltext>$pdb_start_res </TD><TD  align=center class=normaltext>$pdb_end_res</TD></TR>";
	    }
	    
	    print $fh "</TABLE><BR><font size=-2>The Swissprot/PDB mapping was provided by <A HREF=http://www.ebi.ac.uk/msd>MSD</A></font>";
	  }
	  
	}
	
      } else {

	print "<TD class=normaltext valign=bottom><B>Error: $pid</B> is not a valid pdb identifier for this family.";

      }  # / end pdb valid
      if ($pdb_count > 0) {
	print "<form><input type=hidden name=acc value=" . $entry->acc . "> <select name=pdb>";
	foreach my $pdb_got  (sort keys %valid_pdbs) {
	  print "<option>$pdb_got</option>";
	 # }
	}
	 print $fh "</select> <input type=submit value=\"Display pdb\"></form>";
      }


      print $fh "</TD><TD VALIGN=top>";

    } #/ PDB ONLY

     my $table_border = 1;
    $table_border = 0 if (@tmp_pdb);

    if (!@tmp_pdb) {
      print "<table border=1 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=3 cols=1 width='100%'>\n";
      print "<TR><TD CLASS=normaltext VALIGN=top>";
    }

    print "<table border=0 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=3 cols=1 width='100%'>\n";

#	my $source = "RCS";
#	if($entry->from_rdb()) {
#		$source = "RDB " . $Bio::Pfam::Web::PfamWWWConfig::rdb_name;
#	}
    print "<tr><td ALIGN=CENTER bgcolor=#000070 class=whitetableheader><B><I>Accession number:</B></I> <B>", $entry->acc(), "</B></td></tr>\n";

    if (not $entry->is_dead()) {
      if (defined $entry->previous_ids()) {
	    print $fh "<tr><td>Previous identifiers: ", $entry->previous_ids(), "</td></tr>\n";
	}
	 print $fh "<tr><td><table width=100%><TR><TD ALIGN=left><B CLASS=normallargetext>", $entry->description(), "</B> </TD><TD ALIGN=right><a href=$Bio::Pfam::Web::PfamWWWConfig::addcomments?id=", $entry->id(), "&acc=", $entry->acc , "><img src=$Bio::Pfam::Web::PfamWWWConfig::image_link/annotation.gif border=0></A></TD></TR></TABLE></td></tr>\n"; ##OLD

#       print $fh "<tr><td><B CLASS=normallargetext>", $entry->ann->description(), "</B></td></tr>\n"; ##OLD

    }
    else {
      my $rdb = &Bio::Pfam::Web::PfamWWWConfig::get_database();
      my(@dead_family) = $rdb->query("select * from dead_families where pfamA_acc = '" . $entry->acc() . "' " );
      my($acc, $pfamA_id, $comment, $forward_to);
      foreach (@dead_family) {
	($acc, $pfamA_id, $comment, $forward_to) = @{$_};
      }
	print $fh "<tr><td><b>Family <font color=#FF0000>$pfamA_id</font> has been removed from Pfam.</b> ";
      my ($forward_text, $forward_id);
      if ($forward_to) {
	my(@forward) = $rdb->query("select pfamA_id from pfamA where pfamA_acc = '$forward_to'");
	foreach (@forward) {
	  ($forward_id) = @{$_};
	}
	$forward_text = "<A href=/cgi-bin/Pfam/getacc?$forward_to>$forward_id</A>";
      }
      if ($comment) {
	if ($comment =~ /$forward_id/) {
	  $comment =~ s/$forward_id/$forward_text/;
	  print $fh "<P><B>Comment:</B> $comment";
	} else {
	  print $fh "<P><B>Comment:</B> $comment ";
	  print $fh "($forward_text)" if ($forward_to);
	}

      }
      print $fh "</td></tr>\n";
	print $fh "</TABLE>";
	print $fh "</TABLE>";
	print $fh "<P><P><P><P>";
	return 1;

    }




  my $got_complexes = 1;
print $fh "<TR><TD VALIGN=MIDDLE CLASS=normaltext>\n" ;


  #  foreach $line ( $self->ann()->get_Annotations('comment') ) {
#       print $file $before, "CC", $after, $line->text, "\n";
#   }

    if ($entry->ann()->get_Annotations('comment') ) {


      foreach my $line ($entry->ann()->get_Annotations('comment') ) {
	$line = $self->sub_reference_line( $line->text , $entry->acc());
	print $fh "$line ";
      }
      ## TEMP TAKE OUT - prob with annotation
      if ($entry->each_forward_acc()) {
	my @acclist = map { " Pfam:$_"; } $entry->each_forward_acc;
	local $" = ",";
	my $string = " (Refer to: @acclist)";
	$string = $self->sub_reference_line( $string , $entry->acc());
	print $fh "$string";
	$self->print_interaction_info($fh, $entry);
	#$self->print_complexes_info($fh, $entry);
	$got_complexes = 0;
      }

      print $fh "</TD></TR>";






   } #/ end of PFAM annotation
	$self->print_interaction_info($fh, $entry);
	#$self->print_complexs_info($fh, $entry) if($got_complexes);;

    my $map = &Bio::Pfam::Web::PfamWWWConfig::link_mapper();

    my ($int_abstract, $interpro_string, $smart_string, $prosite_string);
    my ($got_interpro, $got_prosite, $got_smart) = (0,0,0);



#foreach my $link ( $entry->ann()->get_Annotations('dblink') ) {
#      if  ($link->database =~ /PDB/)   {
#	  push @tmp_pdb, $link;
#	}
  
#    }


    foreach my $link ($entry->ann()->get_Annotations('dblink')) {
	my $db = $link->database;
	if ($db eq "SMART" or $db eq "INTERPRO" or $db eq "PROSITE") {
	    my $home_link = "<a href=\'".$map->{$link->database}->[0]->{'home'}."\'>".$link->database()."</a>";
	    my $entry_link = $self->get_database_link($link);

	    if ($db eq "PROSITE") {
		if (not $got_interpro) {
		    my $link_template = 'For additional annotation, see the $home_link document <code>$entry_link</code>';
		    eval("\$prosite_string = \"$link_template\"");
		    $got_prosite = 1;
		}
	    }
	    elsif ($db eq "INTERPRO") {
		my $link_template = '<center>$home_link description (entry <code>$entry_link</code>)</center>';
		eval("\$interpro_string = \"$link_template\"");

		my $new_interpro = _get_interpro_entry($entry->acc());

	    $interpro_string .= "<br>".$self->sub_reference_line($new_interpro);
	    $got_interpro = 1;
	    }
	    elsif ($db eq "SMART") {
		my $link_template = 'This family was acquired from $home_link (see <code>$entry_link</code>)';
		eval("\$smart_string = \"$link_template\"");
		$got_smart = 1;
	    }
	}
    }


    my %clans = &Bio::Pfam::Web::PfamWWWConfig::clans_info($entry->acc());

    my %new_interpro = _get_interpro_entry($entry->acc());
    
    if ($clans{AUTO_CLAN}) {
      
      print $fh "<TR><TR><TR><TD ALIGN=CENTER CLASS=whitetableheader bgcolor=#000070><P>Clan: <FONT color=#ffff00>" . $clans{DESCRIPTION} . "</font></TD></TR><TR><TD class=normaltext>This family is a member of the <A href=$Bio::Pfam::Web::PfamWWWConfig::WWW_root/browse/clans.shtml#" . $clans{AUTO_CLAN}. ">" .$clans{DESCRIPTION} . "</A> clan.  This clan includes the following Pfam members: ";
      
      
      #This family forms the clan <A href=$Bio::Pfam::Web::PfamWWWConfig::WWW_root/browse/clans.shtml#" . $clans{AUTO_CLAN}. ">" .$clans{DESCRIPTION} . "</A> with the Pfam members:";
      foreach (@{$clans{MEMBERS}} ) {
	my($pfamA_acc, $pfamA_id) = split(/~/, $_);
	print $fh "<A href=$Bio::Pfam::Web::PfamWWWConfig::getacc" . "?$pfamA_acc>$pfamA_id;</A> ";
      }
      print $fh " </TD></TR>";
      
    }

	#---------------------------------------------------------------------------

	# get a database wrapper object, from which we can get Query
	# objects
	my $dbWrapper;
	eval {
	  $dbWrapper = Bio::Pfam::DB_Wrapper->new;
	};
	if( $@ ) {
	  # problem getting a DB_Wrapper
	}

	# get interpro information (really only the ID and abstract)

	my $interpro;
	if( defined $dbWrapper ) {
	  my $query = $dbWrapper->getQuery( "interpro_new" );
	  $interpro = $query->retrieve( { pfama_acc => $entry->acc() } );
	}

	# first element is the interpro ID, second is the interpro
	# abstract
	my $interpro_id       = $interpro->[0];
	my $interpro_abstract = $interpro->[1];

	if( $interpro_id and $interpro_abstract ) {

	  my $ipHome = $map->{INTERPRO}->[0]->{home};
	  my $ipLink = $map->{INTERPRO}->[0]->{link};
	  $ipLink =~ s/\$id/$interpro_id/;

	  print $q->Tr( $q->td( $q->hr ) );

	  # header row
	  print $fh
		$q->Tr(
			   $q->td( { -class   => "whitetableheader",
						 -bgcolor => "#000070" },
					   $q->p(
							 $q->center(
										$q->a( { -href => $ipHome },
											   $q->font( { -color => "#ffff00" },
														 "INTERPRO" )
											 ),
										" description (entry ",
										$q->a( { -href => $ipLink },
											   $q->font( { -color => "#ffff00" },
														 $interpro_id )
											 ),
										")"
									   )
							)
					 )
			  );

	  # abstract
	  print $fh $q->Tr(
				   $q->td( { -class => "normaltext" },
						  $interpro_abstract
						 )
				  );

	} # end of "if interpro_id..."


	#----------------------------------------

	# get gene ontology data
	
	my %go;
	if( defined $dbWrapper ) {
	  my $query = $dbWrapper->getQuery( "gene_ontology" );
	  my $rs = $query->getRawResults( { pfama_acc => $entry->acc() } );

	  # re-jig the result set into a more useful data structure
	  foreach my $row  ( @$rs ) {
		push @{ $go{$row->[0]} }, { term => $row->[1],
									id   => $row->[2] };
	  }
	}
	
	if( scalar keys %go ) {

	  my $goHome = $map->{GO}->[0]->{home};

	  print $fh $q->start_table( { -border      => 1,
								   -cellpadding => 5,
								   -cellspacing => 0,
								   -width       => "100%" } );

	  # header row
	  print $q->Tr(
				   $q->td( { -class   => "whitetableheader",
							 -align   => "center",
							 -bgcolor => "#000070",
							 -colspan => 2 },
						   $q->a( { -href => $goHome },
								  $q->font( { -color => "#ffffff" },
											"QuickGO" )
								)
						 )
				  );

	  # data
	  foreach my $category ( qw( component function process ) ) {

		# some categories don't have any associated GO terms
		next unless defined @{$go{$category}};

		print $fh $q->start_Tr;
		print $q->td( ucfirst $category );

		print $q->start_td( { -class => "normaltext" } );

		foreach my $goEntry ( @{$go{$category}} ) {
		  my $goLink = $map->{GO}->[0]->{link};
		  $goLink =~ s/\$id/$goEntry->{id}/;
		  print $fh	$goEntry->{term},
                    " (", $q->a( { -href => $goLink },
								 $goEntry->{id} ), ")",
					$q->br;
		}

		print $q->end_td, $q->end_Tr;

	  }

	  print $fh $q->end_table;

	} # end of "if scalar keys %go..."

# this is the section of old code that's replaced by the bit
# above. This old bit uses the (now gone) interpro_and_go table

# my $got_interpro = 1 if
# (defined($new_interpro{'INTERPRO'}));

#     if ($got_interpro) {
#       my $interpro_string;
#       my $home_link = "<a href=\'".$map->{'INTERPRO'}->[0]->{'home'}."\'><FONT color=#ffff00>INTERPRO</FONT></a>";
#       my $entry_link = "<a href=\'".$map->{'INTERPRO'}->[0]->{'link'}."\'><FONT color=#ffff00>" .$new_interpro{'INTERPRO'} . "</FONT></a>";
#       $entry_link =~ s/\$id/$new_interpro{'INTERPRO'}/;
#       my $link_template = '<center>$home_link description (entry <code>$entry_link</code>)</center>';
#       eval("\$interpro_string = \"$link_template\"");


#       print "<TR><TD><HR></TD></TR>";

#       print $fh "<TR><TD CLASS=whitetableheader bgcolor=#000070><P>$interpro_string</TD></TR>";
#        print $fh "<TR><TD CLASS=normaltext>" .$new_interpro{'ABSTRACT'} . " ";

#       if( (defined($new_interpro{'FUNCTION'})) ||  (defined($new_interpro{'PROCESS'})) || (defined($new_interpro{'COMPONENT'})) ) {
# 	my @type = ('FUNCTION', 'PROCESS', 'COMPONENT');
# 	print $fh "<TABLE border=1 cellpadding=5 cellspacing=0  width=100%>";
# 	print $fh "<tr><tr><tr><tr><tr><Tr><TR><TD CLASS=whitetableheader align=center bgcolor=#000070 colspan=2> <A HREF=\"" . $map->{'GO'}->[0]->{'home'}. "\"><font color=#ffffff size=-1>QuickGO</font></A></TD></TR>";
# 	foreach (@type) {
# 	  if (defined($new_interpro{$_})) {

# 	    my($go_text, $go_link) = split(/;/,$new_interpro{$_});
# 	    print $fh "<TR><TD class=normaltext><B>$_</B> :</TD>";

# 	    print $fh "<TD  class=normaltext>";
# 	    print $fh "$go_text ";
# 	    $go_link =~ s/\s+//g;
# 	    my $link = "<a href=\'".$map->{'GO'}->[0]->{'link'}."\'>" .$go_link . "</a>";
# 	    $link =~ s/\$id/$go_link/;
# 	    print $fh " ($link) </TD></TR>";
# 	  }
# 	}
# 	print $fh "</TABLE>";
#       }
#      print " </TD></TR>";

#     }

	#---------------------------------------------------------------------------

    print "</TABLE>" if (@tmp_pdb);


    if  ($got_smart and ($entry->author eq "SMART"
			    or $entry->source eq "Alignment kindly provided by SMART") ) {
	print "<tr>";

	if (@tmp_pdb) {
	  print "<td colspan=2>\n";
	} else {

	  print "<td CLASS=normaltext >\n";
	}


	print $smart_string;
	print $fh "</td></tr>\n";
    }

    else {
	if  ($got_prosite) {
	    print $fh "<tr>";

	    if (@tmp_pdb) {
	      print $fh " <td colspan=2 CLASS=normaltext>\n";
	    } else {
	      print $fh " <td CLASS=normaltext>\n";
	    }

	    print $fh $prosite_string;
	    print $fh "</td></tr>\n";
	}
    }





   print "</table>\n";
   print"</TD></TR></TABLE>" if (!@tmp_pdb);
    # next the main user interaction point


    if (not $entry->is_dead() ) {
#	print "<p><table cellpadding=3 class=normaltext><tr><td>To contribute to the annotation for this family (and win a T-shirt), ";
#	print "click <a href=$Bio::Pfam::Web::PfamWWWConfig::addcomments?id=", $entry->id(), "&acc=", $entry->acc , ">here</a>\n</td></tr></table><p>\n";
	print "<P>";

###<img src=$Bio::Pfam::Web::PfamWWWConfig::WWW_root/images/alignment.gif>

#### New table design !!
	print $fh "
	<table CLASS=normaltext align=center border=1 cellpadding = 5 cellspacing=0 cols=2 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour >
<tr CLASS=whitetableheader>

<td valign=top CLASS=whitetableheader CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Alignment </TD>
<td valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Domain organisation</TD></TR>
";

$self->write_user_interaction_table( $fh );

print $fh "
<tr CLASS=whitetableheader>
<td valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Species Distribution</TD><TD valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=00007>Phylogenetic tree</TD>
</TR>";





	$self->write_species_table( $fh );

	$self->write_phylogenetic_table( $fh );


	print $fh "</tr></table>";


	$self->write_links_table( $fh );


	print $fh "<P><TABLE border=0 WIDTH=100%
           <TR>";
	# now, the references and links
	$self->write_references_table( $fh);


	# now the Pfam specific fields, but only for entries that are alive

	$self->write_Pfam_table( $fh );

	print $fh "</TR></TABLE>";
	# $self->write_bait_form( $fh );

	print $fh <<"EOF";
<p>
For help on making stable links to this page <a href="$Bio::Pfam::Web::PfamWWWConfig::stable_link">click here</a>
EOF
    }
}





=head2 write_Pfam_table

 Title   : write_Pfam_table
 Usage   : $self->write_Pfam_table(\*STDOUT);
 Function:
 Example :
 Returns :
 Args    :

=cut

sub write_Pfam_table {
    my ($self, $fh) = @_;

    my $entry = $self->entry();

 #   print "<center><h3>Pfam specific information</h3></center>\n";
	print $fh "<TD VALIGN=TOP>";
    print $fh "<table WIDTH=100% border=1 CLASS=normaltext bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=5 cellspacing=0 cols=2 >\n";
	print $fh "<TR><TD colspan=2 valign=top align=center CLASS=whitetableheader bgcolor=000070>Pfam specific information</TD></TR>";
    print $fh "<tr><td class='left'>Author of entry</td><td>", $entry->author(), "</td></tr>\n";
    print $fh "<tr><td>Type definition</td><td>", $entry->entry_type(), "</td></tr>\n";
#    print $fh "<tr><td>Alignment method of seed</td><td>", $entry->alignmethod(), "</td></tr>\n";
    print $fh "<tr><td>Source of seed members</td><td>", $entry->source(), "</td></tr>\n";
    print $fh "<tr><td>Average Length</td><td>", $entry->average_length(), "</td></tr>\n";
    print $fh "<tr><td>Average %id</td><td>", $entry->percentage_id(), "</td></tr>\n";
    print $fh "<tr><td>Average Coverage</td><td>", $entry->average_coverage(), "%</td></tr>\n";
    print $fh "</td></tr>";
    print $fh "</table>";
    print $fh "<P>";

    print $fh "<table WIDTH=100% border=1 CLASS=normaltext bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=5 cellspacing=0 cols=2 >\n";


    print $fh "<TR><TD colspan=3 valign=top align=center CLASS=whitetableheader bgcolor=000070>HMMER build information</TD></TR>";
    print $fh "<TR ><TD>&nbsp;</TD><td NOWRAP valign=bottom CLASS=normalmediumtext>Pfam_ls <A href=download_hmm.pl?id=" . $entry->id(). "&mode=ls>&nbsp;[Download HMM]</A></TD><TD  valign=bottom  CLASS=normalmediumtext>Pfam_fs <A href=download_hmm.pl?id=" . $entry->id(). "&mode=fs>&nbsp;[Download HMM] </A></TD></TR>";
    print $fh "<tr><td >Gathering cutoff</td><td NOWRAP> ", $self->chop_cutoff($entry->ls_sequence_gathering_cutoff()), " " , $self->chop_cutoff($entry->ls_domain_gathering_cutoff() ), ";</TD><TD NOWRAP> ", $self->chop_cutoff($entry->fs_sequence_gathering_cutoff() ), " ", $self->chop_cutoff($entry->fs_domain_gathering_cutoff() ), "</td></tr>\n";


    print $fh "<tr><td>Trusted cutoff</td><td NOWRAP>", $self->chop_cutoff($entry->ls_sequence_trusted_cutoff()),  " " , $self->chop_cutoff($entry->ls_domain_trusted_cutoff() ), ";</TD><TD NOWRAP> ",$self->chop_cutoff( $entry->fs_sequence_trusted_cutoff() ), " ", $self->chop_cutoff( $entry->fs_domain_trusted_cutoff() ),"</td></tr>\n";
    print $fh "<tr><td>Noise cutoff</td><td NOWRAP>", $self->chop_cutoff($entry->ls_sequence_noise_cutoff() ),  " " , $self->chop_cutoff($entry->ls_domain_noise_cutoff() ), "; </TD><TD NOWRAP>", $self->chop_cutoff($entry->fs_sequence_noise_cutoff()), " ", $self->chop_cutoff($entry->fs_domain_noise_cutoff()),"</td></tr>\n";
    print $fh "<tr><td valign=top NOWRAP>Build method of HMM</td><td NOWRAP valign=top >";

### TAKEN OUT TEMP
    my $count = 0;
   foreach my $build ($entry->each_build_line()) {
     print $fh "</TD><TD NOWRAP valign=top>" if ($count =~ /2/);
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

  chop($cutoff);
  chop($cutoff);
  chop($cutoff);

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

    my ($id, $acc, $fcount,$scount, $arch_scount, $arch_fcount);

    my $entry = $self->entry();
    if (not defined $entry) {
	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Error: There is no entry to write");
    }

    $id = $entry->id();
    $acc  = $entry->acc();

    $scount = $entry->num_seqs_in_seed();
    $fcount = $entry->num_seqs_in_full();
    $arch_scount = $entry->arch_in_seed();
    $arch_fcount = $entry->arch_in_full();

my $context_count;
$context_count = &Bio::Pfam::Web::PfamWWWConfig::context_regions($acc, 1);

    print $fh qq (


<TR ><TD VALIGN=TOP ALIGN=CENTER>
<table border=0 cellpadding=0 cellspacing=0 cols=1>


<tr>
<td CLASS=normaltext NOWRAP>
<form METHOD="GET" ACTION="$Bio::Pfam::Web::PfamWWWConfig::getalign">
<input name="name" type="hidden" value="$id">
<input name="acc" type="hidden" value="$acc">
<center>
<input name=type type=radio checked value=seed> Seed ($scount)&nbsp;
<input name=type type=radio value=full> Full ($fcount));

#if ($context_count) {
#	print qq(
#<input name=type type=radio value=context><A HREF=\"#\" onClick='w=window.open(\"help.pl\", \"helpwindow\", \"width=450, height=410, scrollbars=yes,resizable=yes\");w.focus();'> Context</A> ($context_count)
#	);
#}

my $arch_text = "architecture";
$arch_text .= "s" if ($arch_fcount > 1);
print $fh qq (
<br><br>

Format <select name=format>
<option value=link> Coloured alignment
<option value=mul> Plain Pfam format
<option value=stock> Plain Stockholm format
<option value=fal> Fasta format (with gaps)
<option value=msf> MSF format
<option value=belvu> Belvu (unix only)
<option value=belold> Belvu (old version unix only)
<option value=jalview> Jalview (Java)
</select>
<P>
<input type=submit value="Get alignment">
<input type=button value=\"View HMM logo\" \" onClick='w=window.open(\"http://www.sanger.ac.uk/cgi-bin/software/analysis/logomat-m.cgi?pfamid=$acc\", \"helpwindow\",\"width=750, height=710, menubar,resizable,scrollbars,status,toolbar, location\");w.focus();' > </center></form>
</td>
</tr>

<tr valign=top>
<td  CLASS=normaltext>
Further alignment options <a href="$Bio::Pfam::Web::PfamWWWConfig::extraalign?name=$id&acc=$acc">here</a>
<BR>
Help relating to Pfam alignments <a href="$Bio::Pfam::Web::PfamWWWConfig::align_help">here</a>
</td>
</tr>
</table>
</td>

<td valign=top ALIGN=CENTER>
<table  border=0 cellpadding=0 cellspacing=0 cols=1>


<tr>
<td  CLASS=normaltext VALIGN=TOP NOWRAP>
<form METHOD="GET" ACTION="$Bio::Pfam::Web::PfamWWWConfig::getall">
<input name="name" type="hidden" value="$id">
<input name="acc" type="hidden" value="$acc">
<input name="verbose" type="hidden" value="true">
<center>
<table border=0 cellpadding=0 cellspacing=0 class=normaltext>
<TR><TD class=normaltext valign=top align=lef>
<input type=hidden name=type value=full checked>

);
    if ($arch_fcount > 0) {
print qq(      <input name=domain_view type=radio checked value=arch> View $arch_fcount representative $arch_text );
    }

print qq(
 </TD><TR>
<TD class=normaltext valign=top align=left><input name=domain_view type=radio  value=all> View architectures for $fcount proteins<BR></TD>


);

#if ($context_count) {
#print qq(
#<TD>&nbsp;</TD><TD>&nbsp;</TD><TD class=normaltext valign=top align=left><input name=type type=radio value=context> <A HREF=\"#\" onClick='w=window.open(\"help.pl?type=Context\", \"helpwindow\", \"width=450, height=410, scrollbars=yes,resizable=yes\");w.focus();'>Context</A><BR>(<B>$context_count</B> domains)</TD>

#);

#}

#print "</TR><TR><TD colspan=1>$scount domains<BR>$arch_scount architecture</TD><TD colspan=1>$fcount domains<BR> $arch_fcount architecture</TD>";


#<P>($scount domains)<BR>($arch_scount architecture)
# ($fcount)<BR>architecture($arch_fcount)
if ($context_count) {
#print qq(
#<TD colspan=1 valign=top align=left>$context_count domains
#);
}

print $fh qq (
</TD></TR></TABLE>
</center>
<P><P></table>Zoom <input name=zoom_factor type=text value="0.5" size=3 maxlength=3> pixels/aa. <P><input type=submit name="list" value="View Graphic">

</form>);
#<br>
#<table border=1 cellpadding=0 CLASS=normaltext>
#<th>As a Graphic</th>
#<th>As a Tree</th>
#<tr><tr>
#<tr valign=bottom>
#<td>
#View by:<BR>
#<input name=domain_view type=radio value=arch checked> Distinct architecture <input name=domain_view type=radio value=all > All domains </TD><TD>&nbsp</TD></TR>
#<TR><TR>
#<tr>
#<td CLASS=NORMALTEXT>
#Zoom <input name=zoom_factor type=text value="0.5" size=3 maxlength=3> pixels/aa.<br>
#</td>
#<td  CLASS=NORMALTEXT>
#<input name=bootstrap type=checkbox> Bootstrap tree
#</td>
#</tr>


#<tr valign=bottom>
#<td>
#<input type=submit name="list" value="View Graphic">
#</td>
#<td>
#<input type=submit name="tree" value="NIFAS Applet">
#</td>
#</tr>

#<tr>
#<td colspan=2  CLASS=NORMALTEXT>
#<br>
#To find out about the NIFAS tree-viewer, click <a href="$Bio::Pfam::Web::PfamWWWConfig::align_help">here</a>
#</td>
#</tr>
#</table>
#</form>

#</td>
#</tr>
#</table>
#</td>
#<P>

#);
}



sub write_phylogenetic_table {
  my ($self, $fh) = @_;

  my $entry = $self->entry();
  print $fh "<td ALIGN=CENTER VALIGN=TOP CLASS=normaltext NOWRAP>\n";

  my $id = $entry->id();
  my $acc =  $entry->acc();
  my $link = $acc . ".tree";

  my $scount = $entry->num_seqs_in_seed();
  my $fcount = $entry->num_seqs_in_full();

    print $fh qq (



<form  METHOD="GET" ACTION="$Bio::Pfam::Web::PfamWWWConfig::cgibin/download_hmm.pl" >
<input type=hidden name=acc value=$acc>
<input type=hidden name=tree value=tree>
<center>
<input type=radio name=\"type\" checked  value=\"seed\"> Seed ($scount)&nbsp;
<input type=radio name=\"type\" value=\"full\" > Full ($fcount)



<br><br>

</center>
);

#  print $fh "View as an applet phylogenetic tree for: $id";
  print $fh "<P><input type=submit Value=\"Download tree\"> <input type=button value=\"ATV Applet\" onClick=\"open_cur_ATV('$acc', this.form)\">";
  print $fh "</form><P>The trees were generated using <A HREF=http://www.sanger.ac.uk/Software/analysis/quicktree/>Quicktree</A><BR> To find out more about ATV phylogenetic tree-viewer <A HREF=http://www.genetics.wustl.edu/eddy/atv>click here</A> ";


 # print $fh "</form></td>\n";
}

sub  _get_interpro_entry {

	my $acc = shift;
	my $new_interpro;

	my %interpro_info;

#	print "BLEE: $acc <P>";
	%interpro_info = &Bio::Pfam::Web::PfamWWWConfig::interpro_info($acc);



#	#my $file = $Bio::Pfam::Web::PfamWWWConfig::data_root . "/interpro/$acc" . ".interpro";
#	my $file =  "/nfs/intweb/doctree/htdocs/Software/Pfam/WebSite/temp/interpro/$acc" . ".interpro";
#	open(_INTERPRO, $file);

#	my $do = 0;
#	while(<_INTERPRO>) {
#		if ($do) {
#			$new_interpro =  $new_interpro . $_;

#		}

#		if($_ =~ /^ABSTRACT\s+(.*)/) {
#		$new_interpro = $1;
#		$do = 1;
#		}



#	}


#	close(_INTERPRO);




return %interpro_info;
}


1;  # says use was ok
