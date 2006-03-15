##########
# Author: mm1
# Last Modified: by mm1


=head1 NAME

Entry2WWW - DESCRIPTION

This package contains helper methods for producing a web-representation
of a Pfam Entry (both Pfam-A and Pfam-B). It is an abstract class. 
Concrete implementations EntryA2WWW and EntryB2WWW should be used.


=cut


package Bio::Pfam::Web::Entry2WWW;
use vars qw(@ISA @EXPORT_OK);
use strict;
use Carp;
use CGI; # JT 20051024 WTSI
my $cgi = new CGI;

use Bio::Pfam::Web::PfamWWWConfig;
use Text::Wrap;

@ISA = qw(Exporter);

# The following entries have been removed from the db mapper:
# SRS-Japan : 'SRS-Japan http://srs.dna.affrc.go.jp/srs//srsc?[PROSITEDOC-id:$id]', 
# SCOP-Japan : 'SCOP-Japan http://www.beri.co.jp/scop/search.cgi?pdb=$id&tlev=$add', 

########### constants...

my $entrycolumns = 100;
my $medlinelink = "http://www.ncbi.nlm.nih.gov:80/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Abstract&list_uids=";


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



=head2 write_references_table

 Title   : write_references_html
 Usage   : $ann2www->write_references_table( $file );
 Function: Prints out literature references in swanky new table format
 Args    : Destination file handle


=cut

sub write_references_table {
    my ($self, $fh) = @_;

    my $entry = $self->entry();
 
    if ($entry->ann()->get_Annotations('reference') ) {
     
      print $fh "<TD VALIGN=TOP >";
	print $fh "<table border=1 CLASS=normaltext bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=5 cellspacing=0 cols=1 WIDTH=100% HEIGHT=100%>\n";
	print $fh "<TR ><TD valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>Literature References</TD></TR>";
	my $count = 1;
	foreach my $ref ( $entry->ann()->get_Annotations('reference')  ) {
	    print $fh "<tr><td CLASS=boldtextsmall HEIGHT=100%>\n";
	    print $fh $self->get_medline_link( $ref->medline(), "$count. "), " ";
	    if (defined ($ref->comment() ) ) {
		#foreach my $refcomm ( $ref->comment->each_flat() ) {
		   my  $refcomm = $self->sub_reference_line( $ref->comment() , $entry->acc());
		    print $fh "$refcomm ";
		#}
	    }
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

    # there may be many Pfam-A and B links, but more complicating is the
    # fact that there may be many more than one kind of each kinf of link.
    # Therefore, Pfam-A and B links that do not come directly after a DC
    # line are grouped with the last Pfam link that DID have a DC line.

    my (@pfam, @pdb, @other, @rosetta);

    # PDB links are also grouped

    my $entry = $self->entry();
    
    my $done_scop = 1;
    my $scop = 0;

    if ($entry->id() !~ /Pfam-B/) {
    
      print $fh "<P><table border=1 CLASS=normaltext cellpadding = 5 cellspacing=0 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=5 cols=2 width='100%'>\n";

      print $fh "<tr><td colspan=2 valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>Database References </TD></TR>\n";
    }


    ### TAKEN OUT - probs in ANN object
    if ($entry->ann()->get_Annotations('dblink')) {
      foreach my $link ($entry->ann()->get_Annotations('dblink')) {
	if ($link->database =~ /PFAM/) {
	  if (defined $link->comment()) {
	    push @pfam, [$link];
	  }
	  else {
	    push @{$pfam[$#pfam]}, $link;
	  }
	}
	elsif ($link->database =~ /PDB/) {
	  push @pdb, $link;
	} elsif ($link->database =~ /ROSETTA/) {
	  push @rosetta, $link;

	} else  {
	  push @other, $link;
	}
	if ($link->database =~ /SCOP/) {
	  $scop = 1;
	}
      }
      
      # now to print ou the table

	if ($entry->id() =~ /Pfam-B/) {
	  print $fh "<P><table border=1 CLASS=normaltext cellpadding = 5 cellspacing=0 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=5 cols=2 width='100%'>\n";

	  print $fh "<tr><td colspan=2 valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>Database References </TD></TR>\n";
	}

	if (@pdb) {
	    print $fh "<tr >\n<td NOWRAP valign=top class='left'><SPAN CLASS=normallargetext>", $pdb[0]->database(), "</span><br>\n";
	    print $fh "<span class=normaltext>You can find out how to set up Rasmol </SPAN>";
	my $link = "\"#\" onClick='w=window.open(\"help.pl?type=Rasmol\", \"helpwindow\", \" width=450, height=410, scrollbars=yes,resizable=yes\");w.focus();'";
	    print $fh "<a href=$link>here</a></td>\n<td NOWRAP valign=top>"; 
	    # now put all of the pdb identifiers into a list box

		print $fh "<TABLE BORDER=0><TR><TD valign=top>";
	    #print $fh "<form method=post action=\"$Bio::Pfam::Web::PfamWWWConfig::getstructure\">\n";
	    print $fh "<form method=post action=/cgi-bin/Pfam/rasmol_2_structure.pl\>\n";
	
	    ### ONLY 1 PDB STRUCTURE SO HAVE TO HACK IT
	    print $fh "<select name=\"pdb\" \">\n";
	    if (length(@pdb) == 1) {
	      my $prim_id =  $pdb[0]->primary_id();
	      my ($pid, $chain) = ($prim_id =~ /^(\S+)\s*(\S*)$/);
	      my ($pdbst, $pdben) ;
	      if ($pdb[0]->optional_id()) {
		($pdbst, $pdben) = split(/;/, $pdb[0]->optional_id());
	      }
	      if (not defined($pdbst) or not defined($pdben)) {
		&Bio::Pfam::Web::PfamWWWConfig::exit_html("Tried to get start and end for pdb id ".					  $pdb[0]->primary_id." and failed\n");
	      }
	      
	      print $fh "<option value=\"$pid:$chain:$pdbst:$pdben\"> $prim_id; $pdbst; $pdben;\n";
	      
	     }
	    
	   
	    ### MULTIPLE STUCTURES
	    foreach my $link ( @pdb ) {
		my $prim_id =  $link->primary_id();
		my ($pid, $chain) = ($prim_id =~ /^(\S+)\s*(\S*)$/);
		my ($pdbst, $pdben) ;
		if ($link->optional_id()) {
		  ($pdbst, $pdben) = split(/;/, $link->optional_id());
		}
		if (not defined($pdbst) or not defined($pdben)) {
		    &Bio::Pfam::Web::PfamWWWConfig::exit_html("Tried to get start and end for pdb id ".
					      $link->primary_id." and failed\n");
		  }
		print $fh "<option value=\"$pid:$chain:$pdbst:$pdben\"> $prim_id; $pdbst; $pdben;\n";
	      }
	    print $fh "</select>\n </TD><TD valign=top NOWRAP>";
	    
	    print $fh "<TABLE BORDER=0><TR><TD ALIGN=CENTER>";
	    print $fh "<input type=\"button\" name=\"PDB 2 Pfam\" value=\"PDB 2 Pfam\" onClick=\"newWindow6(this.form, '/cgi-bin/Pfam/structural_view.pl?pdb=PDBID&acc=".$entry->acc."&name=".$entry->id."')\"  style=\"color: #FFFFFF; background-color: #3973AD; font-family: Arial, Helvetica; font-weight: 6pt; border-style: Bold;\">  ";
	    
	    print $fh "<input type=\"button\" name=\"Scop|Cath|Pfam\" value=\"Scop|Cath|Pfam\" onClick=\"newWindow6(this.form, '/cgi-bin/Pfam/structural_domains2pfam.pl?pdb=PDBID')\"  style=\"color: #FFFFFF; background-color: #3973AD; font-family: Arial, Helvetica; font-weight: 6pt; border-style: Bold;\"> ";

	    print $fh "</TD></TR><TR><TD ALIGN=CENTER>";
	    print $fh "<input type=\"button\" name=\"Rasmol\" value=\"Rasmol\" onClick=\"newWindow3(this.form, 'Rasmol')\"  style=\"color: #FFFFFF; background-color: #3973AD; font-family: Arial, Helvetica; font-weight: 6pt; border-style: Bold;\"> ";

	    print $fh "<input type=\"button\" name=\"Chime\" value=\"Chime\" onClick=\"newWindow3(this.form, 'Chime')\"  style=\"color: #FFFFFF; background-color: #3973AD; font-family: Arial, Helvetica; font-weight: 6pt; border-style: Bold;\"> ";
	

	    print $fh "<input type=\"button\" name=\"Jmol\" value=\"Jmol\" onClick=\"newWindow3(this.form, 'Jmol')\"  style=\"color: #FFFFFF; background-color: #3973AD; font-family: Arial, Helvetica; font-weight: 6pt; border-style: Bold;\"><BR>";
	    print $fh "</TD></TR><TR><TD ALIGN=CENTER>";

	    #### HAS TO BE HARD CODED FOR Pfam-B domains (sigh)
	    if ( (@pdb) && (!$scop) ) {
	 
	      my $scop_link = "PDBSUM~http://www.ebi.ac.uk/thornton-srv/databases/cgi-bin/pdbsum/GetPage.pl?pdbcode=PDBID;SCOP-UK~http://scop.mrc-lmb.cam.ac.uk/scop/search.cgi?pdb=PDBID&tlev=fa;MSD~http://www.ebi.ac.uk/msd-srv/atlas?id=PDBID";
	      
	      my @pdb_links = split(/;/, $scop_link);
	      foreach (@pdb_links) {
		if ($_ =~ /[A-Z]/i) {
		  my ($name, $link) = split(/~/, $_);

		  print $fh "<input type=hidden name=pdb_id value=$name>";	
		  print $fh "<input type=hidden name=pdb_url value=$link>";	
		   print $fh "<input type=\"button\" name=\"$name\" value=\"$name\" onClick=\"newWindow5(this.form, '$link')\"  style=\"color: #FFFFFF; background-color: #3973AD; font-family: Arial, Helvetica; font-weight: 6pt; border-style: Bold;\"> ";
		 
		  
		}		#/ end IF
		
	      }			#/ end FOR
	      
	    }
	    
	    #	    ### end of hard coded region



	    my $count = 0;
	    foreach my $link (@other) {
	      if ($link->database() eq "SCOP") {
		if ($done_scop) {
		my $scop_link = $self->get_database_link($link, "PDB");
		my @pdb_links = split(/;/, $scop_link);
		foreach (@pdb_links) {
		  if ($_ =~ /[A-Z]/i) {
		    my ($name, $link) = split(/~/, $_);
		    
		    print $fh "<input type=hidden name=pdb_id value=$name>";   
		    print $fh "<input type=hidden name=pdb_url value=$link>";  
		    print $fh "<input type=\"button\" name=\"$name\" value=\"$name\" onClick=\"newWindow5(this.form, '$link')\"  style=\"color: #FFFFFF; background-color: #3973AD; font-family: Arial, Helvetica; font-weight: 6pt; border-style: Bold;\"> ";
       		           	
       		           	
		  } #/ end IF
		  
		} #/ end FOR
		$done_scop = 0; 
		}
		splice (@other, $count, 1);
	      } else {
		$count++;
	      } #/ end IF
	      
	  
	    } #/ end FOR
	    print $fh "</TD></TR></TABLE>";
	    print $fh " </form></TD></TR></TABLE>\n</td>\n</tr>\n";
	}
     # END statement

	  # ============================================================
	  # JT 20051024 WTSI

	  if( $entry->acc() =~ /^PF/ ) {
		writePfamALinks( $entry );
	  } elsif( $entry->acc() =~ /^PB/ ) {
		writePfamBLinks( $entry );
	  }

	  # ============================================================


      if ( (@rosetta) && (!@pdb) ){

	my $link2 = $self->get_medline_link("11551178", "Functional inferences from blind ab initio protein structure predictions");

	
	print $fh "<tr>\n<td VALIGN=TOP CLASS=boldtextsmall ><SPAN class=normallargetext><A HREF=\"#\" onClick='w=window.open(\"rosetta.pl\", \"helpwindow\", \"width=450, height=450, scrollbars=yes,resizable=yes\");w.focus();'> ROSETTA</A></SPAN><P><A href=http://www.ncbi.nlm.nih.gov:80/entrez/query.fcgi?cmd=Retrieve&db=PubMed&list_uids=12215415&dopt=Abstract>De Novo Prediction of Three Dimensional Structures for Major Protein Families.</A><BR>Richard Bonneau, Dylan Chivian, Charlie EM Strauss, Carol Rohl, David Baker (2002).  <BR>JMB; v.322, no.1, p.65-78 <P> $link2.<BR> Bonneau, R., Tsai, J., Ruczinski, I. & Baker, D. (2001). <BR>J Struct Biol 134(2-3),186-90. <BR> </td><td><form method=post action=/cgi-bin/Pfam/rasmol_2_structure.pl\>";

	foreach my $link (@rosetta) {
	  my @rest = split(/;/, $link);
	  my $prim_id =  $link->primary_id();
	 # my ($pid, $chain) = ($prim_id =~ /^(\S+)\s*(\S*)$/);
	 # my ($pdbst, $pdben) = $link->each_additional();

	 
	  #if ($link->optional_id()) {
	   # print "LINK: ". $link->optional_id(). " <BR>";
		  #($pdbst, $pdben) = split(/;/, $link->optional_id());
		
	#	}
	  my (@all) = split(/;/,$link->optional_id() );
	  #  my($group_confidence, $scop_confidence, $model_num,$pdb_id,$chain,$mammoth_Z_score,$length_ratio) = split(/;/, $link->each_additional());
	  my $pdb;
	  if ( $all[3] =~ /[0-9]/) {
	    $pdb = " similiar to pdb: " .$all[3];
	    if ($all[4] !~ /0/) {
	      $pdb = $pdb . " chain: ".  $all[4] if  (defined($all[4]));        
	    }
	   
	    $pdb = $pdb . " <input type=radio name=pdb value=" .$all[3] . ":" . $all[4]. "::  >";
	  }
	  my $rosetta_link =  $self->get_database_link($link, "ROSETTA");
	  print "Model ".  $all[2]. " <input type=radio name=pdb value=rosetta~$prim_id off> $pdb  <BR>";
	 # print "EEP : $rosetta_link  PRIM ID: $prim_id  :: ALL: @all <BR>";
	  
	}
	



      }


	foreach my $link ( @other ) {
	  
	  if ($link->database() eq "SCOP") {
		if ($done_scop eq "0") {
			next;
		} else  {

	    	print $fh "<tr>\n<td class='left'><SPAN CLASS=normallargetext>PDB</SPAN>";
		$done_scop = 0;

		}
	  } else {
	    print $fh "<tr>\n<td class='left'><SPAN CLASS=normallargetext>  ", $link->database(), "</SPAN>";
	  }

	    
	    
	    if (defined( $link->comment() )) {
	      print $fh "<br>\n";
	      #print $fh "COMM: " . $link->comment(). "<BR>";

	      #foreach my $linkcomm ($link->comment->each_flat() ) {
		#$linkcomm = $self->sub_reference_line( $linkcomm, $entry->acc() );
	      print $fh "<span class=normaltext>" . $link->comment() . "</span>\n";
	      #}
	    }
	    print $fh "</td>\n<td><code>", $self->get_database_link($link), "</code></td>\n</tr>\n";

	}

	  # ============================================================

	  # this bit of code has been replaced by the bit above, which
	  # generates separates lists of links for the separate sources
	  # JT 20051025 WTSI

#	  foreach my $list ( @pfam ) {

# 	    my $first_link = $$list[0];
# 	    my @all_links = @{$list};
# 	    print $fh "<tr>\n<td VALIGN=TOP><SPAN class=normallargetext>  ", $first_link->database(), "</SPAN>";
# 	    if (defined( $first_link->comment() )) {
# 		  print $fh "<br>\n";

# 		  #foreach my $linkcomm ($first_link->comment->each_flat() ) {
# 		  my $linkcomm = $self->sub_reference_line($first_link->comment() , $entry->acc());
# 		  if ($first_link->database() =~ /PFAMB/i) {
# 			$linkcomm = "The following Pfam-B families contain sequences that according to Prodom are members of this Pfam-A family." if(!$linkcomm); 
# 		  }
# 		  print $fh "<span CLASS=normaltext>$linkcomm </span>\n";
# 		  #}
# 		  # none of the other links in the list will have comments
# 	    }
# 	    print $fh "</td>\n<td VALIGN=TOP><code>";
# 	    print $fh map { $self->get_database_link($_)." " } @all_links;
# 	    print $fh "</code></td>\n</tr>\n";
# 	  }
    }

    my (@dr_line);
    @dr_line = qw (SYSTERS PANDIT FUNSHIFT);
    
    foreach my $tmp_link (@dr_line) {
      my $ref = Bio::Pfam::Web::PfamWWWConfig::link_mapper()->{$tmp_link}->[0]->{'link'};
      my $id = $entry->id();
      my $acc = $entry->acc();
      if ($tmp_link =~ /SYSTERS/) {
	$ref =~ s/THE_ID/$id/;
      } else {
	$ref =~ s/THE_ID/$acc/;
      }
      print qq "<TR><TD CLASS=normallargetext>$tmp_link</TD><TD><A HREF=$ref>" .$id . "</A> </TD></TR>" if ($entry->id() !~ /Pfam-B/);
    }

    print "</table>\n" if ( ($entry->id() !~ /Pfam-B/)  || ($entry->ann()->get_Annotations('dblink')) );
      
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

    print $fh "<td CLASS=normaltext>\n";
    print $fh "<center>\n";
    print $fh "<form method='get' action=\"$Bio::Pfam::Web::PfamWWWConfig::getspecies\">\n";
    print $fh "<input name=\"acc\" type=\"hidden\" value=\"", $entry->acc, "\">\n";
    print $fh "<input name=\"id\" type=\"hidden\" value=\"", $entry->id, "\">\n";
	print $fh "<img src=$Bio::Pfam::Web::PfamWWWConfig::image_link/new.gif> View alignments & domain organisation by species <BR>";
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

    return "<a href=\"$medlinelink". "$id\">$name</a>";
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
    my $db = &Bio::Pfam::Web::PfamWWWConfig::get_database();

    if ( $line =~ /Pfam\:(\w+\d+)/) {
	while( $line =~ /Pfam\:(\w+\d+)/ ) {
	    $acc = $1;
	    eval {$id = $db->acc2id( $acc );};
	    if ($@) {	
		carp "Could not find accession $acc";
		$line =~ s/Pfam:$acc/[Unknown acc] Pfam:$acc/;
		last;
	    } 
	    else {
		$line =~ s/Pfam:$acc/<a href=\"$Bio::Pfam::Web::PfamWWWConfig::getacc?$acc\">$id<\/a>/;
	    }
	}
    }

    while( $line =~ /SWISSPROT\:(\w+\d+)/ ) {
	my @accs;
	push(@accs, $1);
	$acc = $1;
	my @ef = &Bio::Pfam::Web::PfamWWWConfig::protein_sequence(\@accs, $db);
	$ef = $ef[0];
	if (defined $ef and defined $ef->id()) {
	    $id = $ef->id();
	    $line =~ s/SWISSPROT:$acc/<a href=\"$Bio::Pfam::Web::PfamWWWConfig::swisspfam?name=$acc&acc=$family_acc\">$id<\/a>/;
	}
	else {
	    last;
	}
    }


    while( $line =~ /Swiss\:(\w+\d+)/ ) {
	my @accs;
	push(@accs, $1);
	$acc = $1;
	
       	my @ef = &Bio::Pfam::Web::PfamWWWConfig::protein_sequence(\@accs, $db);
	$ef = $ef[0];
	if (defined $ef and defined $ef->id()) {
	    $id = $ef->id();
	    $line =~ s/Swiss:$acc/<a href=\"$Bio::Pfam::Web::PfamWWWConfig::swisspfam?name=$id&acc=$family_acc\">$id<\/a>/;
	}
	else  {
	    last;
	}
    }

    while( $line =~ /EC:(\d+\.\d+\.\d+\.\d+)/) {
	$id = $1;
	my $primary_ec_link = Bio::Pfam::Web::PfamWWWConfig::link_mapper()->{'EC'}->[0]->{'link'};
	eval("\$temp = \"$primary_ec_link\"");
	$line =~ s/EC:$id/EC:<a href=$temp>$id<\/a>/;
    }

    while( $line =~ /PROSITEDOC:(\w+\d+)/) {
	$id = $1;
	my $prosite_link = Bio::Pfam::Web::PfamWWWConfig::link_mapper()->{'PROSITE'}->[0]->{'link'};
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

    ($add) = $ref->optional_id();
    if( $db eq 'SCOP' ) {
	$id =~ tr/[A-Z]/[a-z]/;
    }
    
    if ( $db eq 'PFAMA' ) {
      $id =~ s/\.\d+//;
	my ($link, $database);
	eval {
	    $database = &Bio::Pfam::Web::PfamWWWConfig::get_database();
	    $otherid = $database->acc2id( $id );
	};
	if ($@) {	
	    carp "Could not find accession $id, $@";
	    $link = "Unknown id";
	} 
	else {
	    $link = "<a href=\'$Bio::Pfam::Web::PfamWWWConfig::getacc?$id\'>$otherid</a>";
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
	  $href = "$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=$_";
	  $link = $link ." ". "<a href=\'$href\'>$_</a>";
	}
      } else {
	$href = "$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=$id"; 
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
      my $mapper = &Bio::Pfam::Web::PfamWWWConfig::link_mapper();

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
      my $mapper = &Bio::Pfam::Web::PfamWWWConfig::link_mapper();

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




sub print_complexs_info {

  my ($self, $fh, $entry)  = @_;


  my $db = &Bio::Pfam::Web::PfamWWWConfig::get_database;
  my $acc = $entry->acc();
#  print "ENTRY: $entry acc: " . $entry->acc() . " <BR>";
  my $complexes = $db->get_complexes($entry->acc(), 1);

  if ($complexes) {

    print $fh "<TR><TD COLSPAN=2 CLASS=normaltext>This family forms <B>structural complexes</B> with other Pfam families, to view them click <A HREF=complexes.pl?acc=" . $entry->acc(). ">here</A>";

   print $fh "</TD></TR>";
 }

  return $fh, 1;
}


=head2 print_interaction_info

Will look in the interaction table to see if this domain froms an interaction !!!!
Need a bit more detail here.

=cut

sub print_interaction_info {
	my ($self, $fh, $entry)  = @_;
	my $db = &Bio::Pfam::Web::PfamWWWConfig::get_database;
	my %interaction = $db->get_interacting_domains($entry->acc());
	if (keys %interaction){
		print $fh "<TR><TD COLSPAN=2 CLASS=normaltext><IMG SRC=\"" . $Bio::Pfam::Web::PfamWWWConfig::image_link . "/new.gif\">This family forms <B>interactions</B> with other Pfam families, to view them click <A HREF=interaction_domains.pl?acc=" . $entry->acc(). ">here</A>";
		 print $fh "</TD></TR>";
	}
	return $fh, 1;
}

#===============================================================================
# new subroutines... (new as of 20051026 at least)

# these are essentially copies of the same routine, which formats a
# couple of extra rows in the link tables

# write links for A versus A/B comparisons

# arguments: scalar - reference to the "entry" object
# returns:   none - writes HTML to STDOUT

sub writePfamALinks( $ ) {

  my $entry = shift;

  my $query = "select T2.pfamA_id \
	           from  pfamA as T1 \
	           right join pfamA2pfamA_PRC_results \
                 on T1.auto_pfamA = pfamA2pfamA_PRC_results.auto_pfamA1 \
   	           left  join pfamA as T2 \
                 on T2.auto_pfamA = pfamA2pfamA_PRC_results.auto_pfamA2 \
	           where evalue <= 0.001 \
               and   T1.auto_pfamA != T2.auto_pfamA \
	           and   T1.pfamA_acc = \'" . $entry->acc() . "\'";

  my $db = Bio::Pfam::Web::PfamWWWConfig::get_database();

  my( %atobBOTH, %atobPRC, %atobPRODOM, @atoa );
  foreach ( $db->query( $query ) ) {
	push @atoa, $_->[0];
  }

  # select T1.pfamA_id, T2.pfamA_id
  # from  pfamA as T1
  # right join pfamA2pfamA_PRC_results on T1.auto_pfamA=pfamA2pfamA_PRC_results.auto_pfamA1
  # left  join pfamA as T2             on T2.auto_pfamA=pfamA2pfamA_PRC_results.auto_pfamA2
  # where evalue<= 0.001
  # and   T1.pfamA_id="SH3_1";

  $query = "select pfamB_acc \
	        from   pfamB2pfamA_PRC_results, \
	               pfamB, \
                   pfamA \
	        where  pfamA.auto_pfamA = pfamB2pfamA_PRC_results.auto_pfamA \
            and    pfamB.auto_pfamB = pfamB2pfamA_PRC_results.auto_pfamB \
	        and    evalue <= 0.001 \
	        and    pfamA_acc=\'" . $entry->acc() . "\'";

  foreach ( $db->query( $query ) ) {
	$atobPRC{$_->[0]} = "PRC";
  }

  # select pfamA_id, pfamB_acc
  # from   pfamB2pfamA_PRC_results,
  #        pfamB,
  #        pfamA
  # where  pfamA.auto_pfamA = pfamB2pfamA_PRC_results.auto_pfamA
  # and    pfamB.auto_pfamB = pfamB2pfamA_PRC_results.auto_pfamB
  # and    evalue <= 0.001
  # and   (pfamA_id=\'$fam\');

  # I'm sure that one of these loops should be redundant, but hey, let's
  # not mess with it while it appears to work...
  foreach my $link ( [ $entry->ann()->get_Annotations('dblink') ] ) {
	foreach my $item ( @{$link} ) {
	  next unless $item->database() =~ /PFAMB/;

	  my $idString = $item->primary_id();
	  $idString =~ s/^\s+(.*)/$1/;

	  foreach ( split /\s+/, $idString ) {
		$atobPRODOM{$_} = "PRODOM";
	  }
	}
  }

  # find the list of accessions in both the PRC and PRODOM lists
  foreach ( keys %atobPRC, keys %atobPRODOM ) {
	$atobBOTH{$_} = "" if( exists( $atobPRC{$_} ) and exists( $atobPRODOM{$_} ) );
  }

  # and then prune out those accessions that are in both lists
  foreach ( keys %atobPRC ) {
	delete $atobPRC{$_} if exists $atobBOTH{$_};
  }

  foreach ( keys %atobPRODOM ) {
	delete $atobPRODOM{$_} if exists $atobBOTH{$_};
  }

  # dump the A vs A row
  if ( scalar @atoa ) {
	print $cgi->start_Tr;
	print $cgi->td( { -style => "vertical-align: top",
					  -class => "left" },
					$cgi->span( { -class => "normallargetext" }, "PFAMA" ) );
	# additional comment for the left column ?
	# "The family contains sequences that according to Prodom are
	# related to the following Pfam-A family"

	print $cgi->start_td;
	print $cgi->start_div( { -class => "highlightedblock pfama" } );
	print $cgi->a( { -href => "javascript:void();",
					 -class => "tooltipped",
					 -onMouseOver => "return overlib( 'PfamA relationships can only be identified by PRC', CAPTION, 'PRC only' );",
					 -onMouseOut =>  "return nd();" },
				   "PRC only:" );
	foreach ( @atoa ) {
	  print $cgi->a( { -href => $cgi->url . "?$_" }, $_ ), " ";
	}
	print $cgi->end_div;
	print $cgi->end_td;
	print $cgi->end_Tr;
  }

  # count the lines that we have to dump out
  my $linesToDump = scalar( keys %atobBOTH )
	+ scalar( keys %atobPRC ) 
	  + scalar( keys %atobPRODOM );
		
  # do we have any ?
  if ( $linesToDump ) {

	print $cgi->start_Tr;
	print $cgi->td( { -style => "vertical-align: top",
					  -class => "left" },
					$cgi->span( { -class => "normallargetext" }, "PFAMB" ) );
	print $cgi->start_td;

	if ( scalar keys %atobBOTH ) {
	  print $cgi->start_div( { -class => "highlightedblock both" } );
	  print $cgi->a( { -href => "javascript:void();",
					   -class => "tooltipped",
					   -onMouseOver => "return overlib( 'These entries have been identified as being related to this entry using both PRC and PRODOM', CAPTION, 'PRC + PRODOM' );",
					   -onMouseOut =>  "return nd();" },
					 "PRC + PRODOM:" );
	  foreach ( sort keys %atobBOTH ) {
		print $cgi->a( { -href => "$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=$_" }, "$_" ), " ";
	  }
	  print $cgi->end_div;
	}

	if ( scalar keys %atobPRC ) {
	  print $cgi->start_div( { -class => "highlightedblock prc" } );
	  print $cgi->a( { -href => "javascript:void();",
					   -class => "tooltipped",
					   -onMouseOver => "return overlib( 'These entries have been identified as being related to this entry using only PRC', CAPTION, 'PRC only' );",
					   -onMouseOut =>  "return nd();" },
					 "PRC only:" );
	  foreach ( sort keys %atobPRC ) {
		print $cgi->a( { -href => "$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=$_" }, "$_" ), " ";
	  }
	  print $cgi->end_div;
	}

	if ( scalar keys %atobPRODOM ) {
	  print $cgi->start_div( { -class => "highlightedblock prodom" } );
	  print $cgi->a( { -href => "javascript:void();",
					   -class => "tooltipped",
					   -onMouseOver => "return overlib( 'These entries have been identified as being related to this entry using only PRODOM', CAPTION, 'PRODOM only' );",
					   -onMouseOut =>  "return nd();" },
					 "PRODOM only:" );
	  foreach ( sort keys %atobPRODOM ) {
		print $cgi->a( { -href => "$Bio::Pfam::Web::PfamWWWConfig::getpfamb?acc=$_" }, "$_" ), " ";
	  }
	  print $cgi->end_div;
	}

	print $cgi->end_td;
	print $cgi->end_Tr;

  } # end of "if $linesToDump"

  # done
}

#-------------------------------------------------------------------------------
# write links for B versus A/B comparisons. Just a plain copy of the
# previous routine... ugly, I know, but in keeping with the rest of
# the code in here

# arguments: scalar - reference to the "entry" object
# returns:   none - writes HTML to STDOUT

sub writePfamBLinks( $ ) {

  my $entry = shift;

  my $query = "select T2.pfamB_acc \
	           from  pfamB as T1 \
	           right join pfamB2pfamB_PRC_results \
                 on T1.auto_pfamB = pfamB2pfamB_PRC_results.auto_pfamB1 \
   	           left  join pfamB as T2 \
                 on T2.auto_pfamB = pfamB2pfamB_PRC_results.auto_pfamB2 \
	           where evalue <= 0.001 \
               and   T1.auto_pfamB != T2.auto_pfamB \
	           and   T1.pfamB_acc = \'" . $entry->acc() . "\'";

  my $db = Bio::Pfam::Web::PfamWWWConfig::get_database();

  my( %btoaBOTH, %btoaPRC, %btoaPRODOM, @btob );
  foreach ( $db->query( $query ) ) {
	push @btob, $_->[0];
  }

  my $query = "select pfamA_acc \
	           from   pfamB2pfamA_PRC_results, \
	                  pfamB, \
                      pfamA \
	           where  pfamA.auto_pfamA = pfamB2pfamA_PRC_results.auto_pfamA \
               and    pfamB.auto_pfamB = pfamB2pfamA_PRC_results.auto_pfamB \
	           and    evalue <= 0.001 \
	           and    pfamB_acc=\'" . $entry->acc() . "\'";

  foreach ( $db->query( $query ) ) {
	$btoaPRC{$_->[0]} = "PRC";
  }

  # straight from the database...
  foreach my $link ( [ $entry->ann()->get_Annotations('dblink') ] ) {
	foreach my $item ( @{$link} ) {
	  print STDERR "database: ",$item->database(),"\n";
	  next unless $item->database() =~ /PFAMA/;

	  my $idString = $item->primary_id();
	  $idString =~ s/^\s+(.*)/$1/;

	  my $entryAcc;
	  foreach ( split /\s+/, $idString ) {
		s/^(.*?)\.\d+$/$1/; # strip off version numbers...
		$btoaPRODOM{$_} = "PRODOM";
	  }
	}
  }

  # find the list of accessions in both the PRC and PRODOM lists
  foreach ( keys %btoaPRC, keys %btoaPRODOM ) {
	$btoaBOTH{$_} = "" if( exists( $btoaPRC{$_} ) and exists( $btoaPRODOM{$_} ) );
  }

  # and then prune out those accessions that are in both lists
  foreach ( keys %btoaPRC ) {
	delete $btoaPRC{$_} if exists $btoaBOTH{$_};
  }

  foreach ( keys %btoaPRODOM ) {
	delete $btoaPRODOM{$_} if exists $btoaBOTH{$_};
  }

  # count the lines that we have to dump out
  my $linesToDump = scalar( keys %btoaBOTH )
	+ scalar( keys %btoaPRC ) 
	  + scalar( keys %btoaPRODOM );
		
  # do we have any ?
  if ( $linesToDump ) {

	print $cgi->start_Tr;
	print $cgi->td( { -style => "vertical-align: top",
					  -class => "left" },
					$cgi->span( { -class => "normallargetext" }, "PFAMA" ) );
	print $cgi->start_td;

	if ( scalar keys %btoaBOTH ) {
	  print $cgi->start_div( { -class => "highlightedblock both" } );
	  print $cgi->a( { -href => "javascript:void();",
					   -class => "tooltipped",
					   -onMouseOver => "return overlib( 'These entries have been identified as being related to this entry using both PRC and PRODOM', CAPTION, 'PRC + PRODOM' );",
					   -onMouseOut =>  "return nd();" },
					 "PRC + PRODOM:" );
	  # convert the accessions to id's along the way
	  foreach ( sort keys %btoaBOTH ) {
		print $cgi->a( { -href => "$Bio::Pfam::Web::PfamWWWConfig::getacc?$_" },
					   $db->acc2id($_) ), " ";
	  }
	  print $cgi->end_div;
	}

	if ( scalar keys %btoaPRC ) {
	  print $cgi->start_div( { -class => "highlightedblock prc" } );
	  print $cgi->a( { -href => "javascript:void();",
					   -class => "tooltipped",
					   -onMouseOver => "return overlib( 'These entries have been identified as being related to this entry using only PRC', CAPTION, 'PRC only' );",
					   -onMouseOut =>  "return nd();" },
					 "PRC only:" );
	  foreach ( sort keys %btoaPRC ) {
		print $cgi->a( { -href => "$Bio::Pfam::Web::PfamWWWConfig::getacc?$_" },
					   $db->acc2id($_) ), " ";
	  }
	  print $cgi->end_div;
	}

	if ( scalar keys %btoaPRODOM ) {
	  print $cgi->start_div( { -class => "highlightedblock prodom" } );
	  print $cgi->a( { -href => "javascript:void();",
					   -class => "tooltipped",
					   -onMouseOver => "return overlib( 'These entries have been identified as being related to this entry using only PRODOM', CAPTION, 'PRODOM only' );",
					   -onMouseOut =>  "return nd();" },
					 "PRODOM only:" );
	  foreach ( sort keys %btoaPRODOM ) {
		print $cgi->a( { -href => "$Bio::Pfam::Web::PfamWWWConfig::getacc?$_" },
					   $db->acc2id($_) ), " ";
	  }
	  print $cgi->end_div;
	}

	print $cgi->end_td;
	print $cgi->end_Tr;

  } # end of "if $linesToDump"

  # dump the B vs B row
  if ( scalar @btob ) {
	print $cgi->start_Tr;
	print $cgi->td( { -style => "vertical-align: top",
					  -class => "left" },
					$cgi->span( { -class => "normallargetext" }, "PFAMB" ) );
	# additional comment for the left column ?
	# "The family contains sequences that according to Prodom are
	# related to the following Pfam-A family"

	print $cgi->start_td;
	print $cgi->start_div( { -class => "highlightedblock pfama" } );
	print $cgi->a( { -href => "javascript:void();",
					 -class => "tooltipped",
					 -onMouseOver => "return overlib( 'PfamB relationships can only be identified by PRC', CAPTION, 'PRC only' );",
					 -onMouseOut =>  "return nd();" },
				   "PRC only:" );
	foreach ( @btob ) {
	  print $cgi->a( { -href => $cgi->url . "?acc=$_" }, $_ ), " ";
	}
	print $cgi->end_div;
	print $cgi->end_td;
	print $cgi->end_Tr;
  }

  # done
}
