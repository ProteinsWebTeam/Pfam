#
# Perl Module for EntryB2WWW
#
# Cared for by Kevin Howe <klh@sanger.ac.uk>
#
#Copyright Genome Research Limited (1997). Please see information on licensing in LICENSE


# POD documentation - main docs before the code

=head1 NAME

EntryAWWW - Functionality to write an html version of an Entry

=head1 SYNOPSIS
    use EntryAWWW;

    $ent = $db->get_EntryB_by_acc($acc);
    $ent2www = EntryB2WWW->new( '-entry' => $ent );
    $ent2www->write_table(\*STDOUT);

=head1 DESCRIPTION

This class takes an Bio::Pfam::EntryA in construction, and provides
methods for generating html versions of the entry.

=head1 CONTACT

Mail pfam@sanger.ac.uk with any queries


=cut



package Bio::Pfam::Web::EntryB2WWW;

use vars qw(@ISA);
use Carp;
use Exporter;
use strict;

use Bio::Pfam::Web::PfamWWWConfig;
use Bio::Pfam::Web::Entry2WWW;

@ISA = qw(Bio::Pfam::Web::Entry2WWW Exporter);



sub write_table {
    my $self = shift;
    my $fh = shift;

    my $entry = $self->entry();
    if (not defined $entry) {
	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Error: There is no entry to write");
    }

    # first, the key information

    print $fh "<table border=1  bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour cellpadding=5 cellspacing=0 cols=1 width='100%'>\n";
    print "<tr><td CLASS=whitetableheader bgcolor=000070>Accession number: ", $entry->acc(), "</td></tr>";
    print "<tr><td CLASS=normaltext>If you have information about this family then please tell us by clicking the <a href=$Bio::Pfam::Web::PfamWWWConfig::addcomments?id=", $entry->id(), "&acc=", $entry->acc ,"><img src=$Bio::Pfam::Web::PfamWWWConfig::image_link/annotation.gif border=0></A> button</td></tr>";

    if ($entry->ann()->get_Annotations('comment')) {
	print "<tr><td CLASS=normaltext>\n";
	#foreach my $line ( $entry->ann->flatcomment->each_flat()) {
	    my $line = $self->sub_reference_line( $entry->ann()->get_Annotations('comment'), $entry->acc() );
	    print $fh "$line";
	#} 
	print "<td ><tr  CLASS=normaltext>\n";
    }

 $self->print_complexs_info($fh, $entry); 

   print "</table><p>\n";

	print $fh "
	<table align=center border=1 cellpadding = 5 cellspacing=0 cols=2 bgcolor=$Bio::Pfam::Web::PfamWWWConfig::pfamcolour >
<tr>

<td valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Alignment</TD>
<td valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Domain organisation</TD>
<td valign=top CLASS=whitetableheader ALIGN=CENTER bgcolor=000070>
Species Distribution</TD>
</TR>";

    $self->write_user_interaction_table( $fh );

    $self->write_species_table( $fh );

	print $fh "</tr></table>";

   
    $self->write_links_table( $fh );

 # now, the references and links
    $self->write_references_table( $fh );

    # print "<hr noshade size=4>";

    # $self->write_non_entry_information( $fh );
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
    
    my ($id, $acc, $fcount);

    my $entry = $self->entry();
    if (not defined $entry) {
	&Bio::Pfam::Web::PfamWWWConfig::exit_html("Error: There is no entry to write");
    }

    $id = $entry->id();
    $acc  = $entry->acc();

    $fcount = $entry->num_seqs_in_full();
    print $fh <<EOF;

<tr>

<td>

<table border=0 cols=1>
<tr>
<td CLASS=normaltext>
<form METHOD="GET" ACTION="$Bio::Pfam::Web::PfamWWWConfig::getalign">
<input name="name" type="hidden" value="$id">
<input name="acc" type="hidden" value="$acc">
<input name=type type=hidden value='full'>
Format <select name=format >
<option value=link> Coloured alignment
<option value=mul> Plain Pfam format
<option value=stock> Plain Stockholm format
<option value=fal> Fasta format (with gaps)
<option value=msf> MSF format
<option value=belvu> Belvu (unix only)
<option value=belold> Belvu (old version unix only) 
<option value=jalview> Jalview (Java)
</select>
<br><br><br>
<input type=submit value="Get alignment">($fcount sequences)
</form>
</td>
</tr>

<tr valign=top>
<td  CLASS=normaltext>
Help relating to Pfam alignments <a href="$Bio::Pfam::Web::PfamWWWConfig::align_help">here</a>
</td>
</tr>
</table>
</td>

<td valign=top>
<form METHOD="GET" ACTION="$Bio::Pfam::Web::PfamWWWConfig::getall">
<input name="name" type="hidden" value="$id">
<input name="acc" type="hidden" value="$acc">
<input name="type" type="hidden" value="full">
<input name="verbose" type="hidden" value="true">
<table  CLASS=normaltext width="100%">
<th><center>As a Graphic</center></th>
<tr>
<td>
<center>
Zoom <input name=zoom_factor type=text value="0.5" size=3 maxlength=3> pixels/aa.
</center>
</td>
</tr>

<tr valign=bottom>
<td>
<center>
<input type=submit name="list" value="View Graphic">
</center>
</td>
</tr>

</table>
</form>

</td>
<P>

EOF

}


1;
