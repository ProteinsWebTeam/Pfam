#!/usr/local/bin/perl  -T

#
# swisspfamget - gets a swisspfam entry and html'ifies
#  it...
#
#
#


use strict;
use CGI;
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';
use lib './';

use RfamWWWConfig;
#use AnnSeq2WWW;


$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};

#use lib qw(PfamWWWConfig);
#use lib qw(AnnSeq2WWW);

my ($q, $name, $id,  $desc, $sp, $entry, $annseq, $zoom, $pictures);

$q = new CGI;

print $q->header();

#
# ok. get into the real stuff
#

my $temp_name = $q->param('name');
$name = $1 if ($temp_name =~  /^([\-\_\@\w\s+.]+)$/);
#my $temp_zoom = $q->param('zoom_factor');
#$zoom = $1 if ($temp_zoom =~  /^([-\@\w.]+)$/);
#if (! defined($zoom) ) {
#    $zoom = 0.5;
#}

my $temp_name = $q->param('acc');
my $acc = $1 if ($temp_name =~  /^([\-\_\@\w\s+.]+)$/);

if ( (!$acc) && (!$name) ) {
  &RfamWWWConfig::user_error("You need to enter EMBL name or accession number");

}



my $annseq;
if ($name) {
  $annseq = &RfamWWWConfig::get_sequence($name, "name");
} else {
  $annseq = &RfamWWWConfig::get_sequence($acc, "acc");
}
#my($overlaps) = $pictures->display_domains_gif( $annseq, $zoom, 1, 1 , %new_select_order);
##my($overlaps) = $pictures->display_domains_gif( $annseq, $zoom, 1, 1 , $id, %new_select_order); ## download gif code


#print "SEQ: $annseq <P>";

if ($annseq eq "1" ) {

  print &RfamWWWConfig::header("EMBL entry for $name $acc", );

  print "<span class=normallargetext><center>$name $acc does not match any Rfam families</center></span><P>";

  print &RfamWWWConfig::footer();
} elsif ($annseq)  {

print &RfamWWWConfig::header("EMBL entry for $name $acc ", );

#print "ANNSEQ: $annseq <P>";

print("<CENTER><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>Rfam family</TD><TD BGCOLOR=#000070 CLASS=whitetableheader>Description</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD BGCOLOR=#000070 CLASS=whitetableheader>End</TD></TR>");

foreach my $region ($annseq->eachAnnotatedRegion()) {
  print "<TR><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour><A HREF=/cgi-bin/Rfam/getacc?" .$region->accession . ">" .$region->id . "</A></TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>" . $region->annotation() . "</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>" .$region->model_from . "</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>" . $region->model_to. " </TD> </TR>";
#  print "REGION: $region " .$region->from . " to " .$region->to . " acc: " .$region->accession . " id: " .$region->id . " desc:" .$region->annotation()->description() . " <P>";


  

}


print "</TABLE></CENTER><P><P>";


print "<P><P>";
  print &RfamWWWConfig::footer();

} else {
  

#  print "HERE <P>";
  &RfamWWWConfig::user_error(" <font color=#ff0000>\'$name $acc\'</font> is not recognised as a valid EMBL accession/name ");
}
#$pictures->print_key(%new_select_order);


#print "</TD></TR></TABLE>";

#$pictures->display_domains_table($annseq, $id, %new_select_order, $overlaps);



#my $order_changed;
#if($q->param('select0')) {
#	$order_changed = "order changed";

#}
#&PfamWWWConfig::logs("swiss-prot $order_changed : $id");
