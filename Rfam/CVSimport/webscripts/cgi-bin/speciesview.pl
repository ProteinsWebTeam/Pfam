#! /usr/local/bin/perl -T

use strict;

use lib './';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';
use CGI;
use RfamWWWConfig;
use Tree;
use GetzTax;
#use GIFAlign;
#use Bio::Rfam::SeqRfam;

$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};

my ( $q, $zoom, $family, $name, $print_name, $tree, @list, @ids, $galign );

$q = new CGI;
$zoom = 0.5;

print $q->header;

my $temp = $q->param('family');
$family = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp = $q->param('acc');
my $acc = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp = $q->param('name'); 
$name = $1 if ($temp =~  /^([-\@~\w\s+.]+)$/);


print &RfamWWWConfig::header("Members in $name for family $family", $family, $acc);
#print "<hr noshade size=2>";
#print "Rfam-A domains are large boxes; small three-colored boxes are Rfam-B domains.<br>";
#print "Mouseover to see domain descriptions; click on box to enter family page.<br>";
#print "(This page uses javascript for the mouseover functionality. Make sure you have javascript enabled in your browser)";
#print "<hr noshade size=2>\n";

my %values;
#(%values) = GetzTax::_get_full_names($acc, %values);


# foreach my $key (sort keys %values) {

#   print "KEY: $key, val: " .$values{$key} . " <BR>";
# }

 GetzTax::_file_domain_species($acc, \@list);
#print " VALS: " . %values. " <P>";
#GetzTax::_rdb_domain_species($family, \@list);

#if (!@list) {
#  open (FH, "$RfamWWWConfig::getz -t \"[$RfamWWWConfig::srs_pfam-id:$family]>$RfamWWWConfig::srs_db\" |" );
#  GetzTax::get( \*FH, $family, \@list );
#}


$tree = Tree -> new();
$tree -> grow_tree( \@list, ';' );
$tree -> get_ids( $name, \@ids );

#$galign = GIFAlign->new();


print qq(<TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD></TR>





);


foreach my $seqid (@ids) {
  my $link = $RfamWWWConfig::srsserver;
  $link =~ s/ACC/$seqid/;
  
 # print "SEQ: $seqid <P>";
  my($annseq) = &RfamWWWConfig::species_get_sequence($seqid, $acc);
  foreach my $region ($annseq->eachAnnotatedRegion()) {
    print "<TR><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour ><A HREF=$link>$seqid</A></TD>";
    print "<TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>" . $region->model_from() . "</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>" . $region->model_to(). "</TD></TR>";

  
  }
}


print "</TABLE><P>";

print &RfamWWWConfig::footer();
#$galign -> view_all_figures( $zoom, 1, $acc);

#&RfamWWWConfig::logs("Species domain organisation in $name for family $family");

