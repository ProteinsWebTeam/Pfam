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


print qq(<form method=GET ACTION=/cgi-bin/Rfam/getfasta.pl ><table><tr><td><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader>Description</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>Get Seq</TD></TR>





);

my $count = 0;
foreach my $seqid (@ids) {
  my $link = $RfamWWWConfig::srsserver;
  $link =~ s/THEACC/$seqid/;
  
 # print "SEQ: $seqid <P>";
  my($annseq) = &RfamWWWConfig::species_get_sequence($seqid, $acc);
  foreach my $region ($annseq->eachAnnotatedRegion()) {
    my $desc = $region->annotation();
    if (length($desc) > 70 ) {
      my $temp = substr($desc, 0, 70);
      $temp .= "...";
      $desc = $temp;
    }
    print "<TR><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour ><A HREF=$link>" .$region->version() . "</A></TD>";
     print "<TD WRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour class=normaltext >$desc</TD>";
    print "<TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour class=normaltext>" . $region->model_from() . "</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour class=normaltext>" . $region->model_to(). "</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour><input type=checkbox name=$count value= " . "$seqid" . "/" . $region->model_from()  . "-" . $region->model_to()  . ">      </TD></TR>";

    $count++;
  }
}


print "</TABLE></td></tr><TR><TR><tr><td ALIGN=RIGHT><input type=hidden name=max_count value=$count><input type=hidden name=acc value=$acc><input type=hidden name=aln_type value=full><input type=submit  value=\"View Selected Sequences\" ></TD></TR></form><P>";

print &RfamWWWConfig::footer();
&RfamWWWConfig::logs("SPECIESVIEW:$acc:$name");
#$galign -> view_all_figures( $zoom, 1, $acc);

#&RfamWWWConfig::logs("Species domain organisation in $name for family $family");

