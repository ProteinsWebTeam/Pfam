#!/usr/local/bin/perl -- -*-perl-*-

#my $bio_lib = $RfamWWWConfig::bioperl_dir;
#use lib '$bio_lib';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';

use RfamWWWConfig;
use CGI;
#use GIFAlign;
#use drawing;
#use MEMAlign;
use Paging;
use Paging;
#use AnnSeq2WWW;  

$| = 1;

$query = new CGI;


$acc         = $query->param('acc');
$dom_name    = $query->param('name');
$aln_type    = $query->param('type');
#print "Content-type: text/html\n\n";
#print "Content-type: text/html\n\n";

if ($acc !~ /^RF/i) {

 &RfamWWWConfig::user_error("This is not a valid Rfam accession number", 1);
}




#print "Content-type: text/html\n\n";


#$ali = &RfamWWWConfig::get_alignment($acc,$aln_type,1);




########## 



my $en;
my $db = &RfamWWWConfig::get_database();
my $header;
use Paging;

### HEADER TEXT
if ($aln_type =~ /SEED/i) {
  $header = "Representative ";
} else {
  $header = "All ";
}
$header .=  $dom_name;

if ( $acc =~ /^RF/)  {
  
  $en = $db->get_Entry_by_acc($acc);
  
  $header .=  " " . lc($en->entry_type()) . " sequences";
  
  #/ end header text
  

  
} else {
  
  $header .=   " sequences";
}


#### Used for pfamB families :-)
#print "ALIGNMENT: $ali <P>";
## up cast to MEMAlign object
#bless ($ali,'MEMAlign');

#$ali->id($query->param('name'));
##my $log = "View Family Graphics $aln_type : $acc ($dom_name)";
##&RfamWWWConfig::logs($log);

print $query->header();
##print &RfamWWWConfig::header($header, $dom_name ,$acc );
##print "<CENTER><span class=normallargetext>$header</span></CENTER>";


my $AnnSeq = &RfamWWWConfig::get_member_seqs($acc, $aln_type);

#my $AnnSeq = @{$temp};
#print "ANNSEQ: $AnnSeq <P>";
#print "FILL: @AnnSeq <P>";
 my ($p,$h,$f,$famname);

    $p = new Paging;
    $p->dir($RfamWWWConfig::tempdir);
    $p->chunk(500);
    $p->maxkbytes(2000); #2 Mbytes in the cache system
    $p->print(&RfamWWWConfig::header("$header", $dom_name, $acc));
    $h = &RfamWWWConfig::header("$header head", $dom_name, $acc);
    $f = "<P><P>" . &RfamWWWConfig::footer();
 #   $p->print(&RfamWWWConfig::header("$header", $dom_name, $acc));
$p->print("<CENTER><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD>");

$p->print("<TD  BGCOLOR=#000070 CLASS=whitetableheader>Bits Score</TD>") if ($aln_type =~ /FULL/i);
$p->print("</TR>");

    $p->header("$h <p>$header (cont...)  <P><CENTER><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD>"); 
 $p->header("<TD  BGCOLOR=#000070 CLASS=whitetableheader>Bits Score</TD>")if ($aln_type =~ /FULL/i);
 $p->header("</TR>");

#$p->print("</TABLE></CENTER><P>$f");
    $p->footer("</TABLE></CENTER><P><hr>Next page <a href=\\\"$RfamWWWConfig::image_temp/\$next_file\\\">here</a><p>\Q$f\E");
#exit(0);
#foreach $seq ( @AnnSeq) {
  my $link = $RfamWWWConfig::srsserver;
  foreach my $reg ($AnnSeq->eachAnnotatedRegion) {
    
 #   print "reg: $reg <P>";
    
   # print "BOO $seq <P>";
   # exit(0);
    my $rfamseq_acc = $reg->rfamseq_id();
    # print "ACC: $acc <P>";
    my $start = $reg->model_from();
    my $end = $reg->model_to();
    my $bits = $reg->bits_score();
    $link =~ s/ACC/$rfamseq_acc/;
    
    $p->print ("<TR><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour ><A HREF=$link>$rfamseq_acc</A></TD>");
    $p->print("<TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>$start</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>$end</TD>");
    $p->print("<TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>$bits</TD>") if ($aln_type =~ /FULL/i);
    $p->print("</TR>");

    $p->break;
  }
#  last; ## temp fix havent quit got regions and sequences code sorted yet!
#}

$p->print("</TABLE></CENTER><P>$f");

##print "BOO <P>";
#### UPDATE for new_select_order
#$ali->view_all_figures($header, $verbose, $acc, $dom_name, $all_params, %new_select_order);
