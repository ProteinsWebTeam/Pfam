#!/usr/local/bin/perl -T

#my $bio_lib = $RfamWWWConfig::bioperl_dir;
#use lib '$bio_lib';
use lib './';
$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};

use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';

use RfamWWWConfig;
use CGI;
use strict;
#use GIFAlign;
#use drawing;
#use MEMAlign;
use Paging;
#use AnnSeq2WWW;  

$| = 1;

my $query = new CGI;

my $tmp_acc = $query->param('acc');
my $tmp_name = $query->param('name');
my $tmp_aln_type = $query->param('type');

my $acc         = $1 if ($tmp_acc =~ /^([\-\_\@\w\s+.]+)$/);
my $dom_name    = $1 if ($tmp_name =~ /^([\-\_\@\w\s+.]+)$/);
my $aln_type    = $1 if ($tmp_aln_type =~ /^([\-\_\@\w\s+.]+)$/);
#print "Content-type: text/html\n\n";
#print "Content-type: text/html\n\n";

if ($acc !~ /^RF/i) {

 &RfamWWWConfig::user_error("This is not a valid Rfam accession number", 1);
}


my ($db, $en);

eval {

    $db = &RfamWWWConfig::get_database();

     $en = $db->get_Entry_by_acc( $acc );

    $dom_name = $en->id();

};


if ( ($en->num_seqs_in_full() > 10000) &&  ($aln_type =~ /full/) ) {
  print "Content-type: text/html\n\n";
  print &RfamWWWConfig::header( "Member sequences for $dom_name" , $dom_name, $acc);
  print "<span class=normalmediumtext>This family contains too many members in the full alignment to display in the browser. <P>Instead you can download the full alignment from <A href=$RfamWWWConfig::WWW_root/data/full/$acc.full.gz>here</A><P>";
  print &RfamWWWConfig::footer();
  exit(0);
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

my $pagehead = &RfamWWWConfig::header("$header");

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
# my ($p,$h,$f,$famname);

#    $p = new Paging;
#    $p->dir($RfamWWWConfig::tempdir);
#    $p->chunk(500);
#    $p->maxkbytes(2000); #2 Mbytes in the cache system
    #$p->print(&RfamWWWConfig::header("$header", $dom_name, $acc));
#    $h = &RfamWWWConfig::header("$header head", $dom_name, $acc);
 print  &RfamWWWConfig::header("$header ", $dom_name, $acc);  
  #  $f = "<P><P>" . &RfamWWWConfig::footer();
 #   $p->print(&RfamWWWConfig::header("$header", $dom_name, $acc));
  print "<CENTER><form method=GET ACTION=/cgi-bin/Rfam/getfasta.pl ><table><tr><td><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader WRAP>Description</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD>";



  print "<TD  BGCOLOR=#000070 CLASS=whitetableheader>Bits Score</TD>" if ($aln_type =~ /FULL/i);
#$h .= "</TR>";
 print "<TD  BGCOLOR=#000070 CLASS=whitetableheader>Get Seq</TD>";
#$p->header($h);

#print $p->header();
#$p->header("BOO <P>");
#    $p->header("$h <p>$header (cont...)  <P><CENTER><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>EMBL Accession number</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD  BGCOLOR=#000070 CLASS=whitetableheader>End</TD>"); 
# $p->header("<TD  BGCOLOR=#000070 CLASS=whitetableheader>Bits Score</TD>")if ($aln_type =~ /FULL/i);
# $p->header("</TR>");

#$p->print("</TABLE></CENTER><P>$f");
   # $p->footer("</TABLE></CENTER><P><hr>Next page <a href=\\\"$RfamWWWConfig::image_temp/\$next_file\\\">here</a><p>\Q$f\E");
#exit(0);
#foreach $seq ( @AnnSeq) {
  #print "HERE <P>";
#exit(0);
my $count = 0;
  foreach my $reg ($AnnSeq->eachAnnotatedRegion) {
    my $link = $RfamWWWConfig::srsserver;
 #   print "reg: $reg <P>";
    
   # print "BOO $seq <P>";
   # exit(0);
    my $rfamseq_acc = $reg->rfamseq_id();
    my $rfamseq_acc_version = $reg->version();
    # print "ACC: $acc <P>";
    my $start = $reg->model_from();
    my $end = $reg->model_to();
    my $bits = $reg->bits_score();
    my $desc = $reg->annotation();
    $link =~ s/THEACC/$rfamseq_acc/;
  #  <form method=POST ACTION=/cgi-bin/Rfam/getfasta.pl><input type=hidden name=acc value=$acc><input type=hidden name=start value=$start><input type=hidden name=aln_type value=$aln_type><input type=hidden name=rfamseq_acc value=$rfamseq_acc><input type=hidden name=end value=$end><Input type=Submit Value=\"Get Sequence\" onClick='w=window.open(\"getfasta.pl?acc=$acc\", \"helpwindow\", \"width=450, height=410, scrollbars=yes,resizable=yes\");w.focus();'>  </form>

    if (length($desc) > 70 ) {
      my $temp = substr($desc, 0, 70);
      $temp .= "...";
      $desc = $temp;
    }
    print ("<TR><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour ><A HREF=$link><B>$rfamseq_acc_version</B></A></TD>");
    print ("<TD WRAP valign=center align=left  class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour>$desc</TD><TD NOWRAP valign=center align=left  class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour>$start</TD><TD NOWRAP valign=center align=left  class=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour>$end</TD>");


    print("<TD NOWRAP valign=center align=left  CLASS=normaltext BGCOLOR=$RfamWWWConfig::rfamcolour>$bits</TD>") if ($aln_type =~ /FULL/i);

    print("<TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour><input type=checkbox name=$count value= " . "$rfamseq_acc" . "/" . $start . "-" . $end . ">
  </TD>");
    print("</TR>");

    $count++;
#  <form name=taxonomy method=POST enctype='multipart/form-data' action=\"/cgi-bin/Pfam/complexes.pl\"><input type=hidden name=acc ><input type=button name=/cgi-bin/Rfam/getfasta.pl?acc=$acc&start=$start&end=$end&aln_type=$aln_type&rfamseq_acc=$rfamseq_acc value=\"Get Sequence\" onClick=\"javascript:EepCopyName(this.form , this.name );\"></form>

#    $p->break;
  }
#  last; ## temp fix havent quit got regions and sequences code sorted yet!
#}

print "</TABLE></TD></TR><tr><tr><tr><td align=right><input type=hidden name=max_count value=$count><input type=hidden name=acc value=$acc><input type=hidden name=aln_type value=$aln_type><input type=submit  value=\"View Selected Sequences\" ></form></TD></TR></TABLE></CENTER><P>" ;
print &RfamWWWConfig::footer();
##print "BOO <P>";
#### UPDATE for new_select_order
#$ali->view_all_figures($header, $verbose, $acc, $dom_name, $all_params, %new_select_order);
&RfamWWWConfig::logs("MEMBER:$aln_type");
