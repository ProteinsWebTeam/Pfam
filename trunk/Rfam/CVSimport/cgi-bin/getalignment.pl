#!/usr/local/bin/perl -- -*-perl-*- -T


#
# this file is copyright (c) GRL limited (1997). It can be distributed
# under the GNU Public License. See GNULICENSE for more details.
# Notice that this comes with NO WARRANTY of any implied use.
#


#
# Email ewan (birney@sanger.ac.uk) with moans and complaints.
#

use lib './';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';
use CGI;
use WWWAlign;
use RfamWWWConfig;
use Rfam::RfamAlign;

$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};


my ($query, $aln_type, $acc, $name, $ali, $file_num);

$query = new CGI;

$| = 1;
 

# Define fairly-constants

my $temp = $query->param('type');
$aln_type  = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp      = $query->param('acc');
$acc     = $1 if ($temp =~  /^([-\@~\w.]+)$/);

#my $temp     = $query->param('name');
#$name     = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp     = $query->param('file_num');
$file_num     = $1 if ($temp =~  /^([-\@~\w.]+)$/);

$file_num = 1 if (!$file_num);

my $en;

eval {

    $db = &RfamWWWConfig::get_database();

     $en = $db->get_Entry_by_acc( $acc );

    $name = $en->id();

};

if ($query->param('format') =~ /download/) {
  my $url = "$RfamWWWConfig::WWW_root/data/$aln_type/$acc.full.gz";
  print CGI->redirect("$url")
}


if ( ($en->num_seqs_in_full() > 10000) && ($query->param('format') !~ /link/) && ($aln_type =~ /full/) ) {
  print "Content-type: text/html\n\n";
  print &RfamWWWConfig::header( "$aln_type alignment for $name" , $name, $acc);
  print "<span class=normalmediumtext>This family contains too many members in the full alignment to display in the browser. <P>Instead you can download the full alignment from <A href=$RfamWWWConfig::WWW_root/data/full/$acc.full.gz>here</A><P>";
  print &RfamWWWConfig::footer();
  exit(0);
}

if (! defined( $query->param('format'))) {
    $query->param('format', 'mul');
}


&RfamWWWConfig::logs("ALIGNMENT:$acc:$aln_type:" . $query->param('format') );
#print "Content-type: text/html\n\n";
#print "$aln_type, $acc, $name, $ali, " . $query->param('format'). "  $file_num <BR>";
#print "NUM: $file_num \n";


if ($query->param('format') !~ /link/){
  $ali = &RfamWWWConfig::get_alignment($acc,$aln_type,1, 1);
  $ali->id($name);
 # print "no here : $ali<P>";

} else {
  $ali = Rfam::RfamAlign->new();
}
#print "ALI: $ali <BR>";
##cast up to a WWWAlign enabling the generate_html_alignment method

my $formatter = WWWAlign->new('-alignment' => $ali,
			      '-accession' => $acc,
			      '-aln_type' => $aln_type);
#print "GOT THIS <P>";
$formatter->generate_html_alignment( \*STDOUT, $query , "$aln_type alignment for $name", $acc, $file_num);



    








A


