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

my $temp_search = $q->param('name');
my $search_value = $1 if ($temp_search =~  /^([\-\_\@\w\s+.]+)$/);
$search_value =~ s/(\w)\s+(\w)/$1&$2/g;
#my $temp =  $q->param('acc');
my @search_terms = qw (acc desc author type all medline title journal lit_author all_lit everything);
my (%search_values) = ('acc' => "rfam_id~rfam_acc",
		       'desc' => "description~comment",
		       'lit_author' => "literature_references.author"
		      );
		       
my (@terms_and_values);
foreach my $term(@search_terms) {
  #print "EEP: $term <BR>";
  if ($q->param($term)) {
    if (defined($search_values{$term})) {
   #   print "DEF: $term <P>";
      my $tmp = $search_values{$term};
      if ($tmp =~ /~/) {
	my($a, $b) = split(/~/, $tmp);
	push @terms_and_values, $a;
	push @terms_and_values, $b;
      } else {
	push @terms_and_values, $tmp;
      }
    } else {
      push @terms_and_values, $term;
    }

    #print "GOT IT <BR>";
  }
 #my $temp =  $q->param($term);
#  print "THE TEMP: $temp <BR>";
#  my $search = $1 if ($temp =~  /^([\-\_\@\w\s+.]+)$/);
#  print "TEMP: $search <P>";
#  if ($temp) {
#    $terms_and_values{$_} = $search;
#  }
}


print &RfamWWWConfig::header("Results for Rfam text Search (cont..)");
my $annseq = &RfamWWWConfig::get_sequence($search_value, "name");

if ($annseq) {
  print("<CENTER><SPAN class=normallargetext>EMBL entry for $search_value </span><P><TABLE BORDER=1 CELLPADDING=5  CELLSPACING=0><TR><TD BGCOLOR=#000070 CLASS=whitetableheader>Rfam family</TD><TD BGCOLOR=#000070 CLASS=whitetableheader>Description</TD><TD BGCOLOR=#000070 CLASS=whitetableheader >Start</TD><TD BGCOLOR=#000070 CLASS=whitetableheader>End</TD></TR>");

foreach my $region ($annseq->eachAnnotatedRegion()) {
  print "<TR><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour><A HREF=/cgi-bin/Rfam/getacc?" .$region->accession . ">" .$region->id . "</A></TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>" . $region->annotation() . "</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>" .$region->model_from . "</TD><TD NOWRAP valign=center align=left  BGCOLOR=$RfamWWWConfig::rfamcolour>" . $region->model_to. " </TD> </TR>";
#  print "REGION: $region " .$region->from . " to " .$region->to . " acc: " .$region->accession 
#. " id: " .$region->id . " desc:" .$region->annotation()->description() . " <P>";


  

}


print "</TABLE></CENTER><P><P>";


}



my (@db_results) = &RfamWWWConfig::new_rfam_search($search_value, @terms_and_values);
print "RES: @db_results <P>";
my @results;
foreach (@db_results) {
  my ($acc, $id, $desc) = split(/~/, $_);
  push @results, {'name' => $id};
  $results[$#results]->{'acc'} = $acc;
  $results[$#results]->{'des'} .= $desc;

}

if (@db_results) {

  print qq(<CENTER><span class=normallargetext>Results for query '<SPAN class=normalbluetext>$search_value</SPAN>'</SPAN><P>
	   <table border=1 cellpadding=5 cellspacing=0 align=center bgcolor=$RfamWWWConfig::rfamcolour><tr bgcolor=#000070  ><th class=whitetableheader>Family</th><th  class=whitetableheader>Description</th></tr>\n
	   );
   foreach my $res (@results) {
        if (not $res->{'des'}) {
            $res->{'des'} = $res->{'acc'};
        }
        print "<tr><td > <a href=\"$RfamWWWConfig::getacc?".$res->{'acc'}."\">".$res->{'name'}."</a></td><td class=normaltext>".$res->{'des'}."</td></tr>" ;
       # $p->break;
    }
    print "</table>\n<P>";

}


#print "RESULTS: @results <P>";
#print "ANNSEQ: $annseq <P>";
#print "SEARCH: $search_value <P>";
#print "VALS: @terms_and_values <P>";
#&RfamWWWConfig::array(%terms_and_values);

print &RfamWWWConfig::footer();
