#! /usr/local/bin/perl -T


use lib './';
use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2';

use CGI;
use RfamWWWConfig;
use Tree;
use GetzTax;
use strict;
$| = 1;

$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};

my ( @list, $family, $depth, $tag, $notag, $prog_depth, $tree, $output );

my $query = new CGI;

print $query->header;

my @cats = qw (Eukaryota Bacteria Archaea);
my(%cats_pngs);
my $selected_cat;
foreach (@cats) {
  my $temp = $query->param($_ . ".x");
  my $blee = $1 if ($temp =~  /^([-\@~:\w.]+)$/);

  my $temp_cat_png = $query->param($_);
  my $cat_png = $1 if ($temp_cat_png =~  /^([-\@~:\w.]+)$/);

 # print "BLEE: $blee CAT: $_  & cat_png: $cat_png <P>";
  if($blee) {
    if ($cat_png =~ /minus/) {

    } else {
    
      $selected_cat .= " " . $_;
    }

  } else {

  
    $cats_pngs{$_} = $cat_png;
    $selected_cat .= " ". $_ if ($cat_png =~ /minus/);

  }
 # print "SELECTED: $selected_cat <P>";
}


print &RfamWWWConfig::header("Genome distribution");
#print "cats: $selected_cat<P>" if ($selected_cat);

#foreach (sort keys %cats_pngs) {
#  print "key: $_ , val: " .$cats_pngs{$_} . "<BR>";

#}
print "<form method=post enctype=\"multipart/form-data\"  action=/cgi-bin/Rfam/genome_dist.pl>";

foreach my $cat (@cats) {

  my $png = "plus.png";
  my(%cat_results);
  if ($selected_cat =~ $cat) {
    $png = "minus.png";
    (%cat_results) =   &RfamWWWConfig::genome_category($cat);
  } 
  print "<input type=hidden name=$cat value=$png>";
  print "<input type=image SRC=$RfamWWWConfig::image_link/$png name=\"$cat\" value=$cat border=0> <span class=normallargetext>$cat </span><P>";
  _print_cat_results(%cat_results) if ($selected_cat =~ $cat);
 

}
print "</form>";

print &RfamWWWConfig::footer;


sub _print_cat_results {
  my (%cat_res) = @_;
  
  my(%all_species);
  print "<table border=0><tr><Td><img src=\"/icons/blank.gif\" width=12></TD><TD><table border=1 cellpadding=5 cellspacing=0><TR><TD valign=TOP align=left BGCOLOR=\"#000070\" CLASS=whitetableheader >Species</TD><TD valign=TOP align=right BGCOLOR=\"#000070\" CLASS=whitetableheader  >Number of distinct families</TD><TD  valign=TOP align=right BGCOLOR=\"#000070\" CLASS=whitetableheader >No. of regions</TD></TR>";
  
  foreach (sort keys %cat_res) {
    my ($the_species, $junk) = split(/~/, $_);
    
    my($description, $taxonomy, $auto_genome, $genome_acc) = split(/~/, $_);
    my($species) = $1 if ($description =~ /^(\S+\s+\S+)/);
    my (@values) = @{$cat_res{$_}};
    
    my ($count, $sum);
    foreach (@values) {
      my ($rfam_acc, $rfam_id, $temp_sum) = split(/~/, $_);
      $sum  = $sum + $temp_sum;
      $count++;
      
    }
    if ($count > 0) {
      print "<TR><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=left ><A href=genome_view.pl?genome_acc=" . $genome_acc .">$the_species</A></TD>";
      print "<TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>$count</TD><TD class=normalmediumtext BGCOLOR=$RfamWWWConfig::rfamcolour  NOWRAP valign=center align=right>$sum</TD></TR>";
    }
    
  }
  
  
  
  print "</TABLE></TD></TR></TABLE><P>";

}


