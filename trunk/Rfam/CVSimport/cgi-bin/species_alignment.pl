#!/usr/local/bin/perl  -T


#
# this file is copyright (c) GRL limited (1997). It can be distributed
# under the GNU Public License. See GNULICENSE for more details.
# Notice that this comes with NO WARRANTY of any implied use.
#


#
# Email ewan (birney@sanger.ac.uk) with moans and complaints.
#

use lib '/nfs/WWW/SANGER_docs/perl/bioperl-1.2/';


use lib './';

use CGI;
#use WWWAlign;
use RfamWWWConfig;
use SwissCol;

use Tree;
use GetzTax;

$ENV{'PATH'} = '/bin:/usr/bin';
my $path = $ENV{'PATH'};


my ($query, $acc, $family, $count);

$query = new CGI;

$| = 1;

print $query->header;

# Define fairly-constants

my $temp = $query->param('count');
$count  = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp      = $query->param('acc');
$acc     = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp     = $query->param('family');
$family     = $1 if ($temp =~  /^([-\@~\w.]+)$/);



print &RfamWWWConfig::header("Species alignment" , $acc, $acc);

#print "COUNT: $count, ACC: $acc , FAMILY: $family <P>";
#print "EEP <P>";
my $tmp = 0;
my @params;
while ($tmp <= $count) {


  my $temp = $query->param('species_' . $tmp);
  if ($temp) {
    my $var = $1 if ($temp =~  /^([-\@~\%\w.]+)$/);
    #   print "TEMP: $temp ::: var: $var <BR>";
    push @params, $var;
    # $params{$tmp} = $var if ($var);
  }
  $tmp++;

}


#print "ACC: $acc :: familt: $family  <P><P>";


##### Borrowed from Lorenzo's species code.

my (@list, @ids, $tree);
GetzTax::_file_domain_species($acc, \@list);
#print "LIST: @list <P>";
#if (!@list) {
#  open (FH, "$PfamWWWConfig::getz -t \"[$PfamWWWConfig::srs_pfam-id:$family]>$PfamWWWConfig::srs_db\" |" );
#  GetzTax::get( \*FH, $family, \@list );
#}


#my %pfamseq_ids;
my $seq_count;
foreach my $taxon (@params) {
 # print "TAXON:: $taxon <P>";
  my @ids;
  my $tree = Tree -> new();
  $tree -> grow_tree( \@list, ';' );
  $taxon =~ s/\%20/ /g;
  $tree -> get_ids( $taxon, \@ids );

  #print "THE IDS: @ids <P>";
  foreach my $seqid (@ids) {
    # print "id: $seqid <P>";
    $pfamseq_ids{$seqid} = $seqid;
    $seq_count++;
    #  $galign -> addSeq( Bio::Pfam::SeqPfam->new('-id' => $seqid ));
  }




}
#print "ACC: $acc <P>";
#foreach (sort keys %pfamseq_ids) {
#  print "KEY: $_ , VAL: " . $pfamseq_ids{$_}. " <BR>";
#}
my $file = &coloured_rfam($acc, \%pfamseq_ids);
#print "FILE: $file <P>";

open(_FILE, "gunzip -c $file |") or print "CANNA OPEN $file as $! <P>";
#print "<PRE>";
while(<_FILE>) {

  print "$_";

}
#print "</PRE>";
close(_FILE);


print &RfamWWWConfig::footer();
&RfamWWWConfig::logs("SPECIESALIGNMENT:$acc: ". $seq_count );


