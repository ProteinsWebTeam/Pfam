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

my $temp     = $query->param('name');
$name     = $1 if ($temp =~  /^([-\@~\w.]+)$/);

my $temp     = $query->param('file_num');
$file_num     = $1 if ($temp =~  /^([-\@~\w.]+)$/);

$file_num = 1 if (!$file_num);

if (! defined( $query->param('format'))) {
    $query->param('format', 'mul');
}


#print "Content-type: text/html\n\n";
#print "NUM: $file_num \n";


if ($query->param('format') !~ /link/){
  $ali = &RfamWWWConfig::get_alignment($acc,$aln_type,1, 1);
  $ali->id($name);
 # print "no here : $ali<P>";

} else {
 # print "HERE <P>";
  $ali = Rfam::RfamAlign->new();
}

#print "HERE $name <P>";
##cast up to a WWWAlign enabling the generate_html_alignment method

my $formatter = WWWAlign->new('-alignment' => $ali,
			      '-accession' => $acc,
			      '-aln_type' => $aln_type);
#print "GOT THIS <P>";
$formatter->generate_html_alignment( \*STDOUT, $query , "$aln_type alignment for $name", $acc, $file_num);



    








A


