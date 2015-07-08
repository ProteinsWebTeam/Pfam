#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use Getopt::Long;

use Bio::Pfam::PfamQC;
use Bio::Pfam::Config;

my ($help);

&GetOptions(
  "h" => \$help,
);

if($help or !defined($ARGV[0])){
  &help; 
} 

my $family = shift;
my $pwd = getcwd;

my $familyIO = Bio::Pfam::FamilyIO->new;
my $famObj = $familyIO->loadPfamAFromLocalFile( $family, $pwd );

my $config = Bio::Pfam::Config->new;
my $pfamDB;
if ( $config->location eq 'WTSI' or $config->location eq 'EBI' ) {
  my $connect = $config->pfamlive;
  $pfamDB  = Bio::Pfam::PfamLiveDBManager->new( %{$connect} );
}
unless ( Bio::Pfam::PfamQC::sequenceChecker( $family, $famObj, $pfamDB ) ) {
  print "$family contains sequence errors.  You should rebuild this family.\n";
  exit(1);
}


sub help {

print<<EOF;

usage: $0 <PFAM ACCESSION>

Checks all of the sequences in the SEED and ALIGN files are present in the current pfamseq.  

EOF
 

exit;
  
}
