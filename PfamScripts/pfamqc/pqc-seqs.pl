#!/usr/local/bin/perl

use strict;
use warnings;
use Cwd;
use Getopt::Long;

use Bio::Pfam::PfamQC;

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

unless ( Bio::Pfam::PfamQC::sequenceChecker( $family, $famObj ) ) {
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
