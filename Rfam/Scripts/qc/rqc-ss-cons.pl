#!/usr/bin/env perl
#
# Calculate statistics for how well the structure annotation corresponds 
# with the SEED sequences.
#
use strict;
use warnings;
use File::Touch;
use Cwd;

use Bio::Rfam::QC;
use Bio::Rfam::FamilyIO;

my $pwd = getcwd;

my $fam = '.';
if($ARGV[0]){
  $fam = shift @ARGV;
}

if(! -d $fam or !-e "$fam/SEED"){
  warn "$fam does not look like a Rfam family directory\n";
  help();
}

if(-e "$fam/ssstats"){
  unlink("$fam/ssstats");
}

my $io     = Bio::Rfam::FamilyIO->new;
my $familyObj = $io->loadRfamFromLocalFile( $fam, $pwd );
my $error = Bio::Rfam::QC::ssStats($familyObj, "$pwd/$fam");
if($error){
  warn "There is a problem with your SS stats. Please fix.\n";
  exit(1);
}else{
  touch("$fam/ssstats");
}

sub help {
  print STDERR <<EOF;

rqc-ss-con.pl - calculate statistics for how well the structure annotation 
                corresponds with the SEED sequences.

Usage:    rqc-ss-con.pl <directory>
EOF

exit;
}
