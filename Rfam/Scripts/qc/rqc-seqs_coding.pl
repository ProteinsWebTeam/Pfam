#!/usr/bin/env perl

use strict;
use warnings;
use Cwd;
use File::Touch;

use Bio::Rfam::QC;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;

#Get the inital set up
my $config   = Bio::Rfam::Config->new;
my $familyIO = Bio::Rfam::FamilyIO->new;

if( $#ARGV == -1 ) {
  help();
}
my $family = shift;
my $pwd = getcwd;

#Check that $fam corresponds to a directory.
if(!-d $family){
  help();
}

if(-e "$family/nocoding"){
  unlink("$family/nocoding");
}
if(-e "$family/coding"){
  unlink("$family/coding");
}

my $error = 0;

my $familyObj;
if(!$error){
  eval{
    $familyObj = $familyIO->loadRfamFromLocalFile( $family, $pwd );
  };
  if($@){
    warn "There was an issues loading the entry from file: $@\n";
    exit(1);
  }
}
my $coding;
($error, $coding) = Bio::Rfam::QC::codingSeqs($familyObj, $config);

if($error){
  print STDERR $coding;
  open(C, '>', "$family/coding") or die "";
  print C $coding;
  close(C);
}else{
  touch("$family/nocoding");
}

#------------------------------------------------------------------------------
sub help {

  print<<EOF;

$0 <family> - finds any potneital coding regions in the SEED sequences.\n";

EOF
exit;
}