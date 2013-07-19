#!/usr/bin/env perl


use strict;
use warnings;
use File::Touch;

use Bio::Rfam::QC;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;

my $config = Bio::Rfam::Config->new;
my $dictionary = $config->dictionary;

if( $#ARGV == -1 ) {
  help();
}

my $familyIO = Bio::Rfam::FamilyIO->new;

foreach my $fam (@ARGV){
  help() if(!-d $fam);
  if(!-e "$fam/DESC"){
    die "$fam DESC file does not exist.\n";
  }
  
  #Now actually do the spelling check.
  my $error = Bio::Rfam::QC::checkSpell($fam, $dictionary, $familyIO);
  
  if($error){
    warn "Format error with spell QC.\n";
    exit(1);
  }else{
    touch("$fam/spell");
  }
}

sub help {
  print "$0. Checks spelling of free text in DESC files.\nUsage $0 <rfam-dir>\n";
  exit;
}