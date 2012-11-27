#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Pfam::ViewProcess::PdbImage;

my $view = Bio::Pfam::ViewProcess::PdbImage->new;

if($view->options->{help}){
  &help;
}

if ( $view->options->{acc} ) {
  $view->logger->info("Calculating pdb images for ".$view->options->{acc});
  $view->updateSinglePdb;
}elsif($view->options->{chunk} and $view->options->{chunkSize}){
  $view->updatePdbRange;
}else{
  die "Incorrect options passed in, try running with --help"; 
}


sub help {
  print STDERR <<HELP;

$0 <options>

chunk     : Used in combination with chunksize, this says which page, or multiple
          : of the
chunksize :
acc       : A Pfam accession. This code will also make structure images for a
          : single family.

Notes - There is a blacklist of pdb files encoded into this script. Some NMR structures
and viruses cause massive memory inflations.

This was how this script was run on the Sanger farm.
bsub -q normal  -R"select[mem>8000] rusage[mem=8000]" -M 8000000 -J "pdb[1-30]" 
-o "pdb.\%J.\%I.log" 'makePdbImages.pl -chunkSize 2477 -chunk \$\{LSB_JOBINDEX\}'

HELP

exit;
}
