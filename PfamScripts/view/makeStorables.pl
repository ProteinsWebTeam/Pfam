#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Pfam::ViewProcess::Storable;

my $view = Bio::Pfam::ViewProcess::Storable->new;

if ( exists($view->options->{acc}) and defined($view->options->{acc})  ) {
  $view->logger->info("Calculating storables for ".$view->options->{acc});
  $view->updateSingleFamily;
}elsif(exists($view->options->{chunk}) and defined($view->options->{chunk}) ){
  $view->updateSeqRange;
}else{
  die "Incorrect options passed in, try running with --help"; 
}
