#!/usr/bin/env perl

use strict;
use warnings;
use Log::Log4perl qw(get_logger :levels);
use Data::Printer;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::ViewProcess::Architecture;

my $view = Bio::Pfam::ViewProcess::Architecture->new;
$view->processOptions;

if ( $view->options->{acc} ) {
  $view->logger->info("Calculating architectures for ".$view->options->{acc});
  $view->updateSingleFamily;
}elsif($view->options->{chunk} and $view->options->{chunkSize}){
  $view->updateSeqRange;
}else{
  die "Incorrect options passed in, try running with --help"; 
}
