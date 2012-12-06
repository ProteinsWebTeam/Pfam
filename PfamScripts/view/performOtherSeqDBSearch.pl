#!/usr/bin/env perl
# Script that wraps around HHSEARCH and associated HHMAKE to allow profile-profile
# searches of the Pfam database.

use strict;
use warnings;

use Bio::Pfam::ViewProcess::Search;
my $view = Bio::Pfam::ViewProcess::Search->new;

#Potential inputs.
  # 1. All -all
  # 2. Accesssion -acc
  # 3. Range of accessions --chunk, --chunkSize

#Options to regenerate the tagert library
if($view->options->{help}){
  &help;
}
#We have been given an accession, get the HMM from th
if ( $view->options->{acc} ) {
  $view->logger->info("Preforming Search ".$view->options->{acc});
  $view->searchAll;
}
#We have been given a range, paging through families in the database
#searching each in turn.
elsif($view->options->{chunk} and $view->options->{chunkSize}){
  $view->searchRange;
}
#Perform a full database search
elsif($view->options->{all}){
  $view->submitToFarm(70);
}else{
  die "Incorrect options passed in, try running with --help"; 
}

sub help {
  print STDERR <<HELP;

$0 <options>


acc       : Use the SEED from this Pfam accession to search against the library. 

upload    : Upload the results in the database. Again, do not routinely use this
          : as this option is designed to run during the Ancillary view process.

all       : Submits the whole database to the farm for searching. This requires
          : a status directory to be provided. This allows for recovery etc.
statusdir : somewhere the progress of searches is stored. 

The above submits ranges of the database, using the following options. 

chunk     : Used in combination with chunksize, this says which page
          : of the the results should be worked on. Essentially a  LIMIT OFFSET,LENGTH
chunksize : The size of each chunk
h|help : prints this help message

Synopsis


HELP

exit;
}
