#!/usr/bin/env perl
# Script that wraps around HHSEARCH and associated HHMAKE to allow profile-profile
# searches of the Pfam database.

use strict;
use warnings;

use Bio::Pfam::ViewProcess::HHsearch;

my $view = Bio::Pfam::ViewProcess::HHsearch->new;

#Potential inputs.
  # 1. SEED file, or any file -seed -file
  # 2. Accesssion -acc
  # 3. Range of accessions --chunk, --chunkSize

#Options to regenerate the tagert library
if($view->options->{help}){
  &help;
}

#Make the HMM library if we need to
$view->makeHHLib;
if($view->options->{onlylib}){
  $view->logger->info("Only building hhm-style HMM library.");
  exit;
}

#We have been given an accession, get the SEED file from the database, construct
#a hhm style HMM and search it against the library.
if ( $view->options->{acc} ) {
  $view->logger->info("Performing hhsearch ".$view->options->{acc});
  my $results = $view->makeHHMFromAccAndSearch($view->options->{acc});
  $view->logger->info("Your results are in $results");
}

#We have been given the flag -seed, use the SEED file in the current working
#directory, construct an hhm style HMM and search it against the library.
elsif($view->options->{seed}){
  $view->logger->info("Performing hhsearch SEED file");
  
  my $name;
  if($view->options->{name}){
    $name = $view->options->{name};
  }else{
    $name = 'SEED'
  }
  my $results = $view->makeHHMFromFileAndSearch('SEED', $name);
  $view->logger->info("Your results are in $results");
}
#We have been given a file, use that to construct an hhm style HMM and 
#search it against the library.

elsif($view->options->{file}){
  my $file =  $view->options->{file};
  $view->logger->info("Performing hhsearch $file file");
  
  my $name;
  if($view->options->{name}){
    $name = $view->options->{name};
  }else{
    $name = 'Query'
  }
  my $results = $view->makeHHMFromFileAndSearch($file, $name);
  $view->logger->info("Your results are in $results");
}
#We have been given a range, paging through families in the database
#searching each in turn.
elsif($view->options->{chunk} and $view->options->{chunkSize}){
  $view->searchRange;
}

#Perform a full database search
elsif($view->options->{allVsall}){
  $view->submitToFarm(100);
}else{
  die "Incorrect options passed in, try running with --help"; 
}

sub help {
  print STDERR <<HELP;

$0 <options>


acc       : Use the SEED from this Pfam accession to search against the library. 
seed      : If you are working on a family, it will use the seed alignmnet in the
          : current working directory.
file      : Specifiy an alignment to search against the hmm library.
name      : The label to be give to your hhm. If an accession if supplied, the name will be ignored

newlib    : Generate a new HHlib for all families in the database. Be sure you
          : know what you are doing before running this. 
onlylib   : Will only construct a new library and exit, regardless of anything
          : else. Useful for generating a database via a cronjob.

upload    : Upload the results in the database. Again, do not routinely use this
          : as this option is designed to be run during the Ancillary view process.
evalue    : The evalue threshold at which hits are deemed significant - default is
          : 0.01

allVsall  : Submits the whole database to the farm for searching. This requires
          : a status directory to be provided. This allows for recovery etc.
statusdir : somewhere the progress of searches is stored. 

The above submits ranges of the database, using the following options. 

chunk     : Used in combination with chunksize, this says which page
          : of the the results should be worked on. Essentially a  LIMIT OFFSET,LENGTH
chunksize : The size of each chunk

h|help : prints this help message

Synopsis

Takes one or more seed alignment and searches each against the collection of Pfam
HMMs using hhsearch. Note, we have to use alignments as it appears that hhsearch
has some serious issues estbliashing E-values for HMMER3 formated HMMs. Also, this
should give you the best sensitivity as it is using native formats. The hhm-style
HMMs are constrcutruted using a method that keeps them as close to HMMER3 as possible.

This will generate a results file for you to inspect. 

Example:

1. You are building a new family and want to search it

$0 --seed --name 'CoolDom'

2. You want to search a family in the database

$0 --acc PF00001

3. You have a MSA and want to see if it is related to anything in database

$0 --file 'userAlignment' 

4. You want to generate a new hhm-style library

$0 --onlylib

5. You want to generate a new hhm-style libary and search the database. Note this
   will take some time and generate the library first. 

$0 --newlib --acc PF00001

6. You want to perform an all against all search. This script will only finish/exit
when all jobs have completed. If you find that there are no jobs running, then
look at the logs in the statusdir. Restarting the script with the same parameters,
should allow it to recover from the point in time. The results will be stored in
the statusdir, with a results file for each chunk.

$0 --allVsall --statusdir /tmp

HELP

exit;
}
