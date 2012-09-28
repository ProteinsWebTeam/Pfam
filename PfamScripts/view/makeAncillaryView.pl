#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Pfam::ViewProcess;

-------------------------------------------------------------------------------

#Lets get a new Pfam view object

my $view = Bio::Pfam::ViewProcess->new;

$view->logger->debug("Got pfamlive database connection");

#-------------------------------------------------------------------------------
# Now update the VERSION table with the number of PfamA

my $noPfama = $view->pfamdb->getSchema->resultset('Pfama')->search({})->count;
my $version = $view->pfamdb->getSchema->resultset('Version')->find({});
$version->update({ number_families => $noPfama }) 
  if($version->number_families != $noPfama);

if($acc){
  #Do it for a single family
}elsif($ancillary){
#Do it for a set of families
}else{
  #Do it for the whole database
  
}


#-------------------------------------------------------------------------------
#Find when this job was last run



#-------------------------------------------------------------------------------
#Select all the families that have a changed date more recent that when the last jobs was started



#Now get a list of all sequences belongs to these families 

#Generate the storables for each sequence

#Look to see if the architecture is known for this sequence.  If not add it to the architecture table.

#Foreach family update the number of architectures

#Foreach clan update the number of architectures

# Now add in Kristofers bit to add the domain order

#-------------------------------------------------------------------------------
#Now on to the profile:profile comparisons
#
# Run hhsearch.

# 
