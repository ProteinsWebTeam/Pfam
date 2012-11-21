#!/usr/bin/env perl

use strict;
use warnings;
use Data::Printer;
use Bio::Pfam::ViewProcess::Architecture;
#-------------------------------------------------------------------------------

#Lets get a new Pfam view object
my $view = Bio::Pfam::ViewProcess->new;
my $archView = Bio::Pfam::ViewProcess::Architecture->new;
$archView->processOptions;
#-------------------------------------------------------------------------------
# Now update the VERSION table with the number of PfamA

my $noPfama = $view->pfamdb->getSchema->resultset('Pfama')->search({})->count;
my $version = $view->pfamdb->getSchema->resultset('Version')->find({});
$version->update({ number_families => $noPfama }) 
  if($version->number_families != $noPfama);

if(exists($archView->options->{acc}) and $archView->options->{acc}){
  #Do it for a single family
 p($archView->options);
}elsif(exists($archView->options->{ancillary}) and $archView->options->{ancillary}){
  #Do it for a set of families
}else{
  #Do it for the whole database
  $archView->logger->info("Calculating architectures for the whole database");
  #Start off with the architecture stuff.
  if(! $archView->statusCheck('doneArch')){
    #Clear out all of the old data.
    $archView->clearAllArchitecture;
    
    #Submit the architecture calulcations to the farm. This will not return until
    #all jobs have completed.
    $archView->submitToFarm(300);
    
    #Reset all of the counts in the architecture table and pfamA table
    $archView->updateAllArchitecture;
    $archView->touchStatus('doneArch');
  }
  
  #Update clan architectures
  if(! $archView->statusCheck('doneClanArch')){
    #Determine the list of clans affected by the updated families
    $archView->updateAllClanArchitectures;
    $archView->touchStatus('doneClanArch');
  }

  #Now make the storables.
  if(! $storableView->statusCheck('doneStorables')){
    #Submit the architecture calulcations to the farm. This will not return until
    #all jobs have completed.
    $storableView->submitToFarm(300);
    #Reset all of the counts in the architecture table and pfamA table
    $storableView->touchStatus('doneStorables');
  }
  
  #Now make the structure images
  
  #run scoop
  
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
