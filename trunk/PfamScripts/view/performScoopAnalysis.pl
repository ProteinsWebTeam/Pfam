#!/usr/bin/env perl

use strict;
use warnings;
use Data::Printer;
use Bio::Pfam::ViewProcess::Scoop;

#-------------------------------------------------------------------------------

#Lets get a new Pfam view object
my $scoopView = Bio::Pfam::ViewProcess::Scoop->new;

#run scoop
if(! $scoopView->statusCheck('doneScoop')){
  #Grab the regions from the database and perform the SCOOP analysis
  $scoopView->runScoop;
  $scoopView->touchStatus('doneScoop')
}
