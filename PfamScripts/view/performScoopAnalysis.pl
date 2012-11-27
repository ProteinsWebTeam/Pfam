#!/usr/bin/env perl

use strict;
use warnings;
use Data::Printer;
use Bio::Pfam::ViewProcess;
use Bio::Pfam::ViewProcess::Scoop;

#-------------------------------------------------------------------------------

#Lets get a new Pfam view object
my $view = Bio::Pfam::ViewProcess->new;
my $storableView = Bio::Pfam::ViewProcess::Storable->new;

#run scoop
if(! $view->statusCheck('doneScoop')){
  my $scoopView = Bio::PfamViewProcess::Scoop->new;
  #Grab the regions from the database and perform the SCOOP analysis
  $scoopView->runScoop;
  $scoopView->touchStatus('doneScoop')
}