#!/usr/local/bin/perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::SVN::Client;

my $client = Bio::Pfam::SVN::Client->new;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $dir = shift;
unless(-d $dir){
  $logger->logdie("Could not find the families directory");  
}

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
$logger->debug("Got pfamlive database connection");

my $famRefs = $pfamDB->getAllPfamFamilyData;

foreach my $fam (@$famRefs){
  unless( -d $dir."/".$fam->pfama_acc){
    $logger->debug("Going to check out family ".$fam->pfama_acc." [".$fam->pfama_id."]"); 
    my $dest = $dir."/".$fam->pfama_acc;
    $client->checkoutFamily($fam->pfama_acc, $dest); 
  }   
}
$logger->debug($dir." now has a copy of all families");