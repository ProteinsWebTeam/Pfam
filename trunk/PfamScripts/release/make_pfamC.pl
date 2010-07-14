#!/usr/local/bin/perl

use strict;
use warnings;

use Bio::Pfam::Config;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamLiveDBManager;
use Log::Log4perl qw(:easy);

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $config = Bio::Pfam::Config->new;


#Can we connect to the PfamLive database?
$logger->info("Going to connect to the live database\n");

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );

open(P, ">Pfam-C");

my $clans = $pfamDB->getAllClanData;
foreach my $clan (sort{$a->clan_id cmp $b->clan_id} @$clans){
  #Check that family exists in svn
  my $client = Bio::Pfam::SVN::Client->new;
  $client->checkClanExists($clan->clan_acc);
  my $T;
  open($T, ">$$.tmp") or $logger->logdie("Could not open $$.tmp for writing");
  $client->catFile( $clan->clan_acc, "CLANDESC", $T );
  close($T);
  
  
  open(D, "$$.tmp")  or $logger->logdie("Could not open $$.tmp for writing");
  print P "# STOCKHOLM 1.0\n";
  
  while(<D>){
    if(/^AC   CL\d+/){
      print P "#=GF AC   ".$clan->clan_acc.".".$clan->version."\n";   
    }else{
      print P "#=GF $_";  
    }  
  }
  close(D);
  print P "//\n";  
}
close(P);

