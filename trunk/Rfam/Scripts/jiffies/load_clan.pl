#!/usr/bin/env perl

use strict;
use warnings;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Data::Printer;

my $dir    = shift;
my $family = shift;

print "$dir, $family\n";
unless(defined($family)){
  print STDERR "Please provide a family:\n";
}
if(!-d ($dir)){
  print STDERR "Please provide a path to a family directory, one was specified, but does not exist\n";
}

my $config = Bio::Rfam::Config->new;
my $clanIO = Bio::Rfam::ClanIO->new;
my $rfamdb = $config->rfamlive;


#Need to put a transaction around this block
my $guard = $rfamdb->txn_scope_guard;

  my $clanObj = $familyIO->loadClanFromLocalFile($family, $dir, 'file');

  #Perform QC on the family
  #$self->_qualityControlEntry( $modelObj, $dir, $model, $dfamDB );
  
  my $entry = $rfamdb->resultset('Clan')->find({clan_acc => $clanObj->DESC->AC});
  if($entry){
    $rfamdb->resultset('Clan')->updateClanFromObj($clanObj);
  }else{
    $rfamdb->resultset('Clan')->createClanFromObj($clanObj);
  }
  $rfamdb->resultset('LiteratureReference')->find_or_createFromClanObj( $clanObj );
  $rfamdb->resultset('ClanDatabaseLink')->find_or_createFromClanObj( $clanObj );

  #Now populate the 
  foreach my $member (@{$clanObj->DESC->MEMB}){
     $rfamdb->resultset('ClanMembership')->find_or_create({ rfam_acc => $member, clan_acc => $clanObk->DESC->AC});
  }
  die;
$guard->commit;
