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
my $familyIO = Bio::Rfam::FamilyIO->new;
my $rfamdb = $config->rfamlive;


#Need to put a transaction around this block
my $guard = $rfamdb->txn_scope_guard;

  my $familyObj = $familyIO->loadRfamFromLocalFile($family, $dir, 'file');

  #Perform QC on the family
  #$self->_qualityControlEntry( $modelObj, $dir, $model, $dfamDB );
  my @pages = keys(%{$familyObj->DESC->WIKI});
  my $row = $rfamdb->resultset('Wikitext')->find_or_create({'title' => $pages[0]});
  my $entry = $rfamdb->resultset('Family')->find({rfam_acc => $familyObj->DESC->AC});
  if($entry){
    $rfamdb->resultset('Family')->updateFamilyFromObj($familyObj, $row->auto_wiki);
  }else{
    $rfamdb->resultset('Family')->createFamilyFromObj($familyObj, $row->auto_wiki);
  }
  $rfamdb->resultset('LiteratureReference')->find_or_createFromFamilyObj( $familyObj );
  $rfamdb->resultset('DatabaseLink')->find_or_createFromFamilyObj( $familyObj );  
  $rfamdb->resultset('SeedRegion')->updateSeedRegionsFromFamilyObj( $familyObj );
  $rfamdb->resultset('FullRegion')->updateFullRegionsFromFamilyObj( $familyObj );
  $rfamdb->resultset('FamilyFile')->uploadFilesFromFamilyObj( $familyObj );
  $rfamdb->resultset('PostProcess')->initiatePostProcessJob( $familyObj, 'rdf' );

$guard->commit;
