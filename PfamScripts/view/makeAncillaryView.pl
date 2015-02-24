#!/usr/bin/env perl

use strict;
use warnings;
use Data::Printer;
use Bio::Pfam::ViewProcess;
use Bio::Pfam::ViewProcess::Architecture;
use Bio::Pfam::ViewProcess::Storable;
use Bio::Pfam::ViewProcess::Scoop;
use Bio::Pfam::ViewProcess::PdbImage;
use Bio::Pfam::ViewProcess::Search;

#-------------------------------------------------------------------------------

#Lets get a new Pfam view object
my $view = Bio::Pfam::ViewProcess->new;
my $archView = Bio::Pfam::ViewProcess::Architecture->new;
my $storableView = Bio::Pfam::ViewProcess::Storable->new;
my $pdbImageView = Bio::Pfam::PdbImage->new;
my $searchView = Bio::Pfam::Search->new;
my $scoopView = Bio::Pfam::ViewProcess::Scoop->new;
my $proteomeView = Bio::Pfam::ViewProcess::Proteome->new;
my $hhsearchView = Bio::Pfam::ViewProcess::HHsearch->new;

#-------------------------------------------------------------------------------
# Now update the VERSION table with the number of PfamA

my $noPfama = $view->pfamdb->getSchema->resultset('PfamA')->search({})->count;
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
  if(! $pdbImageView->statusCheck('donePdbImages')){
    #Submit the architecture calulcations to the farm. This will not return until
    #all jobs have completed.
    $pdbImageView->submitToFarm(150);
    #Reset all of the counts in the architecture table and pfamA table
    $pdbImageView->touchStatus('donePdbImages');
  }
  
  #Now make run the models against the 'other' sequence databases
  if(! $searchView->statusCheck('doneOtherSearches')){
    #Submit the architecture calulcations to the farm. This will not return until
    #all jobs have completed.
    $searchView->submitToFarm(75);
    #Reset all of the counts in the architecture table and pfamA table
    $searchView->touchStatus('doneOtherSearches');
  }
}
 
 
 
#All of these steps are affected whether one family is changed or all as they
#involve all by all comparisons. 
#run scoop
  if(! $scoopView->statusCheck('doneScoop')){
    exit;
    #Grab the regions from the database and perform the SCOOP analysis
    $scoopView->runScoop;
    $scoopView->touchStatus('doneScoop')
  }else{
    $scoopView->logger->info('Done Scoop');
  }

  #run HHsearch
  if(! $hhsearchView->statusCheck('doneHHsearch')){
    $hhsearchView->options->{newlib} = 1;
    $hhsearchView->makeHHLib;
    $hhsearchView->upload(1);
    $hhsearchView->submitToFarm(100);
    $hhsearchView->touchStatus('doneHHsearch')
  }

  #Proteome data.....
  if(! $proteomeView->statusCheck('doneProteome')){
    my $protDbh = $proteomeView->pfamdb->getSchema->storage;
    if(! $proteomeView->statusCheck('updateProteomeArch')){
      $protDbh->do("set FOREIGN_KEY_CHECKS=0");
    
      #Update the proteome_architecture, then update the stats
      $protDbh->do("DELETE FROM proteome_architecture");
      $protDbh->do("INSERT INTO proteome_architecture ".
                      "SELECT p.auto_proteome, s.auto_architecture, s.pfamseq_acc, count(s.pfamseq_acc) ".
                      "FROM proteome_pfamseq p, pfamseq s ".
                      "WHERE s.pfamseq_acc=p.pfamseq_acc ".
                      "GROUP BY auto_architecture, auto_proteome");
      
      $protDbh->do("set FOREIGN_KEY_CHECKS=1");
      $proteomeView->touchStatus('updateProteomeArch');
    }
    
    if(! $proteomeView->statusCheck('updateProteomeStats')){
      $protDbh->do("UPDATE complete_proteomes c ".
                   "SET num_total_regions = ( ".
                   "SELECT sum(count) FROM proteome_regions r ".
                   "WHERE r.auto_proteome=c.auto_proteome)");
      
      $protDbh->do("UPDATE complete_proteomes c ".
                    "SET num_proteins= ( SELECT count( distinct r.pfamseq_acc) ".
                    "FROM proteome_regions r ".
                    "WHERE r.auto_proteome=c.auto_proteome)") ;
                    
      $protDbh->do("UPDATE complete_proteomes c SET total_seqs_covered = ( ".
                    "SELECT count( distinct r.pfamseq_acc) FROM proteome_regions r ".
                    "WHERE r.auto_proteome=c.auto_proteome)") ;
      $protDbh->do("UPDATE complete_proteomes SET ".
                    "sequence_coverage = ((total_seqs_covered/total_genome_proteins)*100)");
      $protDbh->do("UPDATE complete_proteomes c ".
                    "SET total_aa_length=( ".
                    "SELECT sum(length) ".
                    "FROM proteome_pfamseq p , pfamseq s ".
                    "WHERE s.pfamseq_acc=p.pfamseq_acc ".
                    "AND p.auto_proteome=c.auto_proteome)");
                    
      $protDbh->do("UPDATE complete_proteomes c SET total_aa_covered=( ".
                    "SELECT sum(seq_end - seq_start + 1) ".
                    "FROM proteome_pfamseq p, pfamA_reg_full_significant s ".
                    "WHERE s.pfamseq_acc=p.pfamseq_acc ". 
                    "AND p.auto_proteome=c.auto_proteome and in_full=1)");
                    
      $protDbh->do("UPDATE complete_proteomes ".
                   "SET residue_coverage = ((total_aa_covered/total_aa_length)*100)");
      
      $protDbh->do("DELETE FROM complete_proteomes WHERE num_proteins=0");
      
      $proteomeView->touchStatus('updateProteomeStats');
    }
    
    if(! $proteomeView->statusCheck('updateProteomeGrp')){
      my @rs = $proteomeView->pfamdb
                              ->getSchema
                                ->resultset('Taxonomy')
                                  ->search({ rank => 'superkingdom' });
      my $sth = $protDbh->prepare("UPDATE complete_proteomes c, taxonomy t ".
                                    "SET grouping=? ".
                                    "WHERE c.ncbi_taxid=t.ncbi_taxid and t.lft> ? and t.rgt< ? ");
      foreach my $row (@rs){
        $sth->execute($row->level, $row->lft, $row->rgt);
      }
      my $rs = $proteomeView->pfamdb
                              ->getSchema
                                ->resultset('Taxonomy')
                                  ->search({ grouping => 'NULL' });
      if($rs->count > 0){
        $proteomeView->logger->logdie("Some complete_proteomes have grouping of null!");
      }
      $proteomeView->touchStatus('updateProteomeGrp');
    }
    $proteomeView->touchStatus('doneProteome');
  }
  
  if(! $proteomeView->statusCheck('doneProteomeTSV')){
    #Each proteome to be searched
    $proteomeView->submitToFarm(75);
    #
    $proteomeView->touchStatus('doneProteomeTSV');
  }
#}


#-------------------------------------------------------------------------------
#Find when this job was last run



#-------------------------------------------------------------------------------
#Select all the families that have a changed date more recent that when the last jobs was started



#Now get a list of all sequences belongs to these families 

