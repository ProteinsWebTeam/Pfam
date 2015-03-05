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
use Bio::Pfam::ViewProcess::Proteome;
use Bio::Pfam::ViewProcess::HHsearch;
use Log::Log4perl qw(:easy);

#-------------------------------------------------------------------------------
#get the logger

Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

#-------------------------------------------------------------------------------

#Lets get a new Pfam view object
$logger->debug("Initialising ViewProcess");
my $view = Bio::Pfam::ViewProcess->new;
$logger->debug("Initialising View/Architecture");
my $archView = Bio::Pfam::ViewProcess::Architecture->new;
$logger->debug("Initialising View/Storable");
my $storableView = Bio::Pfam::ViewProcess::Storable->new;
$logger->debug("Initialising View/PdbImage");
my $pdbImageView = Bio::Pfam::ViewProcess::PdbImage->new;
$logger->debug("Initialising View/Search");
my $searchView = Bio::Pfam::ViewProcess::Search->new;
$logger->debug("Initialising View/Scoop");
my $scoopView = Bio::Pfam::ViewProcess::Scoop->new;
$logger->debug("Initialising View/Proteome");
my $proteomeView = Bio::Pfam::ViewProcess::Proteome->new;
$logger->debug("Initialising View/HHsearch");
my $hhsearchView = Bio::Pfam::ViewProcess::HHsearch->new;

#-------------------------------------------------------------------------------
# Now update the VERSION table with the number of PfamA

$logger->debug("Updating version table");
my $noPfama = $view->pfamdb->getSchema->resultset('PfamA')->search({})->count;
my $version = $view->pfamdb->getSchema->resultset('Version')->find({});
$version->update({ number_families => $noPfama }) 
  if(!$version->number_families || ($version->number_families != $noPfama));

$logger->debug("Calculating architectures");
if(exists($archView->options->{acc}) and $archView->options->{acc}){
  #Do it for a single family
 p($archView->options);
}elsif(exists($archView->options->{ancillary}) and $archView->options->{ancillary}){
  #Do it for a set of families
}else{
  #Do it for the whole database

#-------------------------------------------------------------------------------
# Calculate architectures

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
    $logger->debug("Updating clan architectures"); 
    #Determine the list of clans affected by the updated families
    $archView->updateAllClanArchitectures;
    $archView->touchStatus('doneClanArch');
  }

  #Now make the storables.
  if(! $storableView->statusCheck('doneStorables')){
    $logger->debug("Making storables"); 
    #Submit the storables to the farm. This will not return until
    #all jobs have completed.
    $storableView->submitToFarm(300);
    $storableView->touchStatus('doneStorables');
  }
  
  #Now make the structure images
  if(! $pdbImageView->statusCheck('donePdbImages')){
    $logger->debug("Making structure images"); 
#Submit the pdb image creation to the farm. This will not return until
    #all jobs have completed.
    $pdbImageView->submitToFarm(150);
    $pdbImageView->touchStatus('donePdbImages');
  }
  
  #Now make run the models against the 'other' sequence databases
  if(! $searchView->statusCheck('doneOtherSearches')){
    $logger->debug("Running other searches"); 
    #Submit the database searches to the farm. This will not return until
    #all jobs have completed.
    $searchView->submitToFarm(75);
    $searchView->touchStatus('doneOtherSearches');
  }
} #end of if exists $archView
 
 
 
#All of these steps are affected whether one family is changed or all as they
#involve all by all comparisons. 
#run scoop
  if(! $scoopView->statusCheck('doneScoop')){
    #Grab the regions from the database and perform the SCOOP analysis
    $logger->debug("Performing SCOOP analysis"); 
    $scoopView->runScoop;
    $scoopView->touchStatus('doneScoop')
  }else{
    $scoopView->logger->info('Done Scoop');
  }

  #run HHsearch
  if(! $hhsearchView->statusCheck('doneHHsearch')){
    $logger->debug("Running HHsearch"); 
    $hhsearchView->options->{newlib} = 1;
    $hhsearchView->makeHHLib;
    $hhsearchView->upload(1);
    $hhsearchView->submitToFarm(100);
    $hhsearchView->touchStatus('doneHHsearch')
  }

  
#Proteome data.....
  if(! $proteomeView->statusCheck('doneProteome')){
    $logger->debug("Updating proteome data"); 
    my $protDbh = $proteomeView->pfamdb->getSchema->storage->dbh;
    
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
   
    #not all complete_proteomes are represented in proteome_regions
    #select all auto_proteomes in proteome_regions and put in a hash
    #select count number of regions and add to hash 
    #select count distinct pfamseq and add to hash
    #total_seqs_covered and num_proteins are the same - and were in pfam27 - so no need for 2 queries ###there is an error in pfam27 - what exactly should be in each column?
    my %complete_proteomes_data;
    
    my $st_a = $protDbh->prepare("select distinct auto_proteome from proteome_regions") or die "Cannot prepare statement $!\n";
    my $st_reg = $protDbh->prepare("select sum(count) from proteome_regions where auto_proteome = ?") or die "Cannot prepare statement $!\n";
    my $st_prot = $protDbh->prepare("select count(distinct pfamseq_acc) from proteome_regions where auto_proteome = ?") or die "Cannot prepare statement $!\n";

    $st_a->execute() or die "Cannot execute statement $!\n";
    my $arrayref_a = $st_a->fetchall_arrayref;
    foreach my $row_a (@$arrayref_a){
        $complete_proteomes_data{$row_a->[0]}=1;
    }

   foreach my $autoprot (keys %complete_proteomes_data){
        $st_reg->execute($autoprot) or die "Can't execute statement for $autoprot $!\n"; 
        $st_prot->execute($autoprot) or die "Can't execute statement for $autoprot $!\n"; 
        my $arrayref_r = $st_reg->fetchall_arrayref;
        my $arrayref_p = $st_prot->fetchall_arrayref;
        $complete_proteomes_data{$autoprot}={
            'num_total_regions' => $arrayref_r->[0]->[0],
            'num_proteins' => $arrayref_p->[0]->[0]
            }
   }
p(%complete_proteomes_data);
 
#   exit; 
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
 
    #no need to do this as grouping calculated at sequence update time   
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
    $logger->debug("Creating proteome TSV files"); 
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

