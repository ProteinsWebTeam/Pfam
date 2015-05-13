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
use Cwd;

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



if(! $archView->statusCheck('doneVersion')){
    $logger->debug("Updating version table");
    my $noPfama = $view->pfamdb->getSchema->resultset('PfamA')->search({})->count;
    my $version = $view->pfamdb->getSchema->resultset('Version')->find({});
    $version->update({ number_families => $noPfama }) 
    if(!$version->number_families || ($version->number_families != $noPfama));
    $archView->touchStatus('doneVersion');

  }

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

      $archView->clearAllArchitecture;

      #WORK IN REWRITTEN ARCH STUFF HERE
      system("make_Architecture_new_part1.pl") and die $logger->logdie("Can't run make_Architecture_new_part1.pl");
        
#have jobs finished?
        if (! $archView->statusCheck('doneArchJobs')){
	        $logger->info("Already checked architectures farm jobs have finished\n");
        } else {
	        my $fin = 0;
    	    while (!$fin){
	            open( FH, "bjobs -Jarch|" );
	            my $jobs;
	            while (<FH>){
		            if (/^\d+/){
		                $jobs++;
		            }
	            }
	            close FH;
	            if ($jobs){
		            $logger->info("Architectures farm jobs still running - checking again in 10 minutes\n");
		            sleep(600);
	            } else {
		            $fin = 1;
                    $archView->touchStatus('doneArchJobs');
	            }
	        }
        }
        system("make_Architecture_new_part3.pl") and die $logger->logdie("Can't run make_Architecture_new_part3.pl");

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
    $storableView->submitToFarm;
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
      my $st_pa1 = $protDbh->prepare("SELECT p.auto_proteome, s.auto_architecture, s.pfamseq_acc, count(s.pfamseq_acc) FROM proteome_pfamseq p, pfamseq s WHERE s.pfamseq_acc=p.pfamseq_acc GROUP BY auto_architecture, auto_proteome") or $logger->logdie("Cannot prepare statement");
      my $st_pa2 = $protDbh->prepare("INSERT INTO proteome_architecture (auto_proteome, auto_architecture, type_example, no_seqs) VALUES (?, ?, ?, ?)") or $logger->logdie("Cannot prepare statement");
      $st_pa1->execute;
      my $arrayref_pa = $st_pa1->fetchall_arrayref();
      foreach my $row_pa (@$arrayref_pa){
          my $auto_prot = $row_pa->[0];
          my $auto_arch = $row_pa->[1];
          my $acc = $row_pa->[2];
          my $count = $row_pa->[3];
          if ($auto_arch){
            $st_pa2->execute($auto_prot, $auto_arch, $acc, $count)or $logger->logdie("Cannot insert into proteome_architecture"); 
          }
      }

      $protDbh->do("set FOREIGN_KEY_CHECKS=1");
      $proteomeView->touchStatus('updateProteomeArch');
    }

    #not all complete_proteomes are represented in proteome_regions
    #select all auto_proteomes in proteome_regions
    #select count number of regions
    #select count distinct pfamseq 
    my %complete_proteomes_data;
    
    my $st_a = $protDbh->prepare("select distinct auto_proteome from proteome_regions") or die "Cannot prepare statement $!\n";
    #total regions
    my $st_reg = $protDbh->prepare("select sum(count) from proteome_regions where auto_proteome = ?") or die "Cannot prepare statement $!\n";
    #number proteins (will be equal to total_genome_proteins)
    my $st_prot = $protDbh->prepare("select count(*) from pfamseq p, complete_proteomes c where c.ncbi_taxid = p.ncbi_taxid and auto_proteome = ?") or die "Cannot prepare statement $!\n";
    #total_seqs_covered - ie disinct proteins that have a region
    my $st_seqs_covered = $protDbh->prepare("select count(distinct pfamseq_acc) from proteome_regions where auto_proteome = ?") or die "Cannot prepare statement $!\n";
    #amino acids covered
    my $st_aa_covered = $protDbh->prepare("SELECT sum(seq_end - seq_start + 1) FROM proteome_pfamseq p, pfamA_reg_full_significant s WHERE s.pfamseq_acc=p.pfamseq_acc and in_full=1 and p.auto_proteome = ?") or die "Cannot prepare statement $!\n";
    #total amino acids
    my $st_aa_tot = $protDbh->prepare("select total_aa_length from complete_proteomes where auto_proteome =?") or die "Cannot prepare statement $!\n";

    $st_a->execute() or die "Cannot execute statement $!\n";
    my $arrayref_a = $st_a->fetchall_arrayref;
    foreach my $row_a (@$arrayref_a){
        $complete_proteomes_data{$row_a->[0]}=1;
    }

   foreach my $autoprot (keys %complete_proteomes_data){
        $st_reg->execute($autoprot) or die "Can't execute statement for $autoprot $!\n"; 
        $st_prot->execute($autoprot) or die "Can't execute statement for $autoprot $!\n"; 
        $st_seqs_covered->execute($autoprot) or die "Can't execute statement for $autoprot $!\n";
        $st_aa_covered->execute($autoprot) or die "Can't execute statement for $autoprot $!\n";
        $st_aa_tot->execute($autoprot) or die "Can't execute statement for $autoprot $!\n"; 
        my $arrayref_r = $st_reg->fetchall_arrayref;
        my $arrayref_p = $st_prot->fetchall_arrayref;
        my $arrayref_s = $st_seqs_covered->fetchall_arrayref;
        my $arrayref_a = $st_aa_covered->fetchall_arrayref;
        my $arrayref_t = $st_aa_tot->fetchall_arrayref;
        my $seq_coverage = 0;
        if ($arrayref_p->[0]->[0] > 0 && $arrayref_s->[0]->[0]){
            $seq_coverage = 100*($arrayref_s->[0]->[0]/$arrayref_p->[0]->[0]);
        }
        my $aa_coverage = 0;
        if ($arrayref_t->[0]->[0] > 0 && $arrayref_a->[0]->[0]){
            $aa_coverage = 100*($arrayref_a->[0]->[0]/$arrayref_t->[0]->[0]);  
        }

        $proteomeView->pfamdb->getSchema->resultset('CompleteProteome')->update_or_create(
            {
                auto_proteome => $autoprot,
                num_total_regions => $arrayref_r->[0]->[0],
                num_proteins => $arrayref_p->[0]->[0],
                total_seqs_covered => $arrayref_s->[0]->[0],
                sequence_coverage => $seq_coverage,
                total_aa_covered => $arrayref_a->[0]->[0],
                residue_coverage => $aa_coverage
            }
        );
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

#update Treefam mapping in pfamseq
if(! $view->statusCheck('doneTreefamUpdate')){
    $logger->debug("Updating treeFam mappings in pfamseq");

    my $dbh = $view->pfamdb->getSchema->storage->dbh;

    $logger->debug("Parsing Treefam file");
    my $treefam_mapping = '/nfs/public/rw/xfam/treefam/live/root/static/download/uniprotACC2treefam.txt';
    my @data = read_file($treefam_mapping);
    my %mapping;
    foreach my$line (@data){
        my @accns = split(/\s+/,$line);
        if ($accns[0] =~ /[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}/){
            $mapping{$accns[0]}=$accns[1];
        }
    }
    $logger->debug("Remving old TreeFam mappings from pfamseq");
    my $stt1 = $dbh->prepare("update pfamseq set treefam_acc = \'NULL\'") or die("Can't prepare statement: $dbh->errstr");
    $stt1->execute;

    $logger->debug("Updating pfamseq with new TreeFam mappings");
    my $stt2 = $dbh->prepare("update pfamseq set treefam_acc = ? where pfamseq_acc = ?") or die("Can't prepare statement: $dbh->errstr");

    foreach my $acc (keys %mapping){
        my $treefam = $mapping{$acc};
        $stt2->execute($treefam, $acc);
    }

    $view->touchStatus('doneTreefamUpdate');
}

#populate pfamA_ncbi table
#dump pfama_acc and pfamseq_acc from pfamA_reg_full_significant
my $dir = getcwd;
my $dumpfile = $dir . "/pfam_reg_dump";
unless ( -s $dumpfile){
    $logger->debug("Getting pfamseq accessions from database");
    my $pfamDB = $view->pfamdb;
    my $host = $pfamDB->{host};
    my $user = $pfamDB->{user};
    my $pass = $pfamDB->{password};
    my $port = $pfamDB->{port};
    my $db = $pfamDB->{database};
    my $cmd = "mysql -h $host -u $user -p$pass -P $port $db --quick -e \"select pfamA_acc, pfamseq_acc from pfamA_reg_full_significant where in_full = 1\" > $dumpfile";
    system($cmd) and $logger->logdie("Could not obtain pfamA_acc/pfamseq_acc from database");
}

#submit jobs to calculate and populate pfamA_ncbi
if (! $view->statusCheck('done pfamA_ncbi')){

    my $dbh = $view->pfamdb->getSchema->storage->dbh;

    $logger->debug("Deleting from pfamA_ncbi");
    my $stdel = $dbh->prepare("delete from pfamA_ncbi") or die "Can't prepare statement: $dbh->errstr";
    $stdel->execute();

    $logger->debug("Obtaining Pfam accessions");
    my %pfam_acc;
    my $st = $dbh->prepare("select pfamA_acc from pfamA") or die "Can't prepare statement: $dbh->errstr";
    $st->execute();
    my $arrayref = $st->fetchall_arrayref();
    foreach my $row (@$arrayref){
        $pfam_acc{$row->[0]}=1;
    }

    $logger->debug("Submitting pfamA_ncbi farm jobs");

    foreach my $acc (keys %pfam_acc){
    #bsub the next bit using job group pfamview

        my $queue = 'production-rh6';
        my $resource = "rusage[mem=1000]";
        my $memory = 1000;  
        my $fh = IO::File->new();
        my $group = '/Pfamview';
        my $cmd = "pfama_ncbi.pl -acc $acc -file $dumpfile";
        my $log = "$acc" . "pfama_ncbilog";

        $fh->open( "| bsub -q $queue -M $memory -R $resource -g $group -o $log -Jpfamancbi");
        $fh->print($cmd . "\n");
        $fh->close;
    }
    
        #have jobs finished?
    if (-e "finished_pfama_ncbi"){
	    $logger->info("Already checked pfamA_ncbi jobs have finished\n");
    } else {
	    my $fin = 0;
	    while (!$fin){
	        open( FH, "bjobs -Jpfamancbi|" );
	        my $jobs;
	        while (<FH>){
		        if (/^\d+/){
		            $jobs++;
		        }
	        }
	        close FH;
	        if ($jobs){
		        $logger->info("pfamA_ncbi jobs still running - checking again in 10 minutes\n");
		        sleep(600);
	        } else {
		        $fin = 1;
		        open( FH, "> finished_pfama_ncbi" ) or die "Can not write to file finished_pfama_ncbi";
		        close(FH);
	        }
	    }
    }

    $view->touchStatus('done pfamA_ncbi');
}



#-------------------------------------------------------------------------------
#Find when this job was last run



#-------------------------------------------------------------------------------
#Select all the families that have a changed date more recent that when the last jobs was started



#Now get a list of all sequences belongs to these families 

