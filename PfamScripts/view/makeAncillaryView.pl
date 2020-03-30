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

    my $dbh = $archView->pfamdb->getSchema->storage->dbh;
    my $sth = $dbh->prepare("update version set number_families=$noPfama") or $logger->logdie("Cannot prepare statement, $!");
    $sth->execute() or $logger->logdie("Cannot update number_families in version table".$sth->errstr);
    $archView->touchStatus('doneVersion');

}
else {
  $logger->debug("Done version table update");
}

if(exists($archView->options->{acc}) and $archView->options->{acc}){
  #Do it for a single family
 p($archView->options);
}elsif(exists($archView->options->{ancillary}) and $archView->options->{ancillary}){
  #Do it for a set of families
}else{
  #Do it for the whole database

#-------------------------------------------------------------------------------
# Calculate architectures

  #Start off with the architecture stuff.
  if(! $archView->statusCheck('doneArch')){
      $archView->logger->info("Calculating architectures for the whole database");
      $archView->clearAllArchitecture;

      #WORK IN REWRITTEN ARCH STUFF HERE
      system("make_Architecture_new_part1.pl") and die $logger->logdie("Can't run make_Architecture_new_part1.pl");
        
        #have jobs finished?
        if ($archView->statusCheck('doneArchJobs')){
	        $logger->info("Already checked architectures farm jobs have finished\n");
        } else {
          sleep(600);
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
        my $touch_dir = $archView->options->{statusdir};
        my $queue = $view->{config}->{farm}->{lsf}->{queue};
        system("bsub -q $queue -R \"rusage[mem=20000]\" -M 20000 -o $touch_dir/arch3.log 'make_Architecture_new_part3.pl && touch $touch_dir/doneArch'") and die $logger->logdie("Can't run make_Architecture_new_part3.pl, $!");
        my $x=0;
        until($x==1) {
          sleep(600);
          if(-e "$touch_dir/doneArch") {
            $x=1;
          }
          else {
            $logger->info("Architecture3 farm job still running - checking again in 10 minutes\n");
          }
        }
  }
  else {
    $archView->logger->info("Done architectures");
  }

  #Update clan architectures
  if(! $archView->statusCheck('doneClanArch')){
    $logger->debug("Updating clan architectures"); 
    #Determine the list of clans affected by the updated families
    $archView->updateAllClanArchitectures;
    $archView->touchStatus('doneClanArch');
  }
  else {
    $logger->debug("Done clan architectures");
  }

  #$logger->logdie("done Architecture");
  
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
    $scoopView->submitToFarm;
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
  else {
    $hhsearchView->logger->info('Done HHsearch');
  }

#Proteome data.....
  if(! $proteomeView->statusCheck('doneProteome')){
    $logger->debug("Updating proteome data"); 
    my $protDbh = $proteomeView->pfamdb->getSchema->storage->dbh;
    
    if(! $proteomeView->statusCheck('updateProteomeArch')){
      $protDbh->do("set FOREIGN_KEY_CHECKS=0");
    
      #Update the proteome_architecture, then update the stats
      $protDbh->do("DELETE FROM proteome_architecture");
      my $st_pa = $protDbh->prepare("insert into proteome_architecture (type_example, ncbi_taxid, auto_architecture, no_seqs) select pfamseq_acc, ncbi_taxid, auto_architecture, count(auto_architecture) from pfamseq where auto_architecture>0 group by ncbi_taxid, auto_architecture") or $logger->logdie("Cannot prepare statement, $!");
        
      $st_pa->execute() or $logger->logdie("Cannot insert into proteome_architecture".$st_pa->errstr);

      $protDbh->do("set FOREIGN_KEY_CHECKS=1");
      $proteomeView->touchStatus('updateProteomeArch');
    }

    #Delete old data, if any
    $logger->debug("Deleting from complete_proteomes");
    my $st_proteome_delete = $protDbh->prepare("delete from complete_proteomes") or $logger->logdie("Can't prepare statement: ".$protDbh->errstr);
    $st_proteome_delete->execute() or $logger->logdie("Couldn't execute statement ".$st_proteome_delete->errstr);

    #Insert species, ncbi_taxid, num_proteins, total_aa_length from pfamseq
    $logger->debug("Inserting into complete_proteomes");
    my $st_proteome = $protDbh->prepare("insert into complete_proteomes (species, ncbi_taxid, num_proteins, total_genome_proteins, total_aa_length) select species, ncbi_taxid, count(pfamseq_acc), count(pfamseq_acc), sum(length) from pfamseq where ncbi_taxid !=0 group by ncbi_taxid");
    $st_proteome->execute or $logger->logdie("Couldn't execute statement ".$st_proteome->errstr);

    my $st_proteome_taxid=$protDbh->prepare("select ncbi_taxid from complete_proteomes");
    $st_proteome_taxid->execute() or $logger->logdie("Couldn't execute statement ".$st_proteome_taxid->errstr);

    my ($ncbi_taxid);
    $st_proteome_taxid->bind_columns(\$ncbi_taxid);

    #Set up queries to update other fields in complete_proteomes
    my $st_proteome_taxonomy = $protDbh->prepare("select taxonomy from pfamseq where ncbi_taxid=? limit 1");
    my $st_proteome_dom = $protDbh->prepare("select sum(number_domains) from proteome_regions where ncbi_taxid=?");
    my $st_proteome_seqs = $protDbh->prepare("select count(distinct r.pfamseq_acc) from pfamA_reg_full_significant r, pfamseq p where p.pfamseq_acc=r.pfamseq_acc and ncbi_taxid=?");
    my $st_proteome_res = $protDbh->prepare("select sum(seq_end - seq_start+1) from pfamA_reg_full_significant r, pfamseq s where s.pfamseq_acc=r.pfamseq_acc and in_full=1 and ncbi_taxid=?");

    while ($st_proteome_taxid->fetch()) {

      next unless($ncbi_taxid);
      $logger->debug("Updating ncbi_taxid $ncbi_taxid");;

      #Get grouping info from taxonomy string 
      $st_proteome_taxonomy->execute($ncbi_taxid) or $logger->logdie("Couldn't execute statement ".$st_proteome_taxonomy->errstr);
      my $taxonomy=$st_proteome_taxonomy->fetchrow;

      my $grouping;
      if($taxonomy =~ /^(\S+);\s/ or $taxonomy =~ /^(\S+)\./) {
        $grouping = $1;
      }
      else {
        $logger->logdie("Couldn't extract grouping from '$taxonomy'");
      }

      #Get number of domains
      $st_proteome_dom->execute($ncbi_taxid) or $logger->logdie("Couldn't execute statement ".$st_proteome_dom->errstr);
      my $num_total_regions=$st_proteome_dom->fetchrow;
      $num_total_regions=0 unless($num_total_regions);

      #Get total sequences covered
      $st_proteome_seqs->execute($ncbi_taxid) or $logger->logdie("Couldn't execute statement ".$st_proteome_seqs->errstr);
      my $total_seqs_covered=$st_proteome_seqs->fetchrow;
      $total_seqs_covered=0 unless($total_seqs_covered);

      #Get total residues covered
      $st_proteome_res->execute($ncbi_taxid) or  $logger->logdie("Couldn't execute statement ".$st_proteome_res->errstr);
      my $total_aa_covered=$st_proteome_res->fetchrow;
      $total_aa_covered=0 unless($total_aa_covered);

      #Upload to database
      $proteomeView->pfamdb->getSchema->resultset('CompleteProteome')->update_or_create(
        {
          ncbi_taxid => $ncbi_taxid,
          grouping => $grouping,
          num_total_regions => $num_total_regions,
          total_seqs_covered => $total_seqs_covered,
          total_aa_covered => $total_aa_covered
        }
      );
    }

    #Calculate sequence and residue coverage for each proteome
    $logger->debug("Updating coverage in complete_proteomes");
    my $st_proteome_seq_cov = $protDbh->prepare("update complete_proteomes set sequence_coverage = ( (total_seqs_covered/num_proteins)*100 )");
    $st_proteome_seq_cov->execute() or $logger->logdie("Couldn't execute statement ".$st_proteome_seq_cov->errstr);       

    my $st_proteome_res_cov = $protDbh->prepare("update complete_proteomes set residue_coverage = ( (total_aa_covered/ total_aa_length)*100 )");
    $st_proteome_res_cov->execute() or $logger->logdie("Couldn't execute statement ".$st_proteome_res_cov->errstr);       

    $proteomeView->touchStatus('doneProteome');
  }
  
  if(! $proteomeView->statusCheck('doneProteomeTSV')){
    $logger->debug("Creating proteome TSV files"); 
    #Each proteome to be searched
    $proteomeView->submitToFarm(75);
    #
    $proteomeView->touchStatus('doneProteomeTSV');
  }
  else {
    $logger->info('Done protome TSV files');
  }
#}


if(! $proteomeView->statusCheck('doneTreefamUpdate')){ #Use proteomeView object to access statusCheck subroutine
    $logger->debug("Updating TreeFam mappings in pfamseq and uniprot tables");

    my $dbh = $view->pfamdb->getSchema->storage->dbh;

    $logger->debug("Parsing TreeFam file");
    my $treefam_mapping = '/nfs/public/rw/xfam/treefam/live/root/static/download/uniprotACC2treefam.txt';
    open(TREEFAM, $treefam_mapping) or $logger->logdie("Couldn't open fh to $treefam_mapping, $!");
    my %mapping;
    while(<TREEFAM>) {
        my @accns = split(/\s+/,$_);
        if ($accns[0] =~ /[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}/){
            $mapping{$accns[0]}=$accns[1];
        }
    }
    close TREEFAM;
    $logger->debug("Removing old TreeFam mappings from pfamseq");
    my $stt1 = $dbh->prepare("update pfamseq set treefam_acc = \'NULL\'") or die("Can't prepare statement: $dbh->errstr");
    $stt1->execute;


    $logger->debug("Removing old TreeFam mappings from uniprot");
    my $stt1a = $dbh->prepare("update uniprot set treefam_acc = \'NULL\'") or die("Can't prepare statement: $dbh->errstr");
    $stt1a->execute;


    $logger->debug("Updating pfamseq and uniprot with new TreeFam mappings");
    my $stt2 = $dbh->prepare("update pfamseq set treefam_acc = ? where pfamseq_acc = ?") or die("Can't prepare statement: $dbh->errstr");

    my $stt2a = $dbh->prepare("update uniprot set treefam_acc = ? where uniprot_acc = ?") or die("Can't prepare statement: $dbh->errstr");



    foreach my $acc (keys %mapping){
        my $treefam = $mapping{$acc};
        $stt2->execute($treefam, $acc);
        $stt2a->execute($treefam, $acc);
    }

    $proteomeView->touchStatus('doneTreefamUpdate');
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
if (! $proteomeView->statusCheck('done pfamA_ncbi')){

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

        my $queue = $view->{config}->{farm}->{lsf}->{queue};
        my $resource = "rusage[mem=1000]";
        my $memory = 1000;  
        my $fh = IO::File->new();
        my $group = '/PfamViewGroup';
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

    $proteomeView->touchStatus('done pfamA_ncbi');
}



#-------------------------------------------------------------------------------
#Find when this job was last run



#-------------------------------------------------------------------------------
#Select all the families that have a changed date more recent that when the last jobs was started



#Now get a list of all sequences belongs to these families 

