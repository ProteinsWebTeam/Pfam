#!/usr/bin/env perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Config::General qw(SaveConfig);
use Cwd;
use File::Copy;
use Getopt::Long;
use File::Touch;
# use LSF::Job;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


#Get options
my ($help, $new_release_num, $last_release);
GetOptions(
  "help"  => \$help,
  "release=i" => \$new_release_num,
  "last_release=s" => \$last_release);


#Check command line parameteres are sensible
unless($new_release_num) {
  $logger->info("Incorrect command line parameters");
  help();
}


#Get cwd
my $cwd = cwd();

my $config_live = Bio::Pfam::Config->new;
my $queue = $config_live->{farm}->{lsf}->{queue};


#Make pfamseq, status and logs directories
my $status_dir = "status";
my $pfamseq_dir = "pfamseq";
my $logs_dir = "logs";
foreach my $dir ($status_dir, $pfamseq_dir, $logs_dir) {
  if(-d $dir) {
    $logger->info("Already made $dir directory");
  }
  else {
    $logger->info("Making $dir directory");
    mkdir($dir, 0775 ) or $logger->logdie("Couldn't mkdir $dir, $!");
  }
}


#Lock the database
# if(-e "$status_dir/db_locked") {
#   $logger->info("Already locked Pfamlive database");
# }
# else {
#   $logger->info("Locking pfamlive database");
#   my $user = $ENV{USER};
#   system("pflock -l -allow_user $user") and $logger->logdie("Couldnt run 'pflock -l -allow_user $user, $!");
#   system("touch $status_dir/db_locked") and $logger->logdie("Couldn't touch $status_dir/db_locked");
# }


#Make a copy of pfamlive
if(-e "$status_dir/cloned_database") {
  $logger->info("Already cloned pfamlive");
}
else {
  $logger->info("Cloning pfamlive to pfam_release");

  my $jobid;
  my $job_res = `sbatch --mem=2GB --time=8:00:00 -J clone_db -o '$logs_dir/clone_db.log' -e '$logs_dir/clone_db.log' --wrap="pud-cloneDB.pl -schema -data"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }

  system("sbatch --job-name=clone_db_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/clone_db_done.log' -e '$logs_dir/clone_db_done.log' --wrap=\"touch $status_dir/cloned_database\" ");

  $logger->info("Waiting for cloning of pfamlive to finish");
  until(-e "$status_dir/cloned_database") {
    sleep 600;
  }
}


#Create config for pfam_release
my $pfam_release_config="$cwd/pfam_release.conf";
if(-e "$status_dir/pfam_release_config") {
  $logger->info("Already created a pfam_release config");
}
else {
  $logger->info("Creating a pfam_release config file");

  my $conf = new Config::General($ENV{PFAM_CONFIG}); 
  my %ac = $conf->getall; 

  $ac{Model}->{Pfamlive}->{database} = "pfam_release"; 

  SaveConfig($pfam_release_config, \%ac);

  system("touch $status_dir/pfam_release_config") and $logger->logdie("Couldn't touch $status_dir/pfam_release_config");
}


#Get db connection to pfam_release
$logger->info("Changing PFAM_CONFIG to $pfam_release_config");
$ENV{'PFAM_CONFIG'}=$pfam_release_config;
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Update ncbi_taxonomy and taxonomy tables
if(-e "$status_dir/update_taxonomy") {
  $logger->info("Already updated taxonomy");
}
else {
  $logger->info("Running scripts to update taxonomy tables on the farm");

  my $jobid;
  my $job_res = `sbatch --mem=12GB --time=6:00:00 -J taxonomy -o '$logs_dir/taxonomy.log' -e '$logs_dir/taxonomy.log' --wrap="pud-ncbiTaxonomy.pl; pud-taxonomy.pl"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }

  system("sbatch --job-name=taxonomy_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/taxonomy_done.log' -e '$logs_dir/taxonomy_done.log' --wrap=\"touch $status_dir/update_taxonomy\" ");

  $logger->info("Waiting for taxonomy tables to finish populating");
  until(-e "$status_dir/update_taxonomy") {
    sleep 600;
  }
}



#Update pfamseq and uniprot tables
if(-e "$status_dir/update_pfamseq" and -e "$status_dir/update_uniprot") {
  $logger->info("Already updated pfamseq and uniprot tables");
}
else {
  $logger->info("Running scripts to update pfamseq and uniprot tables on the farm");

  unless(-e "$status_dir/update_pfamseq") {
    my $jobid;
    my $job_res = `sbatch --mem=8GB --time=3-0 -J pfamseq -o '$logs_dir/pfamseq.log' -e '$logs_dir/pfamseq.log' --wrap="pud-update_pfamseq.pl -status_dir status -pfamseq_dir pfamseq"`;

    if ($job_res =~ /^Submitted batch job (\d+)/ ) {
      $jobid = $1;
    }
    system("sbatch --job-name=pfamseq_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/pfamseq_done.log' -e '$logs_dir/pfamseq_done.log' --wrap=\"touch $status_dir/update_pfamseq\" ");
  }

  unless(-e "$status_dir/update_uniprot") {
    my $jobid;
    my $job_res = `sbatch --mem=16GB --time=6-0 -J uniprot -o '$logs_dir/uniprot.log' -e '$logs_dir/uniprot.log' --wrap="pud-update_uniprot.pl -status_dir status -pfamseq_dir pfamseq"`;

    if ($job_res =~ /^Submitted batch job (\d+)/ ) {
      $jobid = $1;
    }
    system("sbatch --job-name=uniprot_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/uniprot_done.log' -e '$logs_dir/uniprot_done.log' --wrap=\"touch $status_dir/update_uniprot\" ");
  }

  $logger->info("Waiting for pfamseq and uniprot tables to finish populating");
  until(-e "$status_dir/update_pfamseq" and -e "$status_dir/update_uniprot") {
    sleep 600;
  }
}


#Add ncbi foreign key back to pfamseq
#It was removed in pud-cloneDB.pl as NCBI taxonomy and EBI taxonomy may not be in sync)
if(-e "$status_dir/added_fk_pfamseq") {
  $logger->info("Already reinstated ncbi foreign key to pfamseq table");
}
else { 
  $logger->info("Adding back ncbi foreign key to pfamseq table");
  my $command="mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{adminuser}." -p". $pfamDB->{adminpassword}." -P ".$pfamDB->{port}." ".$pfamDB->{database}." -e ";
  $command.="'alter table pfamseq add constraint FK_pfamseq_taxid foreign key (ncbi_taxid) references ncbi_taxonomy (ncbi_taxid) on delete cascade on update no action'";
  system("$command");

  system("touch $status_dir/added_fk_pfamseq") and $logger->logdie("Could not touch $status_dir/added_fk_pfamseq");
}




#Run all sequences against antifam and remove any that match
if(-e "$status_dir/run_antifam") {
  $logger->info("Already removed sequences that match AntiFam");
}
else {
  $logger->info("Removing sequences that match AntiFam");

  my $jobid;
  my $job_res = `sbatch --mem=8GB --time=12:00:00 -J antifam -o '$logs_dir/antifam.log' -e '$logs_dir/antifam.log' --wrap="pud-removeAntiFamMatches.pl -status_dir status -pfamseq_dir pfamseq -logs_dir logs"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  system("sbatch --job-name=antifam_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/antifam_done.log' -e '$logs_dir/antifam_done.log' --wrap=\"touch $status_dir/run_antifam\" ");

  $logger->info("Waiting for antifam matches to be removed");
  until(-e "$status_dir/run_antifam") {
    sleep 600;
  }
}


#Make uniprot and pfamseq fasta files
if(-e "$status_dir/pfamseq_fasta" and -e "$status_dir/uniprot_fasta") {
  $logger->info("Already made pfamseq and uniprot fasta files");
}
else {
  $logger->info("Making pfamseq and uniprot fasta files on the farm");

  unless(-e "$status_dir/pfamseq_fasta") {

    my $jobid;
    my $job_res = `sbatch --mem=32GB --time=2:00:00 -J pfamseq_fasta -o '$logs_dir/pfamseq_fasta.log' -e '$logs_dir/pfamseq_fasta.log' --wrap="pud-make_pfamseq_fasta.pl -status_dir status -pfamseq_dir pfamseq -rel $new_release_num"`;

    if ($job_res =~ /^Submitted batch job (\d+)/ ) {
      $jobid = $1;
    }
    system("sbatch --job-name=pfamseq_fasta_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/pfamseq_fasta_done.log' -e '$logs_dir/pfamseq_fasta_done.log' --wrap=\"touch $status_dir/pfamseq_fasta\" ");
  }

  unless(-e "$status_dir/uniprot_fasta") {

    my $jobid;
    my $job_res = `sbatch --mem=64GB --time=4:00:00 -J uniprot_fasta -o '$logs_dir/uniprot_fasta.log' -e '$logs_dir/uniprot_fasta.log' --wrap="pud-make_uniprot_fasta.pl -status_dir status -pfamseq_dir pfamseq -rel $new_release_num"`;

    if ($job_res =~ /^Submitted batch job (\d+)/ ) {
      $jobid = $1;
    }
    system("sbatch --job-name=uniprot_fasta_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/uniprot_fasta_done.log' -e '$logs_dir/uniprot_fasta_done.log' --wrap=\"touch $status_dir/uniprot_fasta\" ");
  }

  $logger->info("Waiting for pfamseq and uniprot fasta files to be written");
  until(-e "$status_dir/pfamseq_fasta" and -e "$status_dir/uniprot_fasta") {
    sleep 600;
  }
}


#Update pfamseq symbolic link to point to new pfamseq
if(-e "$status_dir/pfamseq_sym_link") {
    $logger->info("Already updated pfamseq symbolic link");
}
else {
    $logger->info("Updating pfamseq symbolic link");
    my $new_pfamseq = "pfamseq" . $new_release_num;

    chdir("/nfs/production/agb/pfam/data/") or die "Couldn't chdir into /nfs/production/agb/pfam/data/ $!";
    system("rm -f pfamseq") and die "Couldn't remove old symbolic link, $!";
    system("ln -s $new_pfamseq pfamseq") and $logger->logdie("Couldn't create new symbolic link for pfamseq, $!");

    chdir($cwd) or die "Couldn't chdir into $cwd, $!";
    system("touch $status_dir/pfamseq_sym_link") and $logger->logdie("Couldn't touch $status_dir/pfamseq_sym_link");
}


#Update pfamA_reg_seed table for any deleted sequences in pfamseq/uniprot
if(-e "$status_dir/pfamA_reg_seed") {
  $logger->info("Already updated pfamA_reg_seed table");
}
else {
  $logger->info("Updating pfamA_reg_seed table");
  chdir($pfamseq_dir) or $logger->logdie("Couldn't chdir into $pfamseq_dir, $!");

  my $jobid;
  my $job_res = `sbatch --mem=4GB --time=6:00:00 -J pfamA_reg_seed -o '$cwd/$logs_dir/pfamA_reg_seed.log' -e '$cwd/$logs_dir/pfamA_reg_seed.log' --wrap="pud-update_pfamA_reg_seed.pl"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  system("sbatch --job-name=pfamA_reg_seed_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$cwd/$logs_dir/pfamA_reg_seed_done.log' -e '$cwd/$logs_dir/pfamA_reg_seed_done.log' --wrap=\"touch $cwd/$status_dir/pfamA_reg_seed\" ");


  chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");

  $logger->info("Waiting for pfamA_reg_seed to finish updating");
  until(-e "$status_dir/pfamA_reg_seed") {
    sleep 600;
  }
}

#Create shuffled database
if(-e "$status_dir/shuffled") {
  $logger->info("Already created shuffled database");
}
else {
  $logger->info("Creating and indexing shuffled database");
  chdir($pfamseq_dir) or $logger->logdie("Couldn't chdir into $pfamseq_dir, $!");

  my $shuffled_dir = $config_live->{shuffled}->{location};

  my $jobid;
  my $job_res = `sbatch --mem=24GB --time=2:00:00 -J shuffled -o '$cwd/$logs_dir/shuffled.log' -e '$cwd/$logs_dir/shuffled.log' --wrap="esl-shuffle pfamseq > shuffled; esl-sfetch --index shuffled; cp shuffled* $shuffled_dir/."`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  system("sbatch --job-name=shuffled_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$cwd/$logs_dir/shuffled_done.log' -e '$cwd/$logs_dir/shuffled_done.log' --wrap=\"touch $cwd/$status_dir/shuffled\" ");


  chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");

  $logger->info("Waiting shuffled database to be created");
  until(-e "$status_dir/shuffled") {
    sleep 600;
  }
  print "\nReady to update live config. Also xfam-svn-hl also needs updating to new pfamseq.\n\n";
  exit;
}


#To do - code this bit (it was done manually for 32.0)
# Update config for pfamseq, uniprot and shuffled db size
# Update /hps/software/users/agb/pfam/software/conf/pfam_svn.conf with pfamseq and shuffled dbsize with values
# from $PFAM_CONFIG. Those can be found on the pfamseq_fasta.log, uniprot_fasta.log and shuffled.log
# Also the config on xfam-svn-hl needs updating:
# on codon: become xfm_adm
# ssh xfam-svn-hl
# vim /nfs/production/xfam/pfam/data/pfam_svn_server.conf
# and update pfamseq and uniprot db sizes
# Also copy the new sequence database to xfam-svn-hl
# on codon, in a screen session:
# become xfm_adm
# srun --time=1-0 --mem=2GB -p datamover --pty bash
# rsync -rtv /nfs/production/agb/pfam/data/pfamseq/ xfam-svn-hl:/nfs/production/xfam/pfam/data/pfamseq



#Change config to point to pfam_live
my $live_config="/hps/software/users/agb/pfam/software/conf/pfam_svn.conf";
unless(-s $live_config) {
  $logger->logdie("$live_config does not exist");
}
$logger->info("Changing PFAM_CONFIG back to $live_config");
$ENV{'PFAM_CONFIG'}=$live_config;


#Dump pfamlive db and copy contents of pfam_release to pfamlive
if(-e "$status_dir/copied_pfam_release") {
  $logger->info("Already copied pfam_release over to pfamlive");
}
else {
  $logger->info("Backing up pfamlive and copying pfam_release to pfamlive");

  my $jobid;
  my $job_res = `sbatch --mem=8GB --time=9-0 -J mysqldump -o '$cwd/$logs_dir/mysqldump.log' -e '$cwd/$logs_dir/mysqldump.log' --wrap="pud-pfam_release_copy.pl -status_dir $status_dir -live_config $live_config -release_config $pfam_release_config"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  system("sbatch --job-name=mysqldump_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$cwd/$logs_dir/mysqldump_done.log' -e '$cwd/$logs_dir/mysqldump_done.log' --wrap=\"touch $status_dir/copied_pfam_release\" ");


  $logger->info("Waiting for backup of pfamlive and copy of pfam_release data to pfamlive to finish");
  until(-e "$status_dir/copied_pfam_release") {
    sleep 600;
  }
}


#Check out all families
if(-e "$status_dir/checked_out_families") {
  $logger->info("Already checked out families");
}
else {
  $logger->info("Checking out all Pfam families");

  my $families_dir= "Families";
  mkdir($families_dir, 0755) or $logger->logdie("Couldn't mkdir '$families_dir, $!");
  chdir($families_dir) or $logger->logdie("Couldn't chdir into $families_dir, $!");

  my $jobid;
  my $job_res = `sbatch --mem=4GB --time=2-0 -J pfco_families -o '$cwd/$logs_dir/family_checkout.log' -e '$cwd/$logs_dir/family_checkout.log' --wrap="pud-checkoutAllFamilies.pl"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  system("sbatch --job-name=pfco_families_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$cwd/$logs_dir/family_checkout_done.log' -e '$cwd/$logs_dir/family_checkout_done.log' --wrap=\"touch $cwd/$status_dir/checked_out_families\" ");


  chdir("../") or $logger->logdie("Couldn't chdir up from $families_dir, $!");

  $logger->info("Waiting for all families to checkout");
  until(-e "$status_dir/checked_out_families") {
    sleep 600;
  }
}


#Run seed surgery
if(-e "$status_dir/ran_seed_surgery") {
  $logger->info("Already done seed_surgery");
}
else {
  $logger->info("Running seed surgery script");
    my $seed_surgery_dir= "SeedSurgery";
    unless(-d $seed_surgery_dir) {
    mkdir($seed_surgery_dir, 0755) or $logger->logdie("Couldn't mkdir '$seed_surgery_dir, $!");
  }

  my $jobid;
  my $job_res = `sbatch --mem=16GB --time=3-0 -J seedsurgery -o '$cwd/$logs_dir/seedsurgery.log' -e '$cwd/$logs_dir/seedsurgery.log' --wrap="pud-seedSurgery.pl -families $cwd/Families/ -surgery $cwd/$seed_surgery_dir -md5file $cwd/pfamseq/pfamA_reg_seed.md5"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  system("sbatch --job-name=seedsurgery_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$cwd/$logs_dir/seedsurgery_done.log' -e '$cwd/$logs_dir/seedsurgery_done.log' --wrap=\"touch $status_dir/ran_seed_surgery\" ");


  $logger->info("Waiting for seed surgery script to finish");
  until(-e "$status_dir/ran_seed_surgery") {
    sleep 600;
  }
  $logger->info("Seed surgery script completed. Exiting");
  print "\nReady to start curator SEED surgery and family building.\n\n";
  exit;
}




#Populate other_regions table
if(-e "$status_dir/other_regions") {
  $logger->info("Already calculated other regions"); 
}
else {
  $logger->info("Going to populate other_reg table");

  # my $other_reg = LSF::Job->submit(-q => $queue, -o => "$logs_dir/other_reg.log", -J => 'other_reg', -M => 64000, -R => 'rusage[mem=64000]', "pud-otherReg.pl -statusdir $status_dir -pfamseqdir $pfamseq_dir");
  # my $other_reg2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/other_reg.log", -J => 'other_reg_done', -w => "done($other_reg)", "touch $status_dir/other_regions");

  my $jobid;
  my $job_res = `sbatch --mem=128GB --time=2-0 -J other_reg -o '$logs_dir/other_reg.log' -e '$logs_dir/other_reg.log' --wrap="pud-otherReg.pl -statusdir $status_dir -pfamseqdir $pfamseq_dir"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  system("sbatch --job-name=other_reg_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/other_reg_done.log' -e '$logs_dir/other_reg_done.log' --wrap=\"touch $status_dir/other_regions\" ");


  $logger->info("Waiting for other_reg table to finish populating");
  until(-e "$status_dir/other_regions") {
    sleep 600;
  }
}

#Populate interpro and gene_ontology tables
if(-e "$status_dir/interpro_and_go") {
  $logger->info("Already populated interpro and gene_ontology tables"); 
}
else {
  $logger->info("Going to populate interpro and gene_ontology tables");

  # my $interpro_go = LSF::Job->submit(-q => $queue, -o => "$logs_dir/interpro_go.log", -J => 'interpro_go', -M => 4000, -R => 'rusage[mem=4000]', "pud-buildInterproAndGo.pl");
  # my $interpro_go2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/interpro_go.log", -J => 'interpro_go_done', -w => "done($interpro_go)", "touch $status_dir/interpro_and_go");

  my $jobid;
  my $job_res = `sbatch --mem=8GB --time=1-0 -J interpro_go -o '$logs_dir/interpro_go.log' -e '$logs_dir/interpro_go.log' --wrap="pud-buildInterproAndGo.pl"`;

  if ($job_res =~ /^Submitted batch job (\d+)/ ) {
    $jobid = $1;
  }
  system("sbatch --job-name=interpro_go_done --dependency=afterok:${jobid} --time=1:00 --mem=100 -o '$logs_dir/interpro_go_done.log' -e '$logs_dir/interpro_go_done.log' --wrap=\"touch $status_dir/interpro_and_go\" ");



  $logger->info("Waiting for interpro and gene_ontology tables to finish populating");
  until(-e "$status_dir/interpro_and_go") {
    sleep 300;
  }
}


# NOT DONE 38
#Populate RP field in uniprot table
#Need to ensure this is done after the RP have been created for this version of uniprot
#RP is usually created just before uniprot releases publicly
# if(-e "$status_dir/RP_done") {
#   $logger->info("Already populated RP field in uniprot table"); 
# }
# else {
#   $logger->info("Going to populate RP field in uniprot table");

#   my $RP = LSF::Job->submit(-q => $queue, -o => "$logs_dir/RPXX.log", -J => 'RPXX', -M => 20000, -R => 'rusage[mem=20000]', "pud-getRepresentativeProteomes.pl -statusdir $status_dir");
#   my $RP2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/RPXX.log", -J => 'RPXX_done', -w => "done($RP)", "touch $status_dir/RP_done");

#   $logger->info("Waiting for RP field to finish updating");
#   until(-e "$status_dir/RP_done") {
#     sleep 600;
#   }
# }




# NOT DONE 38
#Populate pdb and pdb_residue_data tables 
# if(-e "$status_dir/pdb_data") {
#   $logger->info("Already populated pdb and pdb_residue_data tables"); 
# }
# else {
#   $logger->info("Going to populate pdb and pdb_residue_data");

#   my $pdb = LSF::Job->submit(-q => $queue, -o => "$logs_dir/pdb.log", -J => 'pdb', -M => 4000, -R => 'rusage[mem=4000]', "pud-getPdbDataAndMapping.pl");
#   my $pdb2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/pdb.log", -J => 'pdb_done', -w => "done($pdb)", "touch $status_dir/pdb_data");

#   $logger->info("Waiting for pdb and pdb_residue_data tables to finish populating");
#   until(-e "$status_dir/pdb_data") {
#     sleep 600;
#   }
# }



#Copy released_pfam_version and released_clan_version table data from the last release db to pfam_live db
#Also put a copy of the release config in the configs directory
if(-e "$status_dir/released_version_tables") {
    $logger->info("Already copied released version tables");
}
else {

    unless($last_release) {
      $logger->info("Please provide last_release as an argument in the format 'XX_X'. ie:\nmake_sequence_update.pl -release 38 -last_release 37_4\n");
      help();
    }


    my $config_dir = "/hps/software/users/agb/pfam/software/conf/";
    my $new_rel_config = "pfam".$new_release_num.".conf";
    $logger->info("Going to copy the pfam release config $pfam_release_config to $config_dir/$new_rel_config");

    system("cp $pfam_release_config $config_dir/$new_rel_config") and die "Couldn't 'cp $pfam_release_config $config_dir/$new_rel_config, $!";

    my $last_rel_db = "pfam_".$last_release;
    my $last_release_conf = "$config_dir/pfam".$last_release.".conf";


    $logger->info("Going to copy released_pfam_version and released_clan_version table data from $last_rel_db to pfam_live database");
    $logger->info("*** Note: If $last_release_conf is not the previous released version of Pfam, you will need to run pud-copy_versions.pl and pass in the config file for the previous release on the command line ***");
    system("pud-copy_versions.pl -pfam_live_config $live_config -last_release_config $last_release_conf") and die "Couldn't run 'pud-copy_versions.pl -pfam_live_config $live_config -last_release_config $last_release_conf', $!"; 

    system("touch $status_dir/released_version_tables") and $logger->logdie("Couldn't touch $status_dir/released_version_tables");
}

print "All completed!\nRemember to manually add sequence database sizes to svn.\n";
exit;

# Adding sequence database sizes to svn
# Add the current sequence database size for pfamseq and uniprot to svn. The database size in svn is used by Pfam Docker to see if the user has the current sequence database. The commit messages should follow the format of the previous commit messages for each database and always start with SEQUP.

# Get number of sequences in pfamseq from PFAM_CONFIG
# cd /nfs/production/agb/pfam/data/Sequences/pfamseq
# echo <pfamseq_dbsize> > pfamseq   (replace pfamseq_dbsize with the current number)
# svn commit -m "SEQUP: Pfamseq 34.0 dbsize" pfamseq

# Get number of sequences in uniprot from PFAM_CONFIG
# cd /nfs/production/xfam/pfam/data/Sequences/uniprotkb
# echo <uniprot_dbsize > > uniprot (replace uniprot_dbsize with the current number)
# svn commit -m "SEQUP: Uniprot dbsize for Pfam 34.0" uniprot 





sub help {


  print STDERR << "EOF";

This script runs the sequence update.

Usage: $0 -release <new release number> -last_release <previous_release_version>

Example:  $0 -release 38 -last_release 37_4

The script will create 3 directories in the current working 
directory: pfamseq, status and logs. Files will be written
to these directories. Most scripts will run on the farm.


EOF
  exit;
}
