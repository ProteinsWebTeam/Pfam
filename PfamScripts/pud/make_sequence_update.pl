#!/usr/bin/env perl

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use Config::General qw(SaveConfig);
use Cwd;
use File::Copy;
use Getopt::Long;
use File::Touch;
use LSF::Job;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


#Get options
my ($help, $new_release_num);
GetOptions(
  "help"  => \$help,
  "release=i" => \$new_release_num);


#Check command line parameteres are sensible
unless($new_release_num) {
  $logger->info("Incorrect command line parameters");
  help();
}


#Get cwd
my $cwd = cwd();


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
if(-e "$status_dir/db_locked") {
  $logger->info("Already locked Pfamlive database");
}
else {
  $logger->info("Locking pfamlive database");
  my $user = $ENV{USER};
  system("pflock -l -allow_user $user") and $logger->logdie("Couldnt run 'pflock -l -allow_user $user, $!");
  system("touch $status_dir/db_locked") and $logger->logdie("Couldn't touch $status_dir/db_locked");
}

#Make a copy of pfamlive
my $config_live = Bio::Pfam::Config->new;
my $queue = $config_live->{farm}->{lsf}->{queue};
if(-e "$status_dir/cloned_database") {
  $logger->info("Already cloned pfamlive");
}
else {
  $logger->info("Cloning pfamlive to pfam_release");

  my $clone_db = LSF::Job->submit(-q => $queue, -o => "$logs_dir/clone_db.log", -J => 'clone_db', "pud-cloneDB.pl -schema -data");
  my $clone_db2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/clone_db.log", -J => 'clone_db_done', -w => "done($clone_db)", "touch $status_dir/cloned_database");

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

  my $tax_job = LSF::Job->submit(-q => $queue, -o => "$logs_dir/taxonomy.log", -J => 'taxonomy', -M => 10000, -R => 'rusage[mem=10000]', "pud-ncbiTaxonomy.pl; pud-taxonomy.pl");
  my $tax_job2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/taxonomy.log", -J => 'taxonomy_done', -w => "done($tax_job)", "touch $status_dir/update_taxonomy");

  $logger->info("Waiting for taxonomy tables to finish populating");
  until(-e "$status_dir/update_taxonomy") {
    sleep 600;
  }
}

#Add ncbi foreign key back to pfamseq
#It was removed in pud-cloneDB.pl as NCBI taxonomy and EBI taxonomy may not be in sync)
if(-e "$status_dir/added_fk_pfamseq") {
  $logger->info("Already removed foreign key from pfamseq table");
}
else { 
  $logger->info("Adding back ncbi foreign key to pfamseq table");
  my $command="mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." ".$pfamDB->{database}." -e ";
  $command.="'alter table pfamseq add constraint FK_pfamseq_1 foreign key (ncbi_taxid) references ncbi_taxonomy (ncbi_taxid) on delete cascade on update no action'";
  
  system("$command") and $logger->logdie("Couldn't add FK to pfamseq table, $!");

  system("touch $status_dir/added_fk_pfamseq") and $logger->logdie("Could not touch $status_dir/added_fk_pfamseq");
}

#Update pfamseq and uniprot tables
if(-e "$status_dir/update_pfamseq" and -e "$status_dir/update_uniprot") {
  $logger->info("Already updated pfamseq and uniprot tables");
}
else {
  $logger->info("Running scripts to update pfamseq and uniprot tables on the farm");

  unless(-e "$status_dir/update_pfamseq") {
    my $pfamseq_job = LSF::Job->submit(-q => $queue, -o => "$logs_dir/pfamseq.log", -J => 'pfamseq', -M => 5000, -R => 'rusage[mem=5000]', "pud-update_pfamseq.pl -status_dir status -pfamseq_dir pfamseq");
    my $pfamseq_job2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/pfamseq.log", -J => 'pfamseq_done', -w => "done($pfamseq_job)", "touch $status_dir/update_pfamseq");
  }

  unless(-e "$status_dir/update_uniprot") {
    my $uniprot_job = LSF::Job->submit(-q => $queue, -o => "$logs_dir/uniprot.log", -J => 'uniprot', -M => 32000, -R => 'rusage[mem=32000]', "pud-update_uniprot.pl -status_dir status -pfamseq_dir pfamseq");
    my $uniprot_job2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/uniprot.log", -J => 'uniprot_done', -w => "done($uniprot_job)", "touch $status_dir/update_uniprot");
  }

  $logger->info("Waiting for pfamseq and uniprot tables to finish populating");
  until(-e "$status_dir/update_pfamseq" and -e "$status_dir/update_uniprot") {
    sleep 600;
  }
}

#Run all sequences against antifam and remove any that match
if(-e "$status_dir/run_antifam") {
  $logger->info("Already removed sequences that match AntiFam");
}
else {
  $logger->info("Removing sequences that match AntiFam");

  system("pud-removeAntiFamMatches.pl -status_dir status -pfamseq_dir pfamseq") and $logger->logdie("Problem running pud-removeAntiFamMatches.pl, $!");

  system("touch $status_dir/run_antifam");
}


#Make uniprot and pfamseq fasta files
if(-e "$status_dir/pfamseq_fasta" and -e "$status_dir/uniprot_fasta") {
  $logger->info("Already made pfamseq and uniprot fasta files");
}
else {
  $logger->info("Making pfamseq and uniprot fasta files on the farm");

  unless(-e "$status_dir/pfamseq_fasta") {
    my $pfamseq_job = LSF::Job->submit(-q => $queue, -o => "$logs_dir/pfamseq_fasta.log", -J => 'pfameq_fasta', -M => 8000, -R => 'rusage[mem=8000]', "pud-make_pfamseq_fasta.pl -status_dir status -pfamseq_dir pfamseq -rel $new_release_num");
    my $pfamseq_job2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/pfamseq_fasta.log", -J => 'pfamseq_fasta_done', -w => "done($pfamseq_job)", "touch $status_dir/pfamseq_fasta");
  }

  unless(-e "$status_dir/uniprot_fasta") {
    my $uniprot_job = LSF::Job->submit(-q => $queue, -o => "$logs_dir/uniprot_fasta.log", -J => 'uniprot_fasta', -M => 32000, -R => 'rusage[mem=32000]', "pud-make_uniprot_fasta.pl -status_dir status -pfamseq_dir pfamseq -rel $new_release_num");
    my $uniprot_job2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/uniprot_fasta.log", -J => 'uniprot_fasta_done', -w => "done($uniprot_job)", "touch $status_dir/uniprot_fasta");
  }   

  $logger->info("Waiting for pfamseq and uniprot fasta files to be written");
  until(-e "$status_dir/pfamseq_fasta" and -e "$status_dir/uniprot_fasta") {
    sleep 600;
  }
}


#Update pfamA_reg_seed table for any deleted sequences in pfamseq/uniprot
if(-e "$status_dir/pfamA_reg_seed") {
  $logger->info("Already updated pfamA_reg_seed table");
}
else {
  $logger->info("Updating pfamA_reg_seed table");
  chdir($pfamseq_dir) or $logger->logdie("Couldn't chdir into $pfamseq_dir, $!");
  my $reg_seed_job = LSF::Job->submit(-q => $queue, -o => "$cwd/$logs_dir/pfamA_reg_seed.log", -J => 'pfamA_reg_seed', "pud-update_pfamA_reg_seed.pl");
  my $reg_seed_job2 = LSF::Job->submit(-q => $queue, -o => "$cwd/$logs_dir/pfamA_reg_seed.log", -J => 'pfamA_reg_seed_done', -w => "done($reg_seed_job)", "touch $cwd/$status_dir/pfamA_reg_seed");

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
  my $shuffled_job = LSF::Job->submit(-q => $queue, -o => "$cwd/$logs_dir/shuffled.log", -J => 'shuffled', -M => 5000, -R => 'rusage[mem=5000]', "esl-shuffle pfamseq > shuffled; esl-sfetch --index shuffled; cp shuffled* $shuffled_dir/.");
  my $shuffled_job2 = LSF::Job->submit(-q => $queue, -o => "$cwd/$logs_dir/shuffled.log", -J => 'shuffled_done', -w => "done($shuffled_job)", "touch $cwd/$status_dir/shuffled");

  chdir($cwd) or $logger->logdie("Couldn't chdir into $cwd, $!");

  $logger->info("Waiting shuffled database to be created");
  until(-e "$status_dir/shuffled") {
    sleep 600;
  }
}

#To do - code this bit (it was done manually for 32.0)
#Update config for pfamseq, uniprot and shuffled db size
#Update /nfs/production/xfam/pfam/data/pfam_svn_server.conf with pfamseq, ncbi (do this after ncbi stuff has run), shuffled dbsize with values from $PFAM_CONFIG


#Change config to point to pfam_live
my $live_config="/nfs/production/xfam/pfam/software/Conf/pfam_svn.conf";
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

  my $copy_pfam_rel = LSF::Job->submit(-q => $queue, -o => "$cwd/$logs_dir/mysqldump.log", -J => 'mysqldump', "pud-pfam_release_copy.pl -status_dir $status_dir -live_config $live_config -release_config $pfam_release_config");
  my $copy_pfam_rel2 = LSF::Job->submit(-q => $queue, -o => "$cwd/$logs_dir/mysqldump.log", -J => 'mysqldump_done', -w => "done($copy_pfam_rel)", "touch $status_dir/copied_pfam_release");

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
  $logger->info("Checking out all Pfam families. Going to lock the Pfam database");

  my $families_dir= "Families";
  mkdir($families_dir, 0755) or $logger->logdie("Couldn't mkdir '$families_dir, $!");
  chdir($families_dir) or $logger->logdie("Couldn't chdir into $families_dir, $!");

  my $family_co = LSF::Job->submit(-q => $queue, -o => "$cwd/$logs_dir/family_checkout.log", -J => 'pfco_families', "pud-checkoutAllFamilies.pl");
  my $family_co2 = LSF::Job->submit(-q => $queue, -o => "$cwd/$logs_dir/family_checkout.log", -J => 'pfco_families_done', -w => "done($family_co)", "touch $cwd/$status_dir/checked_out_families");

  chdir("../") or $logger->logdie("Couldn't chdir up from $families_dir, $!");

  $logger->info("Waiting for all families to checkout");
  until(-e "$status_dir/checked_out_families") {
    sleep 600;
  }

  my $seed_surgery_dir= "SeedSurgery";
  mkdir($seed_surgery_dir, 0755) or $logger->logdie("Couldn't mkdir '$families_dir, $!");

  $logger->info("Time to run seed surgery script on the login nodes:");
  $logger->info("pud-seedSurgery.pl -families $cwd/Families/ -surgery $cwd/$seed_surgery_dir -md5file $cwd/pfamseq/pfamA_reg_seed.md5");
  exit;
}


#Get ncbi database
if(-e "$status_dir/ncbi_database") {
  $logger->info("Already got ncbi database"); 
}
else {
  $logger->info("Getting ncbi database");

  my $ncbi = LSF::Job->submit(-q => $queue, -o => "$logs_dir/ncbi.log", -J => 'ncbi', -M => 16000, -R => 'rusage[mem=16000]', "pud-ncbi.pl");
  my $ncbi2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/ncbi.log", -J => 'ncbi_done', -w => "done($ncbi)", "touch $status_dir/ncbi_database");

  $logger->info("Waiting for ncbi database scripts to finish");
  until(-e "$status_dir/ncbi_database") {
    sleep 600;
  }
}


#Populate other_regions table
if(-e "$status_dir/other_regions") {
  $logger->info("Already calculated other regions"); 
}
else {
  $logger->info("Going to populate other_reg table");

  my $other_reg = LSF::Job->submit(-q => $queue, -o => "$logs_dir/other_reg.log", -J => 'other_reg', -M => 20000, -R => 'rusage[mem=20000]', "pud-otherReg.pl -statusdir $status_dir -pfamseqdir $pfamseq_dir");
  my $other_reg2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/other_reg.log", -J => 'other_reg_done', -w => "done($other_reg)", "touch $status_dir/other_regions");

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

  my $interpro_go = LSF::Job->submit(-q => $queue, -o => "$logs_dir/interpro_go.log", -J => 'interpro_go', -M => 4000, -R => 'rusage[mem=4000]', "pud-buildInterproAndGo.pl");
  my $interpro_go2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/interpro_go.log", -J => 'interpro_go_done', -w => "done($interpro_go)", "touch $status_dir/interpro_and_go");

  $logger->info("Waiting for interpro and gene_ontology tables to finish populating");
  until(-e "$status_dir/other_regions") {
    sleep 300;
  }
}


#Populate RP field in uniprot table
#Need to ensure this is done after the RP have been created for this version of uniprot
#RP is usually created just before uniprot releases publicly
if(-e "$status_dir/RP_done") {
  $logger->info("Already populated RP field in uniprot table"); 
}
else {
  $logger->info("Going to populate RP field in uniprot table");

  my $RP = LSF::Job->submit(-q => $queue, -o => "$logs_dir/RPXX.log", -J => 'RPXX', -M => 10000, -R => 'rusage[mem=10000]', "pud-getRepresentativeProteomes.pl -statusdir $status_dir");
  my $RP2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/RPXX.log", -J => 'RPXX_done', -w => "done($RP)", "touch $status_dir/RP_done");

  $logger->info("Waiting for RP field to finish updating");
  until(-e "$status_dir/RP_done") {
    sleep 600;
  }
}


#Populate pdb and pdb_residue_data tables 
if(-e "$status_dir/pdb_data") {
  $logger->info("Already populated pdb and pdb_residue_data tables"); 
}
else {
  $logger->info("Going to populate pdb and pdb_residue_data");

  my $pdb = LSF::Job->submit(-q => $queue, -o => "$logs_dir/pdb.log", -J => 'pdb', -M => 4000, -R => 'rusage[mem=4000]', "pud-getPdbDataAndMapping.pl");
  my $pdb2 = LSF::Job->submit(-q => $queue, -o => "$logs_dir/pdb.log", -J => 'pdb_done', -w => "done($pdb)", "touch $status_dir/pdb_data");

  $logger->info("Waiting for pdb and pdb_residue_data tables to finish populating");
  until(-e "$status_dir/pdb_data") {
    sleep 600;
  }
}


sub help {


  print STDERR << "EOF";

This script runs the sequence update.

Usage: $0 -release <new release number>

Example:  $0 -release 31 

The script will create 3 directories in the current working 
directory: pfamseq, status and logs. Files will be written
to these directories. Most scripts will run on the farm.


EOF
  exit;
}
