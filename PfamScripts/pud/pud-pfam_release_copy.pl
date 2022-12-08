#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use Cwd;
use Log::Log4perl qw(:easy);

#Script to:
# backup pfam_live
# drop all tables in pfam_live
# copy pfam_release tables to pfam_live


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

my ($live_config, $release_config, $status_dir);
GetOptions('release_config=s' => \$release_config,
  'live_config=s' => \$live_config,
  'status_dir=s' => \$status_dir);


unless(-s $live_config and -s $release_config) {
  $logger->logdie("Need to pass release config and pfamlive config on the command line");
}
unless(-d $status_dir) {
  $logger->logdie("Need to pass status dir on the command line");
}

my $cwd=getcwd;

#Connection for pfam_release
$ENV{'PFAM_CONFIG'}=$release_config;
my $config = Bio::Pfam::Config->new;
my $pfamDB_release = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );


#Connection for pfam_live
$ENV{'PFAM_CONFIG'}=$live_config;
my $config2 = Bio::Pfam::Config->new;
my $pfamDB_live = Bio::Pfam::PfamLiveDBManager->new( %{ $config2->pfamliveAdmin } );


#Make a backup dir
unless(-d "backup") {
  mkdir("backup", 0755) or $logger->logdie("Couldn't mkdir backup, $!");
}
chdir("backup") or $logger->logdie("Couldn't chdir into backup, $!");


#Make a dir for pfamlive backups
my $pfam_live_dir = $pfamDB_live->{database};
unless(-d $pfam_live_dir) {
  mkdir($pfam_live_dir, 0755) or $logger->logdie("Couldn't mkdir dir $pfam_live_dir, $!");
}


#Dump contents of pfamlive
if(-e "$cwd/$status_dir/dumped_pfamlive") {
  $logger->info("Already dumped pfamlive");
}
else {
  $logger->info("Dumping contents of ".$pfamDB_live->{database});
  my $show_tables_command="mysql --skip-column-names -h ".$pfamDB_live->{host}." -u ".$pfamDB_live->{user}." -p". $pfamDB_live->{password}." -P ".$pfamDB_live->{port}." ".$pfamDB_live->{database}." -e \"show tables\"";
  open(TABLES, "$show_tables_command |") or $logger->logdie("Couldn't run '$show_tables_command, $!");
  while(<TABLES>) {
    chomp $_;
    my $table=$_;
    if(-s "$pfam_live_dir/$table.txt") {
      $logger->info("Already done $table");
      next;
    }

    $logger->info("Doing $table");
    my $mysqldump_command = "mysqldump --max_allowed_packet=1024M --routines --quick -uadmin -P".$pfamDB_live->{port}." -h".$pfamDB_live->{host}." -p".$pfamDB_live->{password}." ".$pfamDB_live->{database}." $table > $pfam_live_dir/$table.txt";
    system($mysqldump_command) and $logger->logdie("Couldn't run '$mysqldump_command, $!");
  }
  close TABLES; 

  system("touch $cwd/$status_dir/dumped_pfamlive") and $logger->logdie("Couldn't touch $cwd/$status_dir/dumped_pfamlive, $!");
}
chdir("../") or $logger->logdie("Couldn't chdir up from backup, $!");

#Recreate pfam_live
# dropping used to trigger ERROR 1010 (HY000) - it will drop all the tables, but won't have permission to delete the database
# now it does delete the db, so we must create it
$logger->info("Drop database pfam_live");
my $drop_db_command="mysql -h ".$pfamDB_live->{host}." -u ".$pfamDB_live->{user}." -p". $pfamDB_live->{password}." -P ".$pfamDB_live->{port}." ".$pfamDB_live->{database}." -e \"drop database pfam_live\"";
system($drop_db_command);

$logger->info("Create database pfam_live");
my $create_db_command="mysql -h ".$pfamDB_live->{host}." -u ".$pfamDB_live->{user}." -p". $pfamDB_live->{password}." -P ".$pfamDB_live->{port}." -e \"create database pfam_live\"";
system($create_db_command);


#Copy data from pfam_release into empty pfam_live
#Tables need to be imported in following order to satisfy FK relationships
my @tables=qw(
  ncbi_taxonomy
  taxonomy
  pfamA
  evidence
  uniprot
  markup_key
  wikipedia
  pfamseq
  pfamseq_markup
  pfamseq_disulphide
  pfamseq_antifam
  secondary_pfamseq_acc
  pfamA_reg_seed
  clan
  literature_reference
  _lock
  clan_database_links
  clan_lit_ref
  clan_membership
  dead_clan
  dead_family
  pfamA_database_links
  pfamA_literature_reference
  pfamA_wiki
  current_pfam_version
  nested_domains
  version
  _active_site_hmm_positions
  alignment_and_tree
  architecture
  clan_alignment_and_relationship
  clan_architecture
  complete_proteomes
  edits
  gene_ontology
  interpro
  nested_locations
  other_reg
  pdb
  pdb_image
  pdb_pfamA_reg
  pdb_residue_data
  pfamA2pfamA_hhsearch
  pfamA2pfamA_scoop
  pfamA_architecture
  pfamA_fasta
  pfamA_HMM
  _pfamA_internal
  pfamA_ncbi
  pfam_annseq
  pfamA_reg_full_insignificant
  pfamA_reg_full_significant
  uniprot_reg_full
  pfamA_species_tree
  pfamA_tax_depth
  proteome_architecture
  proteome_regions
  released_clan_version
  released_pfam_version
  pfamA_ncbi_uniprot
  author
  sequence_ontology
  pfamA_author
);

foreach my $table (@tables) {
  $logger->info("Copying $table");
  my $copy_command="mysqldump --max_allowed_packet=1024M --routines --quick -h".$pfamDB_release->{host}." -u ".$pfamDB_release->{user}." -P".$pfamDB_release->{port}." -p".$pfamDB_release->{password}." ".$pfamDB_release->{database}." $table | ";
  $copy_command.="mysql --max_allowed_packet=1024M -h ".$pfamDB_live->{host}." -u ".$pfamDB_live->{user}." -p". $pfamDB_live->{password}." -P ".$pfamDB_live->{port}." ".$pfamDB_live->{database};

  system($copy_command) and $logger->logdie("Couldn't run $copy_command, $!");
}
