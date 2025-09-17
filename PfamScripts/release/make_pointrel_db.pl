#!/usr/bin/env perl

#Script to make a point release db from a release db and pfamlive
#Need to create a new (point) release db config before running this script
#(to do this copy the source release $PFAM_CONFIG to another file, and edit accordingly)
#Need to specify the source release config and the target release config on the command line
#Need to run with pfam_live config activated
#If any tables have been added/deleted, will need to edit the list of tables in @tables

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Log::Log4perl qw(:easy);

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

#Get config files
my ($source_config, $target_config);
GetOptions(
  'source_config=s' => \$source_config,
  'target_config=s' => \$target_config
);

unless(-s $source_config and -s $target_config) {
  $logger->logdie( "You need to enter the configs on the command line.\nE.g. $0 -source_config \$PFAM_CONFIG -target_config <path_to_config>");
}

#Get current config (store this, and then set PFAM_CONFIG back to this)
my $original_config=$ENV{'PFAM_CONFIG'};

#Source release config
$ENV{'PFAM_CONFIG'}=$source_config;

my $config = Bio::Pfam::Config->new;
my $sourceDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

#Target release config
$ENV{'PFAM_CONFIG'}=$target_config;
my $config2 = Bio::Pfam::Config->new;
my $targetDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config2->pfamliveAdmin } );

#Change back to original Live db config
$ENV{'PFAM_CONFIG'}=$original_config;
my $config3 = Bio::Pfam::Config->new;
my $liveDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config3->pfamliveAdmin } );

#Create new db
$logger->info("Creating a new database called ".$targetDB->{database}." on host ".$targetDB->{host}.", port ".$targetDB->{port});
my $create_db = "mysql -h ". $targetDB->{host}." -u ".$targetDB->{user}." -p". $targetDB->{password}." -P ".$targetDB->{port}." -e 'create database ".$targetDB->{database}."'";
system("$create_db") and $logger->logdie("Could not create database, $!");


# Import the tables in this order to satisfy FK relationships
# my @tables=qw(ncbi_taxonomy taxonomy sequence_ontology pfamA evidence uniprot markup_key wikipedia pfamseq pfamseq_markup pfamseq_disulphide pfamseq_antifam secondary_pfamseq_acc pfamA_reg_seed clan literature_reference _lock clan_database_links clan_lit_ref clan_wiki clan_membership dead_clan dead_family pfamA_database_links pfamA_literature_reference pfamA_wiki current_pfam_version nested_domains version _active_site_hmm_positions alignment_and_tree architecture clan_alignment_and_relationship clan_architecture complete_proteomes edits gene_ontology interpro nested_locations other_reg pdb pdb_image pdb_pfamA_reg pdb_residue_data pfamA2pfamA_hhsearch pfamA2pfamA_scoop pfamA_architecture pfamA_fasta pfamA_HMM _pfamA_internal pfamA_ncbi pfamA_ncbi_uniprot pfam_annseq pfamA_reg_full_insignificant pfamA_reg_full_significant uniprot_reg_full pfamA_species_tree pfamA_tax_depth proteome_architecture proteome_regions released_clan_version released_pfam_version pfamA_author author);


my $full_relese_db = $sourceDB->{database};
$full_relese_db =~ s/\_\d+$/\_0/;

# create views from base release database for large immutable tables
my @full_tables=qw(ncbi_taxonomy taxonomy uniprot pfamseq pfamseq_markup pfamseq_disulphide other_reg pdb pdb_residue_data);
$logger->info("Going to create views for " . scalar @full_tables . " tables from " .$full_relese_db. " to ".$targetDB->{database});

for my $table (@full_tables) {
  $logger->info("Creating view $table");
  my $command = "mysql -h ".$targetDB->{host}." -u". $targetDB->{user}." -P ".$targetDB->{port}." -p".$targetDB->{password}." ".$targetDB->{database}." -e \"CREATE VIEW ${table} AS SELECT * FROM ${full_relese_db}.${table}\"";
  system("$command") and $logger->logdie("Couldn't copy $table, $!");
}


# copy remaining tables from last point release
my @tables=qw(sequence_ontology pfamA evidence markup_key wikipedia pfamseq_antifam secondary_pfamseq_acc pfamA_reg_seed clan literature_reference _lock clan_database_links clan_lit_ref clan_wiki clan_membership dead_clan dead_family pfamA_database_links pfamA_literature_reference pfamA_wiki current_pfam_version nested_domains version _active_site_hmm_positions alignment_and_tree architecture clan_alignment_and_relationship clan_architecture complete_proteomes edits gene_ontology interpro nested_locations pdb_image pdb_pfamA_reg pfamA2pfamA_hhsearch pfamA2pfamA_scoop pfamA_architecture pfamA_fasta pfamA_HMM _pfamA_internal pfam_annseq pfamA_reg_full_insignificant pfamA_reg_full_significant pfamA_tax_depth proteome_architecture released_clan_version released_pfam_version pfamA_author);
$logger->info("Going to copy across " . scalar @tables . " tables from " .$sourceDB->{database}. " to ".$targetDB->{database});

for my $table (@tables) {
  $logger->info("Copying $table");
  my $command = "mysqldump --max_allowed_packet=1024M -h ".$sourceDB->{host}." -u ".$sourceDB->{user}." -P ".$sourceDB->{port}." -p".$sourceDB->{password}." ".$sourceDB->{database}." $table | mysql --max_allowed_packet=1024M -h ".$targetDB->{host}."  -u". $targetDB->{user}." -P ".$targetDB->{port}." -p".$targetDB->{password}." ".$targetDB->{database};
  system("$command") and $logger->logdie("Couldn't copy $table, $!");
}

# copy author from pfam_live as that can be modified
my @live_tables=qw(author);
$logger->info("Going to copy across " . scalar @live_tables . " tables from " .$liveDB->{database}. " to ".$targetDB->{database});

for my $table (@live_tables) {
  $logger->info("Copying $table");
  my $command = "mysqldump --max_allowed_packet=1024M -h ".$liveDB->{host}." -u ".$liveDB->{user}." -P ".$liveDB->{port}." -p".$liveDB->{password}." ".$liveDB->{database}." $table | mysql --max_allowed_packet=1024M -h ".$targetDB->{host}."  -u". $targetDB->{user}." -P ".$targetDB->{port}." -p".$targetDB->{password}." ".$targetDB->{database};
  system("$command") and $logger->logdie("Couldn't copy $table, $!");
}


$logger->info("Going to drop some FKs on ".$targetDB->{database});
my $command = "mysql -h ".$targetDB->{host}." -u". $targetDB->{user}." -P ".$targetDB->{port}." -p".$targetDB->{password}." ".$targetDB->{database}." -e \"ALTER TABLE edits DROP FOREIGN KEY fk_edits_pfamseq1\" ";
system("$command") and $logger->warn("Couldn't drop fk_edits_pfamseq1, $!");

$command = "mysql -h ".$targetDB->{host}." -u". $targetDB->{user}." -P ".$targetDB->{port}." -p".$targetDB->{password}." ".$targetDB->{database}." -e \"ALTER TABLE nested_locations DROP FOREIGN KEY fk_nested_locations_pfamseq1\" ";
system("$command") and $logger->warn("Couldn't drop fk_nested_locations_pfamseq1, $!");




$logger->info("Successfully Completed!\n");