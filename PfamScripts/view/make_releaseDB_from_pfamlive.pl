#!/usr/bin/env perl

#Script to make a release db from pfamlive
#Need to create a release db config before running this script
#(to do this copy $PFAM_CONFIG to another file, and edit the pfam_live section)
#Need to specify the pfamlive config and release db config on the command line
#If any tables have been added/deleted, will need to edit the list of tables in 
#@tables

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
my ($live_config, $release_config);
GetOptions('live_config=s'    => \$live_config,
  'release_config=s' => \$release_config);

unless(-s $live_config and -s $release_config) {
  $logger->logdie( "You need to enter the configs on the command line.\nE.g. $0 -live_config \$PFAM_CONFIG -release_config <path_to_config>");
}

#Get current config (store this, and then set PFAM_CONFIG back to this)
my $original_config=$ENV{'PFAM_CONFIG'};

#Live config
$ENV{'PFAM_CONFIG'}=$live_config;

my $config = Bio::Pfam::Config->new;
my $liveDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

#Release config
$ENV{'PFAM_CONFIG'}=$release_config;
my $config2 = Bio::Pfam::Config->new;
my $releaseDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config2->pfamliveAdmin } );

#Change back to original config
$ENV{'PFAM_CONFIG'}=$original_config;

#Create new db
$logger->info("Creating a new database called ".$releaseDB->{database}." on host ".$releaseDB->{host}.", port ".$releaseDB->{port});
my $create_db = "mysql -h ". $releaseDB->{host}." -u ".$releaseDB->{user}." -p". $releaseDB->{password}." -P ".$releaseDB->{port}." -e 'create database ".$releaseDB->{database}."'";
system("$create_db") and $logger->logdie("Could not create database, $!");

#Import the tables in this order to satisfy FK relationships
my @tables=qw(ncbi_taxonomy taxonomy sequence_ontology pfamA evidence uniprot markup_key wikipedia pfamseq pfamseq_markup pfamseq_disulphide pfamseq_antifam secondary_pfamseq_acc pfamA_reg_seed clan literature_reference _lock clan_database_links clan_lit_ref clan_membership dead_clan dead_family pfamA_database_links pfamA_literature_reference pfamA_wiki current_pfam_version nested_domains version _active_site_hmm_positions alignment_and_tree architecture clan_alignment_and_relationship clan_architecture complete_proteomes edits gene_ontology interpro nested_locations other_reg pdb pdb_image pdb_pfamA_reg pdb_residue_data pfamA2pfamA_hhsearch pfamA2pfamA_scoop pfamA_architecture pfamA_fasta pfamA_HMM pfamA_interactions _pfamA_internal pfamA_ncbi pfamA_ncbi_uniprot pfam_annseq pfamA_reg_full_insignificant pfamA_reg_full_significant uniprot_reg_full pfamA_species_tree pfamA_tax_depth proteome_architecture proteome_regions released_clan_version released_pfam_version pfamA_author author);
my $table_list=@tables;


#Check number of tables in pfamlive is = to number of tables in @tables
my $dbh = $liveDB->getSchema->storage->dbh;
my $sth=$dbh->prepare("select count(*) from information_schema.tables where table_schema = '".$liveDB->{database}."'");
$sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
my $num_tables=$sth->fetchrow;
if($num_tables == $table_list) {
  $logger->info("Going to copy across the $num_tables tables in pfamlive to ".$releaseDB->{database});
}
else {
  $logger->logdie("The number of tables in pfamlive ($num_tables) is different to the number of tables in the array ($table_list)");
}

for my $table (@tables) {
  $logger->info("Copying $table");
  my $command = "mysqldump --max_allowed_packet=1024M -h ".$liveDB->{host}." -u ".$liveDB->{user}." -P ".$liveDB->{port}." -p".$liveDB->{password}." ".$liveDB->{database}." $table | mysql --max_allowed_packet=1024M -h ".$releaseDB->{host}."  -u". $releaseDB->{user}." -P ".$releaseDB->{port}." -p".$releaseDB->{password}." ".$releaseDB->{database};
  system("$command") and $logger->logdie("Couldn't copy $table, $!");
}
