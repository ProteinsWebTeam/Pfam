#!/usr/bin/env perl
#
# This script updates a release database with the latest pfam_live data.
# Ii should be used to create a new point release, based on an existing release.
#

use strict;
use warnings;

use Cwd;

use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamQC;
use Bio::Pfam::PfamLiveDBManager;

use Getopt::Long;
use Log::Log4perl qw(:easy);
use Time::HiRes qw(usleep);

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();



#Get config files
my ($release_config);
GetOptions(
  'release_config=s' => \$release_config
);

unless(-s $release_config) {
  $logger->logdie( "You need to enter the release config on the command line.\n
    E.g. $0 -release_config <path_to_config>\n
    Additionally the live config needs to be sourced\n");
}

#Live config
#Get current live config (store this, and then set PFAM_CONFIG back to this)
my $live_config = $ENV{'PFAM_CONFIG'};
my $config_live = Bio::Pfam::Config->new;
my $liveDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config_live->pfamlive } );
my $live_dbh    = $liveDB->getSchema->storage->dbh;

#Release config
$ENV{'PFAM_CONFIG'}=$release_config;
my $config_rel = Bio::Pfam::Config->new;
my $releaseDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config_rel->pfamlive } );
my $rel_dbh    = $releaseDB->getSchema->storage->dbh;

#Change back to original live config
$ENV{'PFAM_CONFIG'} = $live_config;

my $pwd = getcwd;

my $live_db = $config_live->pfamlive->{database};
$logger->info("Working on LIVE db $live_db...\n");
my $rel_db = $config_rel->pfamlive->{database};
$logger->info("Working on RELEASE db $rel_db...\n");


my $live_authors = $liveDB->getSchema->resultset('Author')->search();
my $rel_authors = $releaseDB->getSchema->resultset('Author')->search();

if ($live_authors != $rel_authors) {
  $logger->info("Checking 'author' table... release needs updating\n");
  $logger->error("Table author does not match the live copy. Please update the author table on the Release database.");
  die;
} else {
  $logger->info("Checking 'author' table... up to date\n");
}

my $live_sth = $live_dbh->prepare( 'SELECT clan_acc, clan_id, updated FROM clan' );
$live_sth->execute;

my $live_clans = $live_sth->fetchall_hashref('clan_acc');


my $rel_sth = $rel_dbh->prepare( 'SELECT clan_acc, clan_id, updated FROM clan' );
$rel_sth->execute;

my $rel_clans = $rel_sth->fetchall_hashref('clan_acc');


my @clans_common = ();

foreach (sort { $live_clans->{$a}{updated} cmp $live_clans->{$b}{updated} } keys %$live_clans) {
  push(@clans_common, $_) if exists $rel_clans->{$_};
}

my @clans_not_in_rel = ();
foreach (keys %{$live_clans}) {
  push(@clans_not_in_rel, $_) unless exists $rel_clans->{$_};
}

my @clans_not_in_live = ();
foreach (keys %{$rel_clans}) {
  push(@clans_not_in_live, $_) unless exists $live_clans->{$_};
}

my @clans_updated;
foreach (@clans_common) {
  if ($live_clans->{$_}->{updated} gt $rel_clans->{$_}->{updated}) {
     push(@clans_updated, $_);
  }
}

$live_sth = $live_dbh->prepare( 'SELECT pfamA_acc, pfama_id, updated FROM pfamA' );
$live_sth->execute;

my $live_pfama = $live_sth->fetchall_hashref('pfamA_acc');


$rel_sth = $rel_dbh->prepare( 'SELECT pfamA_acc, pfama_id, updated FROM pfamA' );
$rel_sth->execute;

my $rel_pfama = $rel_sth->fetchall_hashref('pfamA_acc');

my @common = ();
foreach (sort { $live_pfama->{$a}{updated} cmp $live_pfama->{$b}{updated} } keys %$live_pfama) {
  push(@common, $_) if exists $rel_pfama->{$_};
}

my @not_in_rel = ();
foreach (keys %{$live_pfama}) {
  push(@not_in_rel, $_) unless exists $rel_pfama->{$_};
}

my @not_in_live = ();
foreach (keys %{$rel_pfama}) {
  push(@not_in_live, $_) unless exists $live_pfama->{$_};
}

my @updated;
foreach (@common) {
  if ($live_pfama->{$_}->{updated} gt $rel_pfama->{$_}->{updated}) {
     push(@updated, $_);
  }
}


$logger->info( scalar @common . " families and " . scalar @clans_common . " clans in release do not need updating.\n");
$logger->info( scalar @updated . " families and " . scalar @clans_updated . " clans need updating in release.\n");
$logger->info( scalar @not_in_rel . " families and " . scalar @clans_not_in_rel . " clans need to be added to release.\n");
$logger->info( scalar @not_in_live . " families and " . scalar @clans_not_in_live . " clans need to be removed from release.\n");



if (!-e "view_clans") {
  my %view_clans;
  foreach my $clan_to_vp( (@clans_updated,@clans_not_in_rel) ) {
    next if (exists $view_clans{$clan_to_vp});
    my $clan_id = $liveDB->clanAcc2Id($clan_to_vp);
    $view_clans{$clan_to_vp} = $clan_id;
  }

  open (my $clan_fh, '>', "view_clans");
  foreach my $view_clan_acc (sort keys %view_clans){
    print $clan_fh "$view_clan_acc\n";
  }
  close ($clan_fh);
} else {
  $logger->info("view_clans file already exists. will not overwrite");
}



if (!-e "view_fams") {
  my %view_fams;
  foreach my $fam_to_vp ( (@updated,@not_in_rel) ) {
    next if (exists $view_fams{$fam_to_vp});
    my $fam_id = $liveDB->acc2id($fam_to_vp);
    $view_fams{$fam_to_vp} = $fam_id;
    
  }
  open (my $fams_fh, '>', "view_fams");
  foreach my $view_acc (sort keys %view_fams){
    print $fams_fh "$view_acc\n";
  }
  close ($fams_fh);
} else {
  $logger->info("view_clans file already exists. will not overwrite");
}




$logger->info("Going to update RELEASE to delete clans...\n");

foreach my $clan_rm (sort @clans_not_in_live) {

  $releaseDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config_rel->pfamlive } );

  $logger->info("Removing clan $clan_rm...");
  $releaseDB->deleteClan( $clan_rm, 'point_release', '' , $rel_db );
  $logger->info("\tRemoved $clan_rm");
}


$logger->info("Going to update RELEASE to update clans...\n");

foreach my $clan_upd (@clans_updated) {
  $logger->info("Checking out clan $clan_upd...");
  system("clco $clan_upd");
  my $clanIO = Bio::Pfam::ClanIO->new;
  my $clanObj = $clanIO->loadClanFromLocalFile( $clan_upd, $pwd );
  $clanIO->updateClanInRDB($clanObj, $releaseDB, 0 );

  $releaseDB->getSchema->resultset('ClanAlignmentAndRelationship')->search({
      clan_acc => $clan_upd
    })->delete;

  $logger->info("\tUpdated $clan_upd");
}

$logger->info("Going to update RELEASE to add new clans...\n");
foreach my $clan_new (sort @clans_not_in_rel) {
  system("clco $clan_new");
  my $clanIO = Bio::Pfam::ClanIO->new;
  my $clanObj = $clanIO->loadClanFromLocalFile( $clan_new, $pwd );

  $clanIO->updateClanInRDB($clanObj, $releaseDB, 1, $rel_db);

  $logger->info("\tAdded $clan_new");
}





$logger->info("Going to update RELEASE to delete families...\n");
foreach my $fam_rm (sort @not_in_live) {

  $releaseDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config_rel->pfamlive } );

  $logger->info("Removing family $fam_rm...\n");
  $releaseDB->deletePfamA( $fam_rm, 'point_release', '' , $rel_db );

  my $fam_clan_db = $releaseDB->getClanDataByPfam($fam_rm) // '';

  if ($fam_clan_db) {
    $fam_clan_db = $fam_clan_db->clan_acc->clan_acc;
    $releaseDB->removeFamilyFromClanMembership($fam_clan_db, $fam_rm);
  }
  $logger->info("\tRemoved $fam_rm");

}






$logger->info("Going to update RELEASE to update families...\n");
foreach my $fam_upd (@updated) {
  $logger->info("Checking out family $fam_upd...");
  $releaseDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config_rel->pfamlive } );

  system("pfco $fam_upd");
  my $familyIO = Bio::Pfam::FamilyIO->new;
  my $famObj = $familyIO->loadPfamAFromLocalFile( $fam_upd, $pwd );

  my $fam_clan = $famObj->DESC->CL // '';
  my $fam_clan_db = $releaseDB->getClanDataByPfam($fam_upd) // '';

  if ($fam_clan_db) {
    $fam_clan_db = $fam_clan_db->clan_acc->clan_acc;
  }

  Bio::Pfam::PfamQC::sequenceChecker( $fam_upd, $famObj, $releaseDB, 1 );

  $familyIO->updatePfamAInRDB($famObj, $releaseDB, 0);

  $familyIO->updatePfamARegions($famObj, $releaseDB);

  $familyIO->uploadPfamAHMM($famObj, $releaseDB, $pwd, 0);

  $familyIO->uploadPfamAAligns($famObj, $releaseDB, $pwd, 0);

  $familyIO->create_or_update_author($releaseDB, $famObj);




  if ($fam_clan ne $fam_clan_db) {
    if ($fam_clan_db) {
      $releaseDB->removeFamilyFromClanMembership($fam_clan_db, $fam_upd);
    }
    if ($fam_clan) {
      $releaseDB->updateClanMembership($fam_clan, $fam_upd);
    }
  }

  $releaseDB->getSchema->resultset('AlignmentAndTree')->search({
      pfama_acc => $fam_upd
    })->delete;

  $logger->info("\tUpdated $fam_upd");
}




$logger->info("Going to update RELEASE to add new families...\n");
foreach my $fam_new (sort @not_in_rel) {
  $logger->info("Checking out family $fam_new...");
  $releaseDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config_rel->pfamlive } );

  system("pfco $fam_new");
  my $familyIO = Bio::Pfam::FamilyIO->new;
  my $famObj = $familyIO->loadPfamAFromLocalFile( $fam_new, $pwd );
  my $fam_clan = $famObj->DESC->CL;

  Bio::Pfam::PfamQC::sequenceChecker( $fam_new, $famObj, $releaseDB, 1 );

  $familyIO->updatePfamAInRDB($famObj, $releaseDB, 1, $rel_db);

  $familyIO->updatePfamARegions($famObj, $releaseDB);

  $familyIO->uploadPfamAHMM($famObj, $releaseDB, $pwd, 0);

  $familyIO->uploadPfamAAligns($famObj, $releaseDB, $pwd, 0);

  $familyIO->create_or_update_author($releaseDB, $famObj);

  if ($fam_clan) {
    $releaseDB->updateClanMembership($fam_clan, $fam_new);
  }

  $logger->info("\tAdded $fam_new");
}


$logger->info("Successfully Completed!\n");

