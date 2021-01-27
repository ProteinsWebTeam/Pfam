#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Script to copy released_pfam_version and released_clan_version table data from the last release db to pfam_live db
#The script uses mysqldump to overwrite the released_pfam_version and released_clan_version tables in pfam_live
#Then it deletes any rows in these tables that are not in pfamA (ie deletes rows corresponding to families killed since the last release)

my ($last_release_config, $pfam_live_config);
GetOptions('last_release_config=s' => \$last_release_config,
           'pfam_live_config=s'   => \$pfam_live_config);


unless($last_release_config and -s $last_release_config and $pfam_live_config and -s $pfam_live_config) {
    print STDERR "Need to specify pfam_live config and the config for the last release on the command line\nE.g. $0 -pfam_live_config /nfs/production/xfam/pfam/software/Conf/pfam_svn.conf -last_release_config /nfs/production/xfam/pfam/software/Conf/pfam33.conf\n";
    exit();
}

#Get db connections
$ENV{'PFAM_CONFIG'}=$last_release_config;
my $config1 = Bio::Pfam::Config->new;
my $pfamDB_last_rel = Bio::Pfam::PfamLiveDBManager->new( %{ $config1->pfamlive } );

$ENV{'PFAM_CONFIG'}=$pfam_live_config;
my $config2 = Bio::Pfam::Config->new;
my $pfamDB_live = Bio::Pfam::PfamLiveDBManager->new( %{ $config2->pfamlive } );


#Copy released_pfam_version data from last release to pfamlive
my $mysqldump_pfam_version_command = "mysqldump -h ".$pfamDB_last_rel->{host}." -u ". $pfamDB_last_rel->{user} ." -p". $pfamDB_last_rel->{password}." -P ". $pfamDB_last_rel->{port} . " " . $pfamDB_last_rel->{database}. " released_pfam_version | mysql -h ".$pfamDB_live->{host}." -u ".$pfamDB_live->{adminuser}." -p". $pfamDB_live->{adminpassword}." -P ".$pfamDB_live->{port}." ".$pfamDB_live->{database};
system("$mysqldump_pfam_version_command") and die "Couldn't run '$mysqldump_pfam_version_command', $!";


#Copy released_clan_version data from last release to pfamlive
my $mysqldump_clan_version_command = "mysqldump -h ".$pfamDB_last_rel->{host}." -u ". $pfamDB_last_rel->{user} ." -p". $pfamDB_last_rel->{password}." -P ". $pfamDB_last_rel->{port} . " " . $pfamDB_last_rel->{database}. " released_clan_version | mysql -h ".$pfamDB_live->{host}." -u ".$pfamDB_live->{adminuser}." -p". $pfamDB_live->{adminpassword}." -P ".$pfamDB_live->{port}." ".$pfamDB_live->{database};
system("$mysqldump_clan_version_command") and die "Couldn't run '$mysqldump_clan_version_command', $!";


#Delete any rows which correspond to families deleted since the last release
my $dbh = $pfamDB_live->getSchema->storage->dbh;

my $sth1 = $dbh->prepare("delete from released_pfam_version where pfamA_acc not in (select pfamA_acc from pfamA)");
$sth1->execute() or die "Couldn't execute statement ".$sth1->errstr."\n";

my $sth2 = $dbh->prepare("delete from released_clan_version where clan_acc not in (select clan_acc from clan)");
$sth2->execute() or die "Couldn't execute statement ".$sth2->errstr."\n";
