#!/usr/bin/env perl
#
# Generates the apicuron data referring to a minor pfam release

use strict;
use warnings;

use Cwd;

use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
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


my $live_db = $config_live->pfamlive->{database};
$logger->info("Working on LIVE db $live_db...\n");
my $rel_db = $config_rel->pfamlive->{database};
$logger->info("Working on RELEASE db $rel_db...\n");

# Curator list, add new as needed
my $curators = {
    'agb'          => '0000-0002-6982-4660',
    'schuguransky' => '0000-0002-0520-0736',
    'blp'          => '0000-0001-6837-2941',
    'aea'          => '0000-0002-0450-0091',
    'typhaine'     => '0000-0001-5663-0894',
    'sanchez'      => '0000-0002-2300-8502',
    'vmonzon'      => '0000-0001-7125-6212',
    'aleixlafita'  => '0000-0003-1549-3162',
    'emogro'       => '0000-0003-3544-1634',
    'ddallachiesa' => '0009-0000-9283-3330',
    'ebarrionuevo' => '0000-0003-3271-6305',
    'pthomas'      => '0009-0005-3611-2148',
    'nflores'      => '0000-0003-2496-881X'
};


# Prepare the output file
open (my $fh, '>', "apicuron_${rel_db}_update.json");

my $output = <<EOL;
{
    "reports": [
EOL

# Get updated families
my $rel_sth = $rel_dbh->prepare( "select pfamA_acc from pfamA where change_status = 'CHANGED' limit 5" );
$rel_sth->execute;

my $changed = $rel_sth->fetchall_arrayref;

foreach (@{$changed}) {
  my $acc = shift @{$_};

  my $svn_log = `svn log --xml --limit 1 https://xfam-svn-hl.ebi.ac.uk/svn/pfam/trunk/Data/Families/$acc`;

  my ($author, $date);
  if ($svn_log =~ /<author>(.*)<\/author>/) {
    $author = $1;
  }
  if ($svn_log =~ /<date>(.*)<\/date>/) {
    my $svndate = $1;
    if ($svndate =~ /(\d+-\d+-\d+)T(\d+:\d+:\d+)/) {
      $date = "$1 $2";
    }
  }
  my $orcid = $curators->{$author};

  if ($orcid) {
    $output .= <<EOL;
        {
            "activity_term": "update_family",
            "curator_orcid": "$orcid",
            "entity_uri": "https://www.ebi.ac.uk/interpro/entry/pfam/$acc",
            "timestamp": "$date"
        },
EOL
  }
}

# Get new families
$rel_sth = $rel_dbh->prepare( "select pfamA_acc from pfamA where change_status = 'NEW' limit 5" );
$rel_sth->execute;

my $new = $rel_sth->fetchall_arrayref;

foreach (@{$new}) {
  my $acc = shift @{$_};

  my $svn_log = `svn log --xml --limit 1 https://xfam-svn-hl.ebi.ac.uk/svn/pfam/trunk/Data/Families/$acc`;

  my ($author, $date);
  if ($svn_log =~ /<author>(.*)<\/author>/) {
    $author = $1;
  }
  if ($svn_log =~ /<date>(.*)<\/date>/) {
    my $svndate = $1;
    if ($svndate =~ /(\d+-\d+-\d+)T(\d+:\d+:\d+)/) {
      $date = "$1 $2";
    }
  }
  my $orcid = $curators->{$author};

  if ($orcid) {
    $output .= <<EOL;
        {
            "activity_term": "create_family",
            "curator_orcid": "$orcid",
            "entity_uri": "https://www.ebi.ac.uk/interpro/entry/pfam/$acc",
            "timestamp": "$date"
        },
EOL
  }
}

# Get deleted families
$rel_sth = $rel_dbh->prepare( "select pfamA_acc AS acc from dead_family where user = '$rel_db' limit 1" );
$rel_sth->execute;

my $killed = $rel_sth->fetchall_arrayref;

foreach (@{$killed}) {
  my $acc = shift @{$_};

  my $live_sth = $live_dbh->prepare( "select pfamA_acc AS acc, user, killed from dead_family where pfamA_acc = '$acc'" );
  $live_sth->execute;

  my $killed_fam = shift @{$live_sth->fetchall_arrayref};

  my $orcid = $curators->{$killed_fam->[1]};
  my $date = $killed_fam->[2];

  if ($orcid) {
    $output .= <<EOL;
        {
            "activity_term": "delete_family",
            "curator_orcid": "$orcid",
            "entity_uri": "https://www.ebi.ac.uk/interpro/entry/pfam/$acc",
            "timestamp": "$date"
        },
EOL
  }
}

# Get updated clans
$rel_sth = $rel_dbh->prepare( "select MIN(updated) - INTERVAL 5 DAY from clan where deposited_by = '$rel_db'" );
$rel_sth->execute;

my $cutoff = shift @{$rel_sth->fetchall_arrayref};
$cutoff = $cutoff->[0];

$rel_sth = $rel_dbh->prepare( "select clan_acc from clan where updated > '$cutoff' and deposited_by != '$rel_db' limit 5" );
$rel_sth->execute;

my $changed_clan = $rel_sth->fetchall_arrayref;

foreach (@{$changed_clan}) {
  my $acc = shift @{$_};

  my $svn_log = `svn log --xml --limit 1 https://xfam-svn-hl.ebi.ac.uk/svn/pfam/trunk/Data/Clans/$acc`;

  my ($author, $date);
  if ($svn_log =~ /<author>(.*)<\/author>/) {
    $author = $1;
  }
  if ($svn_log =~ /<date>(.*)<\/date>/) {
    my $svndate = $1;
    if ($svndate =~ /(\d+-\d+-\d+)T(\d+:\d+:\d+)/) {
      $date = "$1 $2";
    }
  }
  my $orcid = $curators->{$author};

  if ($orcid) {
    $output .= <<EOL;
        {
            "activity_term": "update_clan",
            "curator_orcid": "$orcid",
            "entity_uri": "https://www.ebi.ac.uk/interpro/set/pfam/$acc",
            "timestamp": "$date"
        },
EOL
  }
}

# Get new clans
$rel_sth = $rel_dbh->prepare( "select clan_acc from clan where updated > '$cutoff' and deposited_by = '$rel_db' limit 5" );
$rel_sth->execute;

my $new_clan = $rel_sth->fetchall_arrayref;

foreach (@{$new_clan}) {
  my $acc = shift @{$_};

  my $live_sth = $live_dbh->prepare( "select clan_acc, deposited_by, created from clan where clan_acc = '$acc'" );
  $live_sth->execute;

  my $new_clan = shift @{$live_sth->fetchall_arrayref};

  my $orcid = $curators->{$new_clan->[1]};
  my $date = $new_clan->[2];

  if ($orcid) {
    $output .= <<EOL;
        {
            "activity_term": "create_clan",
            "curator_orcid": "$orcid",
            "entity_uri": "https://www.ebi.ac.uk/interpro/set/pfam/$acc",
            "timestamp": "$date"
        },
EOL
  }
}

# Get deleted clans
$rel_sth = $rel_dbh->prepare( "select clan_acc from dead_clan where user = '$rel_db'" );
$rel_sth->execute;

my $killed_clan = $rel_sth->fetchall_arrayref;

foreach (@{$killed_clan}) {
  my $acc = shift @{$_};

  my $live_sth = $live_dbh->prepare( "select clan_acc, user, killed from dead_clan where clan_acc = '$acc'" );
  $live_sth->execute;

  my $killed_fam = shift @{$live_sth->fetchall_arrayref};

  my $orcid = $curators->{$killed_fam->[1]};
  my $date = $killed_fam->[2];

  if ($orcid) {
    $output .= <<EOL;
        {
            "activity_term": "delete_clan",
            "curator_orcid": "$orcid",
            "entity_uri": "https://www.ebi.ac.uk/interpro/set/pfam/$acc",
            "timestamp": "$date"
        },
EOL
  }
}


# Complete and write output file
$output =~ s/,$//;

$output .= <<EOL;
    ],
    "resource_id": "pfam"
}
EOL

$logger->info("Writing to apicuron_${rel_db}_update.json\n");

print $fh $output;

$logger->info("Done\n");
