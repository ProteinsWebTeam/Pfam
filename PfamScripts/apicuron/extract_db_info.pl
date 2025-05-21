#!/usr/bin/env perl
#
# This will extract the deletion events from the database.
# Those can be manually added to the python script extracted info.
# update $cutoff_date with the desired maximum date for the records

use strict;
use warnings;

# use Cwd;
# use Data::Dumper;
# use Getopt::Long;
# use File::Temp;
# use Try::Tiny;

use Cwd;

use Bio::Pfam::FamilyIO;
use Bio::Pfam::ClanIO;
use Bio::Pfam::PfamLiveDBManager;


use Smart::Comments;

use Getopt::Long;
use Log::Log4perl qw(:easy);
# use File::Path qw(make_path remove_tree);
use Time::HiRes qw(usleep);

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();


# my $cutoff_date = '2024-11-16';


#Get config files
my ($cutoff_date);
GetOptions(
  'cutoff_date=s' => \$cutoff_date
);

if (! $cutoff_date) {
  $logger->info( "You need to enter a cutoff_date on the command line.\n
    E.g. $0 -cutoff_date 2024-11-16\n
    Additionally the live config needs to be sourced\n");
  exit;
}

#Release config
my $release_config = $ENV{'PFAM_CONFIG'};
my $config_rel = Bio::Pfam::Config->new;
my $releaseDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config_rel->pfamlive } );
my $rel_dbh    = $releaseDB->getSchema->storage->dbh;

my $pwd = getcwd;


my $rel_db = $config_rel->pfamlive->{database};
$logger->info("Working on database $rel_db up to $cutoff_date...\n");



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
open (my $fh, '>', "deletion_events.json");

my $output = <<EOL;
{
    "reports": [
EOL


my $rel_sth = $rel_dbh->prepare( "select pfamA_acc AS acc, user, killed from dead_family where killed < '$cutoff_date' UNION select clan_acc AS acc, user, killed from dead_clan where killed < '$cutoff_date'" );
# # $sth->bind_param( 1, $date );
$rel_sth->execute;

my $kills = $rel_sth->fetchall_hashref('acc');
## $kills


foreach my $acc (sort keys %{$kills}) {
  my $item = $kills->{"$acc"};
  if ($curators->{ $kills->{"$acc"}->{"user"} }) {
    my $orcid = $curators->{ $kills->{"$acc"}->{"user"} };
    my $date = $kills->{"$acc"}->{"killed"};
    my $type = 'delete_family';
    my $uri = "https://www.ebi.ac.uk/interpro/entry/pfam/$acc";

    if ($acc =~ /CL/) {
      $type = 'delete_clan';
      $uri = "https://www.ebi.ac.uk/interpro/set/pfam/$acc";
    }

$output .= <<EOL;
        {
            "activity_term": "$type",
            "curator_orcid": "$orcid",
            "entity_uri": "$uri",
            "timestamp": "$date"
        },
EOL

  } else {
    # skip, not in curators list
  }
}


$output =~ s/,$//;

$output .= <<EOL;
    ],
    "resource_id": "pfam"
}
EOL

$logger->info("Writing to deletion_events.json\n");

print $fh $output;

$logger->info("Done\n");
