#!/usr/bin/env perl
#
# This is a simple cron script to check the list of families deposited in the 
# preceding 24 hours. Any that were added by "untrusted" curators are flagged
# and the list mailed out. Set the list of recipients in the "MAILTO" address 
# in the crontab. The list of *trusted* curators is given in the system wide
# configuration file. See $ENV{PFAM_CONFIG} for the location of that file.
#
# pqc-editors-cron.pl
# jt6 20110606 WTSI
#
# $Id$

use strict;
use warnings;

use DBI;
use Config::General;
use DateTime;
use DateTime::Format::MySQL;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;

$ENV{DBIC_DT_SEARCH_OK}=1; #This is to prevent a warning that is otherwise displayed

# get the configuration
my $config = Bio::Pfam::Config->new;

# get the list of trusted depositors from the config
my %trusted = map { $_ => 1 } @{ $config->{trusted_depositors} };

# connect to the DB and get the schema
my $pfam_live_db_manager = Bio::Pfam::PfamLiveDBManager->new( %{$config->pfamlive} );
my $pfam_live_schema = $pfam_live_db_manager->getSchema;

# get today's date
my $today     = DateTime->now( time_zone => 'local' );
$today->set_formatter( DateTime::Format::MySQL->new );

my $yesterday = $today->clone;
$yesterday->subtract( days => 1 );

# get the list of families that were created in the last 24 hours
my $new_families_rs = $pfam_live_schema->resultset('PfamA')
                                       ->search( { created => { '>=', $yesterday } }, {} );

# check which new families were deposited by untrusted curators
my @untrusted;
while ( my $pfam = $new_families_rs->next ) {
  push @untrusted, $pfam unless $trusted{ $pfam->deposited_by };
}

# if there were no families, we're done
exit unless @untrusted;

# print out the list
print <<EOF;

The following families were added between $yesterday and $today
by untrusted depositors:

accession ID                             depositor     description
EOF

my ( $acc, $id, $desc, $by );
format = 
@<<<<<<   @<<<<<<<<<<<<<<<<<<<<<<<<<<<<< @<<<<<<<<<<<< @*
$acc,     $id,                           $by,          $desc
.

foreach my $pfam ( @untrusted ) {
  $acc  = $pfam->pfama_acc;
  $id   = $pfam->pfama_id;
  $by   = $pfam->deposited_by;
  $desc = $pfam->description;
  write;
}

print "\n";

