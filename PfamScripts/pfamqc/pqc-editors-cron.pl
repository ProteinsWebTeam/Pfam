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



# get the list of families that were updated in the last 24 hours
my $updated_families_rs = $pfam_live_schema->resultset('PfamA')
                                       ->search( { updated => { '>=', $yesterday },
                                       'me.updated' => { '!=' => \'me.created' }, }, {} );

my @modified;
while ( my $pfam = $updated_families_rs->next ) {
  push @modified, $pfam;
}

my @modified_full;
foreach my $pfam ( @modified ) {
	my $acc  = $pfam->pfama_acc;

	my $svn_log = `svn log --xml --limit 1 https://xfam-svn-hl.ebi.ac.uk/svn/pfam/trunk/Data/Families/$acc`;

	my ($author, $msg);
	if ($svn_log =~ /<author>(.*)<\/author>/) {
		$author = $1;
	}
	if ($svn_log =~ /<msg>(.*)<\/msg>/s) {
		$msg = $1;
		$msg =~ s/\n/ /g;
	}
	push @modified_full, {
		acc => $pfam->pfama_acc,
		id  => $pfam->pfama_id,
		author => $author,
		msg => $msg
	};
}



# if there were no families, we're done
exit unless @untrusted && @modified_full;

print "\n" . scalar @untrusted . " families were added and " . scalar @modified_full . " families were updated between $yesterday and $today.\n";

if (@untrusted) {
	# print out the list
	print "\nThe following families were added:\n\n";

	printf "%-10s   %-30s %-12s %s\n", "Accession", "Identifier", "Depositor", "Description";

	foreach my $pfam ( @untrusted ) {
	  my $acc  = $pfam->pfama_acc;
	  my $id   = $pfam->pfama_id;
	  my $by   = $pfam->deposited_by;
	  my $desc = $pfam->description;
	  printf "%-10s   %-30s %-12s %s\n", $acc, $id, $by, $desc;
	}

	print "\n";

}

if (@modified_full) {
	# print out the list

	print "\nThe following families were updated:\n\n";

	printf "%-10s   %-30s %-12s %s\n", "Accession", "Identifier", "Author", "Message";

	foreach my $pfam ( @modified_full ) {
	  my $acc  = $pfam->{acc};
	  my $id   = $pfam->{id};
	  my $author= $pfam->{author};
	  my $msg  = $pfam->{msg};
	  printf "%-10s   %-30s %-12s %s\n", $acc, $id, $author, $msg;
	}

	print "\n";
}
