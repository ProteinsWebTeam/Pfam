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
use XML::Simple;

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

# Format to SVN-compatible: YYYY-MM-DDTHH:MM
my $yesterday_str = format_svn_datetime($yesterday);
my $today_str     = format_svn_datetime($today);


my @modified_full;
foreach my $pfam ( @modified ) {
	my $acc  = $pfam->pfama_acc;
	my $svn_log = `svn log --xml -r {$yesterday_str}:{$today_str} https://xfam-svn-hl.ebi.ac.uk/svn/pfam/trunk/Data/Families/$acc`;

    my $xml = XML::Simple->new;
    my $data = $xml->XMLin($svn_log, ForceArray => ['logentry']);

    my @commits;
	foreach my $entry (@{ $data->{logentry} }) {
	    my $author = $entry->{author} || '';
	    my $msg    = $entry->{msg}    || '';
	    next if $author eq 'xfm_adm';
	    next if $msg =~ /^PFNEW:/;
	    $msg =~ s/\n/ /g;
	    push @commits, { author => $author, msg => $msg };
	}

	push @modified_full, {
		acc => $pfam->pfama_acc,
		id  => $pfam->pfama_id,
		commits => [@commits]
	};
}

# get the list of families that were killed in the last 24 hours
my $killed_families_rs = $pfam_live_schema->resultset('DeadFamily')
                                       ->search( { killed => { '>=', $yesterday } }, {} );

my @killed;
while ( my $pfam = $killed_families_rs->next ) {
  push @killed, $pfam;
}


# if there were no families, we're done
exit unless @untrusted || @modified_full || @killed;


print "\nSummary of family changes between $yesterday and $today\n";
print "Created: " . scalar @untrusted . "\n";
print "Updated: " . scalar @modified_full . "\n";
print "Deleted: " . scalar @killed . "\n";
print "\n";

if (@untrusted) {
	# print out the list
	print "\nThe following families were created:\n\n";

	printf "%-10s   %-30s %-12s %s\n", "Accession", "Identifier", "Author", "Description";

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

      foreach my $commit (@{$pfam->{commits}}) {
      	my $author= $commit->{author};
	  	my $msg  = $commit->{msg};
	  	printf "%-10s   %-30s %-12s %s\n", $acc, $id, $author, $msg;
	  }
	}

	print "\n";
}


if (@killed) {
	# print out the list

	print "\nThe following families were deleted:\n\n";

	printf "%-10s   %-30s %-12s %s\n", "Accession", "Identifier", "Author", "Comment";

	foreach my $pfam ( @killed ) {
	  my $acc  = $pfam->pfama_acc;
	  my $id   = $pfam->pfama_id;
	  my $by   = $pfam->user;
	  my $desc = $pfam->comment;
	  printf "%-10s   %-30s %-12s %s\n", $acc, $id, $by, $desc;
	}

	print "\n";
}


sub format_svn_datetime {
    my ($dt) = @_;
    
    # YYYY-MM-DD
    my $date = $dt->ymd;
    my $hour = sprintf "%02d", $dt->hour;
    my $min  = sprintf "%02d", $dt->minute;
    my $sec  = sprintf "%02d", $dt->second;

    return "${date}T${hour}:${min}:${sec}";
}
