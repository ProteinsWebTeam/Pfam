#!/usr/bin/env perl
#
# This will submit curation events to apicuron
# ORCID env for the curator must be set, i.e. 'export ORCID=0000-0000-0000-0000'
# submit_apicuron.pl <pfam_acc> <curation_event>, i.e. submit_apicuron.pl PF12345 update_family
# On Success will print 'Success'

use strict;
use warnings;
use Bio::Pfam::Config;
use HTTP::Request;
use LWP::UserAgent;
use POSIX qw(strftime);


if(scalar(@ARGV) != 2) { 
	print "Usage: submit_apicuron.pl <pfam_acc> <curation_event>\n";
	exit; 
}
my ($acc, $event) = @ARGV;

my $orcid = $ENV{ORCID};
if (!$orcid) {
	print "ORCID env is not set! Please set with \"export ORCID=0000-0000-0000-0000\"\n";
	exit; 
}


my $event_map = {
	'create_family' => 'https://www.ebi.ac.uk/interpro/entry/pfam/',
	'update_family' => 'https://www.ebi.ac.uk/interpro/entry/pfam/',
	'delete_family' => 'https://www.ebi.ac.uk/interpro/entry/pfam/',
	'create_clan'   => 'https://www.ebi.ac.uk/interpro/set/pfam/',
	'update_clan'   => 'https://www.ebi.ac.uk/interpro/set/pfam/',
	'delete_clan'   => 'https://www.ebi.ac.uk/interpro/set/pfam/',
};
my $uri = $event_map->{$event} . $acc;

my $date = strftime "%Y-%m-%d %H:%M:%S", localtime;

my $config = Bio::Pfam::Config->new;

my $header = [
  'version' => '2',
  'authorization' => $config->apicuronKey,
  'Content-Type' => 'application/json; charset=UTF-8'
];

my $report = <<EOL;
{
    "resource_id": "pfam",
    "activity_term": "$event",
    "curator_orcid": "$orcid",
    "entity_uri": "$uri",
    "timestamp": "$date"
}
EOL

my $r = HTTP::Request->new('POST', 'https://apicuron.org/api/reports/single', $header, $report);

my $ua = LWP::UserAgent->new();
my $res = $ua->request($r);

if ($res->is_success) {
	print "Success";
} else {
	print "Apicuron request failed: " . $res->status_line;
}
