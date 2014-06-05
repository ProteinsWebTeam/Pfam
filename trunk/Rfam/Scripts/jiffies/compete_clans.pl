#!/usr/bin/env perl

use strict;
use warnings;
use Data::Printer;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::View;
use Bio::Rfam::SVN::Client;
use Bio::Rfam::QC;
use Scalar::Util qw(reftype);
use DateTime;

my $config = Bio::Rfam::Config->new;
my $rfamdb = $config->rfamlive;

my $client = Bio::Rfam::SVN::Client->new({config => $config});

my %clans;
my @family_in_clan;
my @overlaps;
my @clans_to_update;


my $dt = DateTime->now;

my $time = join ' ', $dt->ymd, $dt->hms;

print "$time\n";

my $interval = "> now() - interval 1 week";

my $clan_update = $rfamdb->resultset('Clan')->search({updated => \$interval});


while (my $cl = $clan_update->next) {
	push @clans_to_update,  $cl->clan_acc;
}

for my $clan (@clans_to_update) {
	my $clan_rs = $rfamdb->resultset('ClanMembership')->search({clan_acc => $clan});
	while (my $row = $clan_rs->next) {
		my ($clan_acc, $rfam_acc) = ($row->clan_acc->clan_acc, $row->rfam_acc->rfam_acc);
		push @{$clans{$clan_acc}}, $rfam_acc;
	}
}

for my $clan (keys %clans) {
	my $start_family = shift @{$clans{$clan}};
	print "$start_family\n";
	my $familyIO = Bio::Rfam::FamilyIO->new;
	my $familyObj = $familyIO->loadRfamFromSVN($start_family, $client);
    my $error = Bio::Rfam::QC::findClanOverlaps($familyObj, $rfamdb, $config,  \@{$clans{$clan}});
}

exit;




