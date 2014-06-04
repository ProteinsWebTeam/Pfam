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

my $config = Bio::Rfam::Config->new;
my $rfamdb = $config->rfamlive;

my $client = Bio::Rfam::SVN::Client->new({config => $config});

my %clans;
my @family_in_clan;
my @overlaps;

my $clan_rs = $rfamdb->resultset('ClanMembership')->search;

while (my $row = $clan_rs->next) {
	my $clan_acc = $row->clan_acc->clan_acc;
	my $rfam_acc = $row->rfam_acc->rfam_acc;
	push @{$clans{$clan_acc}}, $rfam_acc;
	push @family_in_clan, $rfam_acc;
}


for my $clan (keys %clans) {
	my $start_family = shift @{$clans{$clan}};
	print "$start_family\n";
	my $familyIO = Bio::Rfam::FamilyIO->new;
	my $familyObj = $familyIO->loadRfamFromSVN($start_family, $client);
    my $error = Bio::Rfam::QC::findClanOverlaps($familyObj, $rfamdb, $config,  \@{$clans{$clan}});
}
exit;


