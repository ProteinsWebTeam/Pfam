#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::MGnifam;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new(%{$config->pfamliveAdmin});

unless($pfamDB and $pfamDB->{database} eq "mgnifam") {
  die "Need to use the config for mgnifam\n";
}

my $duf_num = "MGDUF";
$duf_num .= Bio::Pfam::MGnifam::next_MGDUF_number($pfamDB);
print "\nThe next available MGDUF number is: $duf_num\n\n";
