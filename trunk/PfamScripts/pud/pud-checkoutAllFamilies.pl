#!/usr/bin/env perl

use strict;
use Bio::Pfam::SVN::Client;
use Bio::Pfam::Config;
use Cwd;

my $dir = $ARGV[0] || getcwd;
die qq(Unable to write to directory $dir) unless -d $dir && -w $dir;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $client = Bio::Pfam::SVN::Client->new;
$client->checkoutAllFamilies($dir);
