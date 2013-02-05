#!/usr/bin/env perl

use strict;
use warnings;
use Data::Printer;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::View;

my $family = 'RF00014';
my $uuid   = 'D0A7A25A-6BF9-11E2-95E1-6A292FD990EF';
# get the configuration
my $config = Bio::Rfam::Config->new;


# get a list of the plugins in the "trees" set
my $plugins = $config->viewPluginSets('rfamseq');

p($plugins);

my $view = Bio::Rfam::View->new ( { 
  plugins   => $plugins,
  config    => $config,
  family    => $family,
  job_uuid  => $uuid,
  seqdb     => 'rfamseq'
} );

$view->startJob;

foreach my $plugin ( @{ $view->plugin_list } ) {
  $plugin->process;
}

print "done.\n";

