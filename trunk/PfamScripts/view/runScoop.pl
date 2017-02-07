#!/usr/bin/env perl

use strict;
use warnings;
use Bio::Pfam::ViewProcess::Scoop;


my $scoopView = Bio::Pfam::ViewProcess::Scoop->new;
$scoopView->runScoop();

