#!/usr/bin/perl -w

use strict;
use warnings;

use Test::More;
use Bio::Pfam::Config;

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::FamilyIO;
use File::Basename;
use Cwd 'abs_path';

BEGIN
{
    use_ok('PfamLive') || print "Failed to load Bio::Pfam::PfamLive!\n";
    use_ok('Bio::Pfam::Wiki::Scraper') || print "Failed to load Bio::Pfam::Wiki::Scraper\n";
    use_ok('Bio::Pfam::Wiki::Updater') || print "Failed to load Bio::Pfam::Wiki::Updater\n";
}

# ------------------------------------------------------------------------------
# db setup
# ------------------------------------------------------------------------------
