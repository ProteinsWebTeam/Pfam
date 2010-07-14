#!/usr/local/bin/perl

use strict;
use warnings;


use strict;
use warnings;
use Getopt::Long;
use Compress::Zlib;
use File::Copy;
use Cwd;
use LWP::UserAgent;
use Log::Log4perl qw(:easy);

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::PfamJobsDBManager;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
