#!/usr/bin/env perl
# 
# example-writeCM.pl: simple script to demonstrate how to use the Bio::Rfam::FamilyIO::writeCM() method.
#
# NOTE: This script must be run from Rfam/Scripts/jiffies inside a checkout of the Rfam code repository,
#       since it includes hardcoded paths to test CM files.
# 
use strict;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::CM;
use Bio::Rfam::Config;
use Bio::Rfam::SVN::Client;

my $config = Bio::Rfam::Config->new;
my $client = Bio::Rfam::SVN::Client->new({config => $config});
my $family = $ARGV[0];
my $CMFILE = "$family.CM";

my $familyIO = Bio::Rfam::FamilyIO->new;
my $familyObj = $familyIO->loadRfamFromSVN($family, $client);

$familyIO->writeAnnotatedCM($familyObj, $CMFILE ,0);

