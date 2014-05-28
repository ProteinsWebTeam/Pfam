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

my $in_cmfile1 = "../../Tests/data/RF00014/CM"; 
my $in_cmfile2 = "../../Tests/data/RF00006/CM"; 

my $familyIO = Bio::Rfam::FamilyIO->new( );

# read in CM files
my $cm1 = $familyIO->parseCM( $in_cmfile1 );
my $cm2 = $familyIO->parseCM( $in_cmfile2 );

# examples of changing all values that we can currently change: NAME, ACC, DESC, GA, TC, NC
$cm1->setName       ("TestFamily"); 
$cm1->setAccession  ("RF99989");
$cm1->setDescription("A test family");
$cm1->setGA         ("43");
$cm1->setTC         ("44");
$cm1->setNC         ("42");

# writing the CM to an output file
my $outfile1 = "RF00014.cm";
$familyIO->writeCM($cm1, $outfile1);

# writing two CMs to the same output file
my $outfile2 = "double.cm";
$familyIO->writeCM($cm1, $outfile2);
$familyIO->writeCM($cm2, $outfile2, 1); # the '1' says: append this CM to the existing file

# unlink $outfile1;
# unlink $outfile2;

exit 0;

