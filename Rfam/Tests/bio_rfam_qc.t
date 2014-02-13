use strict;
use warnings;
use Test::More tests => 6;
use Bio::Rfam::FamilyIO;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::QC' )         || print "Failed to load Bio::Rfam::QC!\n";
}

my $dir = $FindBin::Bin;
my $test_data= $dir . '/data';
my $test_family= 'RF00006';

my $familyIO = Bio::Rfam::FamilyIO->new( );
isa_ok($familyIO, 'Bio::Rfam::FamilyIO');
my $family = $familyIO->loadRfamFromLocalFile($test_family, $test_data);

isa_ok($family, 'Bio::Rfam::Family');
is( Bio::Rfam::QC::checkFamilyFormat($family), 0, "Family passes check");



my $lncRNA = 'lncRNAGood';
$family = $familyIO->loadRfamFromLocalFile($lncRNA, $test_data);
is( Bio::Rfam::QC::checkSEEDFormat($family), 0, "Good lncRNA family passes no structure check.");
$lncRNA = 'lncRNABad';
$family = $familyIO->loadRfamFromLocalFile($lncRNA, $test_data);
is( Bio::Rfam::QC::checkSEEDFormat($family), 1, "Bad lncRNA family passes failed structure check.");

