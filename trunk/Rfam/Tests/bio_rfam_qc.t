use strict;
use warnings;
use Test::More tests => 4;
use Bio::Rfam::FamilyIO;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::QC' )         || print "Failed to load Bio::Rfam::QC!\n";
}

my $dir = $FindBin::Bin;
my $test_data= $dir . '/data';
my $test_family= 'RF00014';

my $familyIO = Bio::Rfam::FamilyIO->new( );
isa_ok($familyIO, 'Bio::Rfam::FamilyIO');
my $family = $familyIO->loadRfamFromLocalFile($test_family, $test_data);

isa_ok($family, 'Bio::Rfam::Family');
ok( Bio::Rfam::QC::checkFamilyFormat($family), "Family passes check");
