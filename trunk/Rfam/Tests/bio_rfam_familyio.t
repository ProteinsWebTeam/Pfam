use strict;
use warnings;
use Test::More tests => 3;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::FamilyIO' ) || print "Failed to load Bio::Rfam::FamilyIO!\n";
}

my $dir = $FindBin::Bin;
my $test_data= $dir . '/data';
my $test_family= 'RF00014';

my $familyIO = Bio::Rfam::FamilyIO->new( );
isa_ok($familyIO, 'Bio::Rfam::FamilyIO');

my $family = $familyIO->loadRfamFromLocalFile($test_family, $test_data);
isa_ok($family, 'Bio::Rfam::Family');
