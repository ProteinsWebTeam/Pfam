use strict;
use warnings;
use Test::More tests => 1;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::QC' )         || print "Failed to load Bio::Rfam::QC!\n";
}

my $dir = $FindBin::Bin;
#my $test_data = $dir . '/data/RF00014';

#my $familyIO = Bio::Rfam::FamilyIO->new( );

#my $family = Bio::Rfam::Family->new( { SEED => { aliType => 'seed',
#                                                 fileLocation => $test_data.'/SEED'},
#                                       DESC  => $desc,
#                                       TBLOUT => { fileLocation => $test_data.'/TBLOUT'} });
#isa_ok($family, 'Bio::Rfam::Family');
