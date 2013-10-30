use strict;
use warnings;
use Test::More tests => 2;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::Config' )         || print "Failed to load Bio::Rfam::Config!\n";
}


my $config = Bio::Rfam::Config->new;

my $dir = $FindBin::Bin;

isa_ok($config, 'Bio::Rfam::Config');


#my $test_data = $dir . '/data/RF00014';

#my $familyIO = Bio::Rfam::FamilyIO->new( );

#my $family = Bio::Rfam::Family->new( { SEED => { aliType => 'seed',
#                                                 fileLocation => $test_data.'/SEED'},
#                                       DESC  => $desc,
#                                       TBLOUT => { fileLocation => $test_data.'/TBLOUT'} });
#isa_ok($family, 'Bio::Rfam::Family');
