use strict;
use warnings;
use Test::More tests => 2;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::Family::TBLOUT' ) || print "Failed to load Bio::Rfam::Family::TBLOUT!\n";
}

my $dir = $FindBin::Bin;
my $test_file = $dir . '/data/RF00014/TBLOUT';
my $tblout = Bio::Rfam::Family::TBLOUT->new( { fileLocation => $test_file } );
isa_ok($tblout, 'Bio::Rfam::Family::TBLOUT');
