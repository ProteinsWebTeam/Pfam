use strict;
use warnings;
use Test::More tests => 3;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::Family::MSA' ) || print "Failed to load Bio::Rfam::Family::MSA!\n";
}

my $dir = $FindBin::Bin;
my $test_file = $dir . '/data/test.sto';

my $msa = Bio::Rfam::Family::MSA->new({
    fileLocation => $test_file,
    aliType => 'seed'
});

isa_ok($msa, 'Bio::Rfam::Family::MSA');

is($msa->aliType, 'seed', 'Expecting to see seed type');

