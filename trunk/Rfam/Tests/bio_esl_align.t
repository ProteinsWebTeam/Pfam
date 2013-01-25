use strict;
use warnings;
use Test::More tests => 3;

BEGIN {
  use_ok( 'Bio::Rfam::Family::MSA' ) || print "Failed to load Bio::Rfam::Family::MSA!\n";
}

my $msa = Bio::Rfam::Family::MSA->new({
    fileLocation => '/Users/clementsj/scratch/test.sto',
    type => 'seed'
});

isa_ok($msa, 'Bio::Rfam::Family::MSA');

is($msa->type, 'seed', 'Expecting to see seed type');

