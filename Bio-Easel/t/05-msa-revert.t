use strict;
use warnings FATAL => 'all';
use Test::More tests => 4;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print 'Bail out!\n';
}

# read in file
my $alnfile = './t/data/test.sto';
my $msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, 'Bio::Easel::MSA');

# change sequence name, and check it's set properly
$msa->set_sqname(2, 'Sauron');
my $name = $msa->get_sqname(2);
is($name, 'Sauron', 'set sqname should set name appropriately');

# revert to original, and make sure sequence name reverts too
$msa->revert_to_original;
$name = $msa->get_sqname(2);
is($name, 'orc', 'revert_to_original should revert sq names');

