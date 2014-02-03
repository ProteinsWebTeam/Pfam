use strict;
use warnings FATAL => 'all';
use Test::More tests => 7;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print 'Bail out!\n';
}

my $alnfile = './t/data/test.sto';

my ($msa, $name);

# run all tests twice, once in digital and once in text mode
for(my $mode = 0; $mode <= 1; $mode++) { 
  # read in file
  $msa = Bio::Easel::MSA->new({
    fileLocation => $alnfile, 
    forceText    => $mode,
  });
  isa_ok($msa, 'Bio::Easel::MSA');

  # change sequence name, and check it's set properly
  $msa->set_sqname(2, 'Sauron');
  $name = $msa->get_sqname(2);
  is($name, 'Sauron', "set sqname set name appropriately (mode $mode)");

  # revert to original, and make sure sequence name reverts too
  $msa->revert_to_original;
  $name = $msa->get_sqname(2);
  is($name, 'orc', "revert_to_original reverted sq names (mode $mode)");
}
