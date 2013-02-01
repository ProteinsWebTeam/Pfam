use strict;
use warnings FATAL => 'all';
use Test::More tests => 6;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

# read in file
my $alnfile = "./t/data/RF00014-seed.sto";
my $msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, "Bio::Easel::MSA");

# test nse_create
my $nadded = $msa->nse_createHAA;
is($nadded, 5, "nse_createHAA method failed to return correct value");

my $testname = "M15749.1/140-200";
my ($oname, $ofract) = $msa->nse_overlap($testname);
is($oname, "M15749.1/155-239", "nse_overlap method failed to return correct value");

$testname = $msa->get_sqname(1);
is($testname, "CP000653.1/2739273-2739189", "get_sqname method failed to return correct value");
my $len = $msa->nse_len($testname);
is($len, 85, "nse_len method failed to return correct value");

=cut
