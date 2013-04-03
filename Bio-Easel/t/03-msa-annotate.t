use strict;
use warnings FATAL => 'all';
use Test::More tests => 4;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

# test new 
my $alnfile = "./t/data/test.sto";
my $msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, "Bio::Easel::MSA");

# test addGF
#TODO: make this into a test somehow
$msa->addGF("BM", "cmbuild CM SEED");

my $outfile = "./t/data/test-msa-annot.out";

$msa->write_msa($outfile);
unlink $outfile;

# test has_rf
my $has_rf = $msa->has_rf;
is($has_rf, "0");

my $alnfile2 = "./t/data/test.rf.sto";
my $msa2 = Bio::Easel::MSA->new({
   fileLocation => $alnfile2, 
});
$has_rf = $msa2->has_rf;
is($has_rf, "1");

