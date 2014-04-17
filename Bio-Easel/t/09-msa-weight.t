use strict;
use warnings FATAL => 'all';
use Test::More tests => 3;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

# test new 
my $alnfile = "./t/data/test.sto";
my $msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, "Bio::Easel::MSA");

# test weight_GSC
$msa->weight_GSC;

# test getwgt after calc'ing weights
my $wgt = $msa->get_sqwgt(2);
$wgt = sprintf("%.2f", $wgt);
is($wgt, "0.95");

