use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

# test new 
my $alnfile = "./t/data/test.sto";
my $msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, "Bio::Easel::MSA");

# test calc_and_write_bp_stats
#TODO: make this into a test somehow
my $outfile = "./t/data/test-msa-bp.out";

$msa->set_accession("RFXXXXX");
$msa->calc_and_write_bp_stats($outfile);

#unlink $outfile