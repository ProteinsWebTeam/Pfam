use strict;
use warnings FATAL => 'all';
use Test::More tests => 13;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

###########################################################
# The 'stats' functions only work on digitized alignments #
# so we force digital mode for these tests.               #
###########################################################
my $alnfile     = "./t/data/test.sto";

# first test new without a forcetext value
my $msa1 = Bio::Easel::MSA->new({
    fileLocation => $alnfile, 
});
isa_ok($msa1, "Bio::Easel::MSA");

my $mis = $msa1->calculate_most_informative_sequence(0);
is($mis, "-WRSWCUUCGGMWSKSRCV-MMA-BYS-", "calculate_most_informative_sequence() worked.");

my @fcbpA = $msa1->calculate_pos_fcbp();
is(int(($fcbpA[2] * 100) + 0.5), 0,   "calculate_pos_fcbp() seems to work (pos 2)");
is(int(($fcbpA[3] * 100) + 0.5), 100, "calculate_pos_fcbp() seems to work (pos 3)");
is(int(($fcbpA[4] * 100) + 0.5), 100, "calculate_pos_fcbp() seems to work (pos 4)");
is(int(($fcbpA[5] * 100) + 0.5), 100, "calculate_pos_fcbp() seems to work (pos 5)");
is(int(($fcbpA[6] * 100) + 0.5), 0,   "calculate_pos_fcbp() seems to work (pos 6)");


my @covA = $msa1->calculate_pos_covariation();
is(int(($covA[2] * 100) + 0.5), 0,   "calculate_pos_covariation() seems to work (pos 2)");
is(int(($covA[3] * 100) + 0.5), 133, "calculate_pos_covariation() seems to work (pos 3)");
is(int(($covA[4] * 100) + 0.5), 133, "calculate_pos_covariation() seems to work (pos 4)");
is(int(($covA[5] * 100) + 0.5), 0,   "calculate_pos_covariation() seems to work (pos 5)");
is(int(($covA[6] * 100) + 0.5), 0,   "calculate_pos_covariation() seems to work (pos 6)");

  
