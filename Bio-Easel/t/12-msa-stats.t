use strict;
use warnings FATAL => 'all';
use Test::More tests => 24;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

###########################################################
# The 'stats' functions only work on digitized alignments #
# so we force digital mode for these tests.               #
###########################################################
my $alnfile         = "./t/data/test.sto";
my $rfamfile_allgap = "./t/data/RF00014-seed-allgap.sto";

my $msa1 = Bio::Easel::MSA->new({
    fileLocation => $alnfile, 
});
isa_ok($msa1, "Bio::Easel::MSA");

my $mis = $msa1->most_informative_sequence(0.5, 0);
is($mis, "-WRSWCUUCGGMWSKSRCV-MMA-BYS-", "calculate_most_informative_sequence() worked.");

my @fcbpA = $msa1->pos_fcbp();
is(int(($fcbpA[2] * 100) + 0.5), 0,   "calculate_pos_fcbp() seems to work (pos 3)");
is(int(($fcbpA[3] * 100) + 0.5), 100, "calculate_pos_fcbp() seems to work (pos 4)");
is(int(($fcbpA[4] * 100) + 0.5), 100, "calculate_pos_fcbp() seems to work (pos 5)");
is(int(($fcbpA[5] * 100) + 0.5), 100, "calculate_pos_fcbp() seems to work (pos 6)");
is(int(($fcbpA[6] * 100) + 0.5), 0,   "calculate_pos_fcbp() seems to work (pos 7)");

my @covA = $msa1->pos_covariation();
is(int(($covA[2] * 100) + 0.5), 0,   "calculate_pos_covariation() seems to work (pos 3)");
is(int(($covA[3] * 100) + 0.5), 133, "calculate_pos_covariation() seems to work (pos 4)");
is(int(($covA[4] * 100) + 0.5), 133, "calculate_pos_covariation() seems to work (pos 5)");
is(int(($covA[5] * 100) + 0.5), 0,   "calculate_pos_covariation() seems to work (pos 6)");
is(int(($covA[6] * 100) + 0.5), 0,   "calculate_pos_covariation() seems to work (pos 7)");

undef $msa1;
  
$msa1 = Bio::Easel::MSA->new({
    fileLocation => $rfamfile_allgap, 
});
isa_ok($msa1, "Bio::Easel::MSA");
my @entA = $msa1->pos_entropy();
is(int(($entA[1] * 100) + 0.5), 200, "calculate_pos_ent() seems to work (pos 2)");
is(int(($entA[2] * 100) + 0.5), 137, "calculate_pos_ent() seems to work (pos 3)");
is(int(($entA[3] * 100) + 0.5), 0,   "calculate_pos_ent() seems to work (pos 4)");
is(int(($entA[4] * 100) + 0.5), 72,  "calculate_pos_ent() seems to work (pos 5)");
is(int(($entA[5] * 100) + 0.5), 0,   "calculate_pos_ent() seems to work (pos 6)");

my @consA = $msa1->pos_conservation();
is(int(($consA[1] * 100) + 0.5),   0, "calculate_pos_conservation() seems to work (pos 2)");
is(int(($consA[2] * 100) + 0.5),  60, "calculate_pos_conservation() seems to work (pos 3)");
is(int(($consA[3] * 100) + 0.5), 100, "calculate_pos_conservation() seems to work (pos 4)");
is(int(($consA[4] * 100) + 0.5),  80, "calculate_pos_conservation() seems to work (pos 5)");
is(int(($consA[31] * 100) + 0.5), 40, "calculate_pos_conservation() seems to work (pos 32)");

