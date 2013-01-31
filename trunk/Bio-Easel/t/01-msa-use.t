use strict;
use warnings FATAL => 'all';
use Test::More tests => 10;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

# test new 
my $alnfile = "./t/data/test.sto";
my $msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, "Bio::Easel::MSA");

# test msa
my $msa2 = $msa->msa;
isa_ok($msa2, "ESL_MSA");
# TODO: check that $msa and $msa2 are identical

# test path
my $path = $msa->path;
is($path, "./t/data/test.sto");

# test nseq
my $nseq = $msa->nseq;
is($nseq, 3, "nseq method failed to return correct number");

# test get_sqname
my $sqname = $msa->get_sqname(2);
is($sqname, "orc", "get_sqname method failed to return correct value");

# test set_sqname
$msa->set_sqname(2, "Sauron");
$sqname = $msa->get_sqname(2);
is($sqname, "Sauron", "get_sqname method failed to return correct value following set_sqname() call");

# any_allgap_columns
my $any_gaps = $msa->any_allgap_columns;
is($any_gaps, 0, "any_allgap_columns failed to return correct value");
# TODO add alignment that has >=1 all gap columns and use it here

# test write_msa
my $outfile = "./t/data/test-msa.out";
$msa->write_msa($outfile);

# read it in
my $msa3 = Bio::Easel::MSA->new({
   fileLocation => $outfile,
});
isa_ok($msa3, "Bio::Easel::MSA");
unlink $outfile;

# test nseq
$nseq = $msa3->nseq;
printf("HEY nseq $nseq\n");
is($nseq, 3, "nseq method failed to return correct number (pass 2)");

# FIX DESTROY CALL!
#TODO: test free_msa
# test DESTROY
#$msa->DESTROY;
#$msa2->DESTROY;
#$msa3->DESTROY;
#TODO: test that it was destroyed, don't know how to do that yet

=cut
