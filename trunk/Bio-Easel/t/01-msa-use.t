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

# check nseq returns 3
my $nseq = $msa->nseq();
is($nseq, 3, "nseq method failed to return correct number");

# check alen returns 28
my $alen = $msa->alen();
is($alen, 28, "alen method failed to return correct number $alen != 28");

=for comment
printf("nseq: %d\n", $msa->nseq);

#for($i = 0; $i < $ea->nseq; $i++) { 
    # Q, we probably don't want to set sqname[] array for ESL_MSA, it's already 
    # stored in ESL_MSA so $ea->sqname doesn't set a value in $ea object, is
    # that kosher?
    # (actually: do weed need to store nseq...)
#    printf("seq $i %s\n", $ea->get_sqname($i));

#    $ea->set_sqname($i, "seq" . $i);
#    printf("seq $i %s\n", $ea->get_sqname($i));
#    printf("\n");
#}
# Q: $ea->set_outfile? 

#($fh, $filename) = tempfile();
#$ea->write_msa($filename);
#close($fh);

#my $ea2 = new EslAlign ($filename);
=cut
