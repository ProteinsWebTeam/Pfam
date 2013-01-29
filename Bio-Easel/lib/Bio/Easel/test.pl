use EslAlign;
use File::Temp qw(tempfile);

#EslAlign::test("in.stk", "out.stk");

my $ea = new EslAlign ($alnfile);

printf("nseq: %d\n", $ea->nseq);

for($i = 0; $i < $ea->nseq; $i++) { 
    # Q, we probably don't want to set sqname[] array for ESL_MSA, it's already 
    # stored in ESL_MSA so $ea->sqname_idx doesn't set a value in $ea object, is
    # that kosher?
    # (actually: do weed need to store nseq...)
    printf("seq $i %s\n", $ea->get_sqname_idx($i));

    $ea->set_sqname_idx($i, "seq" . $i);
    printf("seq $i %s\n", $ea->get_sqname_idx($i));
    printf("\n");
}
# Q: $ea->set_outfile? 

($fh, $filename) = tempfile();
$ea->write_msa($filename);
close($fh);

my $ea2 = new EslAlign ($filename);



