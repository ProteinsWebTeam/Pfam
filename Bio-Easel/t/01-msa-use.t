use strict;
use warnings FATAL => 'all';
use Test::More tests => 32;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

# test new 
my $alnfile = "./t/data/test.sto";
my $msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, "Bio::Easel::MSA");

# test new with required stockholm format
my $omsa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
   reqdFormat   => "stockholm",
});
isa_ok($omsa, "Bio::Easel::MSA");

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

# average_id
my $avgpid = $msa->average_id(100);
# TODO figure out how to check if a float is close to a predicted value
#print STDERR "average pid $avgpid\n";

# get_sqlen
my $len = $msa->get_sqlen(0);
is($len, 24, "get_sqlen failed to return correct value");

#TODO make this into a test:
my $avglen = $msa->average_sqlen();
#is($len, 24, "average_sqlen failed to return correct value");

# test addGC_identity
$msa->addGC_identity(1); # '1' says: indicated identical columns with conserved residue, not a '*'

# test write_msa
my $outfile = "./t/data/test-msa.out";
$msa->write_msa($outfile);

# read it in
my $msa3 = Bio::Easel::MSA->new({
   fileLocation => $outfile,
});
isa_ok($msa3, "Bio::Easel::MSA");

#make sure ID annotation correctly set by addGC_identity
open(IN, $outfile);
my $trash = <IN>;
$trash = <IN>;
$trash = <IN>;
$trash = <IN>;
$trash = <IN>;
$trash = <IN>;
my $id    = <IN>;
chomp $id;
is($id, "#=GC ID      .....CUUC.G......C....A.....", "addGC_identity method failed to properly calculate and/or set ID annotation");
unlink $outfile;

# test nseq
$nseq = $msa3->nseq;
is($nseq, 3, "nseq method failed to return correct number (pass 2)");

# test write_msa with afa format
$outfile = "./t/data/test-msa-afa.out";
$msa3->write_msa($outfile, "afa");
open(IN, $outfile);
my $line1 = <IN>;
my $line2 = <IN>;
close(IN);
is($line1, ">human\n", "write_msa() failed to output AFA");
is($line2, "-AAGACUUCGGAUCUGGCG-ACA-CCC-\n", "write_msa() failed to output AFA");
unlink $outfile;

# test write_msa with unaligned fasta format
$outfile = "./t/data/test-msa-fa.out";
$msa3->write_msa($outfile, "fasta");
open(IN, $outfile);
$line1 = <IN>;
$line2 = <IN>;
close(IN);
is($line1, ">human\n", "write_msa() failed to output fasta");
is($line2, "AAGACUUCGGAUCUGGCGACACCC\n", "write_msa() failed to output fasta");
unlink $outfile;

# test write_single_unaligned_seq
$outfile = "./t/data/test-msa-fa.out";
$msa3->write_single_unaligned_seq(0, $outfile);
open(IN, $outfile);
$line1 = <IN>;
$line2 = <IN>;
close(IN);
is($line1, ">human\n", "write_single_unaligned_seq() failed to output fasta");
is($line2, "AAGACUUCGGAUCUGGCGACACCC\n", "write_single_unaligned_seq() failed to output correctly");
unlink $outfile;

#######################################################
# test functions that replace msa with a new ESL_MSA:
# sequence_subset()
# 
$msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, "Bio::Easel::MSA");
my @keepmeA = (1, 0, 1);
my $new_msa = $msa->sequence_subset(\@keepmeA);
isa_ok($new_msa, "Bio::Easel::MSA");
my $sub_nseq = $new_msa->nseq;
is($sub_nseq, "2", "sequence_subset failed to work");

# write it out
$outfile = "./t/data/test-msa-fa.out";
$new_msa->write_msa($outfile, "stockholm");
open(IN, $outfile);
$line1 = <IN>;
$line1 = <IN>;
$line1 = <IN>;
$line1 = <IN>;
close(IN);
is($line1, "orc          -AGGUCUUC-GCACGGGCAGCCACUUC-\n", "sequence_subset failed");
unlink $outfile;

# now test remove_all_gap_columns
$new_msa->remove_all_gap_columns(0);
my $sub_alen = $new_msa->alen;
is($sub_alen, "26", "remove_all_gap_columns failed to work");
$new_msa->write_msa($outfile, "stockholm");
open(IN, $outfile);
$line1 = <IN>;
$line1 = <IN>;
$line1 = <IN>;
$line1 = <IN>;
close(IN);
is($line1, "orc          AGGUCUUC-GCACGGGCAGCCACUUC\n", "remove_all_gap_columns failed");
unlink $outfile;

# test clone and column_subset
my $newer_msa = $new_msa->clone_msa();
isa_ok($newer_msa, "Bio::Easel::MSA");
my @usemeA = (1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0);
$newer_msa->column_subset(\@usemeA);
$newer_msa->write_msa($outfile, "stockholm");
open(IN, $outfile);
$line1 = <IN>;
$line1 = <IN>;
$line1 = <IN>;
$line1 = <IN>;
close(IN);
is($line1, "orc          AGCU-CCGCGCCU\n", "column_subset failed");
unlink $outfile;

# test remove_rf_gap_columns
$alnfile = "./t/data/test.rf.sto";
$msa = Bio::Easel::MSA->new({
   fileLocation => $alnfile, 
});
isa_ok($msa, "Bio::Easel::MSA");
$msa->remove_rf_gap_columns();
$sub_alen = $msa->alen;
is($sub_alen, "24", "remove_rf_gap_columns failed to work");

$msa->revert_to_original();
my $alen = $msa->alen;
is($alen, "28", "revert_to_original() failed to work");

isa_ok($msa, "Bio::Easel::MSA");
$msa->remove_rf_gap_columns("~-");
# no gaps should be removed
$sub_alen = $msa->alen;
is($sub_alen, "28", "remove_rf_gap_columns failed to work");

#######################################################

# FIX DESTROY CALL!
#TODO: test free_msa
# test DESTROY
#$msa->DESTROY;
#$msa2->DESTROY;
#$msa3->DESTROY;
#TODO: test that it was destroyed, don't know how to do that yet

