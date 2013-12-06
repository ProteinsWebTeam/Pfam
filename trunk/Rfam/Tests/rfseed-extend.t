#! /usr/bin/perl
#
# Tests for rfseed-extend.pl
#
# EPN, Mon Dec  2 13:22:05 2013
use strict;
use warnings FATAL => 'all';
use Test::More tests => 158;

BEGIN {
    use_ok( 'Bio::Easel::MSA'    ) || print "Bail out!\n";
    use_ok( 'Bio::Easel::SqFile' ) || print "Bail out!\n";
    use_ok( 'Bio::Rfam::Utils'   ) || print "Bail out!\n";
}

my $datadir   = "./data/rfseed-extend";
my $scriptdir = "../Scripts/make/";

my @unlinkA    = (); # array of files to unlink after each test
my @reqdfilesA = ("SEED", "DESC"); # list of files to copy to current dir before each test

my $testctr  = 0; # counter over script runs
my $dbfile   = $datadir . "/testtinydb.fa"; # the database file
my $listfile = $datadir . "/list.txt";
my $seedfile = $datadir . "/SEED";
my $msa;     # an MSA (Bio::Easel::MSA object)
my $sqname;  # a sequence name
my @sqnameA; # an array of sequence names
my $alistr;  # string of an aligned sequence
my $i;       # counter over sequences
my $fetched_seqstring = ""; # fetched sequences, in a string
my $msa_seqstring = "";     # sequences taken from MSA, in a string

###########################################################################################
# Script test 1: Extending a SEED 5' and 3': 'rfseed-extend.pl -dbfile $dbfile -5 5 -3 3'
###########################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-dbfile $dbfile -5 5 -3 3", \@unlinkA, $testctr);
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "SEED", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 88, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/13995-14082", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/1-85",              "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/3-88",            "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/1-80",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14164-14077", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/94-11",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/86-1",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/77-1",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# test the aligned seqs
$alistr = $msa->get_sqstring_aligned(0); is($alistr, "ACAAGGUUUCAGUGAGAUUCUAACAAUAUUCACAGCUUGGGAAGUCAUCCAUUCAACUCGGUAAUGGCUAAUUAGUGUAUGGUGAGAU", "rfseed-extend test $testctr: sequence 1 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(1); is($alistr, "---GGUCAGCAACAGCUCAGCGGUUACUUCUCGACACGGAAUUGUAAUUCUGAAAACCUUUCGGGGUUCGAAACCCGCGGGCGCCACC", "rfseed-extend test $testctr: sequence 2 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(2); is($alistr, "CUGGCUUUAGCUCAGCGGUUACUUCGAGUACAUUGUAACCACCUCUCUGGGUGGUUCGAGACCCGCGGGUGCUUUCCAGCUCUUUU--", "rfseed-extend test $testctr: sequence 3 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(3); is($alistr, "-----GCCAAGGUGGCAGAGUUCGGCCCAACGCAUCCGCCUGCAGAGCGGAACCCCCGCCGGUUCAAAUCCGGCCCUUGGCUCCA---", "rfseed-extend test $testctr: sequence 4 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(4); is($alistr, "CUACCCAGGUCAUUCCACUUACCACUCCCUAGAGAGUUUACAGAACCUGGAACACAUGAUUAUAAUGGAAACAGCAGAGAUGAUCUCA", "rfseed-extend test $testctr: sequence 5 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(5); is($alistr, "--GUCAGGUGGCGCCCGCGGGUUUCGAACCCCGAAAGGUUUUCAGAAUUACAAUUCCGUGGUAUUUCGAGAAGUAACCGCUGA--GCU", "rfseed-extend test $testctr: sequence 6 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(6); is($alistr, "GAGCG-CCCGCGGGUCUCGAACAACCCAGACAGGUUGCUUGUUUCAAUUAAAGAACUGUCGAAGUAACCGCUGAGCUAAAGCCAGCC-", "rfseed-extend test $testctr: sequence 7 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(7); is($alistr, "-----U--GGUGGGGCCGGAGGGAUUCGAACCCCCGAUCGACUGAUCUGGAGUCAGUCGCCAUGCCGGGAUUGGCCACAGCC--CC--", "rfseed-extend test $testctr: sequence 8 extended into alignment correctly");
clean_up(\@unlinkA);

###########################################################################################
# Script test 2: Extending a SEED 5' only 'rfseed-extend.pl -dbfile $dbfile -5 7'
###########################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-dbfile $dbfile -5 7", \@unlinkA, $testctr);
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "SEED", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 87, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/13993-14079", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/1-82",              "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/1-87",            "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/1-80",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14166-14080", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/94-14",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/88-3",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/77-2",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# test the aligned seqs
$alistr = $msa->get_sqstring_aligned(0); is($alistr, "GGACAAGGUUUCAGUGAGAUUCUAACAAUAUUCACAGCUUGGGAAGUCAUCCAUUCAACUCGGUAAUGGCUAAUUAGUGUAUGGUGA", "rfseed-extend test $testctr: sequence 1 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(1); is($alistr, "-----GGUCAGCAACAGCUCAGCGGUUACUUCUCGACACGGAAUUGUAAUUCUGAAAACCUUUCGGGGUUCGAAACCCGCGGGCGCC", "rfseed-extend test $testctr: sequence 2 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(2); is($alistr, "GGCUGGCUUUAGCUCAGCGGUUACUUCGAGUACAUUGUAACCACCUCUCUGGGUGGUUCGAGACCCGCGGGUGCUUUCCAGCUCUUU", "rfseed-extend test $testctr: sequence 3 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(3); is($alistr, "-------GCCAAGGUGGCAGAGUUCGGCCCAACGCAUCCGCCUGCAGAGCGGAACCCCCGCCGGUUCAAAUCCGGCCCUUGGCUCCA", "rfseed-extend test $testctr: sequence 4 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(4); is($alistr, "ACCUACCCAGGUCAUUCCACUUACCACUCCCUAGAGAGUUUACAGAACCUGGAACACAUGAUUAUAAUGGAAACAGCAGAGAUGAUC", "rfseed-extend test $testctr: sequence 5 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(5); is($alistr, "----GUCAGGUGGCGCCCGCGGGUUUCGAACCCCGAAAGGUUUUCAGAAUUACAAUUCCGUGGUAUUUCGAGAAGUAACCGCUGA--", "rfseed-extend test $testctr: sequence 6 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(6); is($alistr, "GAGAGCG-CCCGCGGGUCUCGAACAACCCAGACAGGUUGCUUGUUUCAAUUAAAGAACUGUCGAAGUAACCGCUGAGCUAAAGCCAG", "rfseed-extend test $testctr: sequence 7 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(7); is($alistr, "-------U--GGUGGGGCCGGAGGGAUUCGAACCCCCGAUCGACUGAUCUGGAGUCAGUCGCCAUGCCGGGAUUGGCCACAGCC--C", "rfseed-extend test $testctr: sequence 8 trimmed into alignment correctly");
clean_up(\@unlinkA);

###########################################################################################
# Script test 3: Extending a SEED 3' only: 'rfseed-extend.pl -dbfile $dbfile -3 6'
###########################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-dbfile $dbfile -3 6", \@unlinkA, $testctr);
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "SEED", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 86, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/14000-14085", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/3-88",              "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/8-88",            "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/1-80",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14159-14074", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/91-8",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/81-1",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/77-1",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# test the aligned seqs
$alistr = $msa->get_sqstring_aligned(0); is($alistr, "GUUUCAGUGAGAUUCUAACAAUAUUCACAGCUUGGGAAGUCAUCCAUUCAACUCGGUAAUGGCUAAUUAGUGUAUGGUGAGAUCAU", "rfseed-extend test $testctr: sequence 1 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(1); is($alistr, "UCAGCAACAGCUCAGCGGUUACUUCUCGACACGGAAUUGUAAUUCUGAAAACCUUUCGGGGUUCGAAACCCGCGGGCGCCACCUGA", "rfseed-extend test $testctr: sequence 2 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(2); is($alistr, "UUUAGCUCAGCGGUUACUUCGAGUACAUUGUAACCACCUCUCUGGGUGGUUCGAGACCCGCGGGUGCUUUCCAGCUCUUUU-----", "rfseed-extend test $testctr: sequence 3 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(3); is($alistr, "GCCAAGGUGGCAGAGUUCGGCCCAACGCAUCCGCCUGCAGAGCGGAACCCCCGCCGGUUCAAAUCCGGCCCUUGGCUCCA------", "rfseed-extend test $testctr: sequence 4 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(4); is($alistr, "CAGGUCAUUCCACUUACCACUCCCUAGAGAGUUUACAGAACCUGGAACACAUGAUUAUAAUGGAAACAGCAGAGAUGAUCUCACCA", "rfseed-extend test $testctr: sequence 5 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(5); is($alistr, "AGGUGGCGCCCGCGGGUUUCGAACCCCGAAAGGUUUUCAGAAUUACAAUUCCGUGGUAUUUCGAGAAGUAACCGCUGA--GCUGUU", "rfseed-extend test $testctr: sequence 6 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(6); is($alistr, "-CCCGCGGGUCUCGAACAACCCAGACAGGUUGCUUGUUUCAAUUAAAGAACUGUCGAAGUAACCGCUGAGCUAAAGCCAGCC----", "rfseed-extend test $testctr: sequence 7 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(7); is($alistr, "U--GGUGGGGCCGGAGGGAUUCGAACCCCCGAUCGACUGAUCUGGAGUCAGUCGCCAUGCCGGGAUUGGCCACAGCC--CC-----", "rfseed-extend test $testctr: sequence 8 extended into alignment correctly");
clean_up(\@unlinkA);

###########################################################################################
# Script test 4: Trimming a SEED 5' and 3' 'rfseed-extend.pl -t -dbfile $dbfile -5 5 -3 3'
###########################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-t -dbfile $dbfile -5 5 -3 3", \@unlinkA, $testctr);
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "SEED", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 72, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/14005-14076", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/8-79",              "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/13-84",           "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/6-77",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14154-14083", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/86-15",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/77-6",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/74-3",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# test the aligned seqs
$alistr = $msa->get_sqstring_aligned(0); is($alistr, "AGUGAGAUUCUAACAAUAUUCACAGCUUGGGAAGUCAUCCAUUCAACUCGGUAAUGGCUAAUUAGUGUAUGG", "rfseed-extend test $testctr: sequence 1 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(1); is($alistr, "AACAGCUCAGCGGUUACUUCUCGACACGGAAUUGUAAUUCUGAAAACCUUUCGGGGUUCGAAACCCGCGGGC", "rfseed-extend test $testctr: sequence 2 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(2); is($alistr, "CUCAGCGGUUACUUCGAGUACAUUGUAACCACCUCUCUGGGUGGUUCGAGACCCGCGGGUGCUUUCCAGCUC", "rfseed-extend test $testctr: sequence 3 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(3); is($alistr, "GGUGGCAGAGUUCGGCCCAACGCAUCCGCCUGCAGAGCGGAACCCCCGCCGGUUCAAAUCCGGCCCUUGGCU", "rfseed-extend test $testctr: sequence 4 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(4); is($alistr, "CAUUCCACUUACCACUCCCUAGAGAGUUUACAGAACCUGGAACACAUGAUUAUAAUGGAAACAGCAGAGAUG", "rfseed-extend test $testctr: sequence 5 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(5); is($alistr, "GCGCCCGCGGGUUUCGAACCCCGAAAGGUUUUCAGAAUUACAAUUCCGUGGUAUUUCGAGAAGUAACCGCUG", "rfseed-extend test $testctr: sequence 6 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(6); is($alistr, "CGGGUCUCGAACAACCCAGACAGGUUGCUUGUUUCAAUUAAAGAACUGUCGAAGUAACCGCUGAGCUAAAGC", "rfseed-extend test $testctr: sequence 7 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(7); is($alistr, "UGGGGCCGGAGGGAUUCGAACCCCCGAUCGACUGAUCUGGAGUCAGUCGCCAUGCCGGGAUUGGCCACAGCC", "rfseed-extend test $testctr: sequence 8 trimmed into alignment correctly");
clean_up(\@unlinkA);

###########################################################################################
# Script test 5: Trimming a SEED 5' only 'rfseed-extend.pl -t -dbfile $dbfile -5 8'
###########################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-t -dbfile $dbfile -5 8", \@unlinkA, $testctr);
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "SEED", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 72, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/14008-14079", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/11-82",             "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/16-87",           "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/9-80",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14151-14080", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/83-14",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/74-3",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/71-2",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# test the aligned seqs
$alistr = $msa->get_sqstring_aligned(0); is($alistr, "GAGAUUCUAACAAUAUUCACAGCUUGGGAAGUCAUCCAUUCAACUCGGUAAUGGCUAAUUAGUGUAUGGUGA", "rfseed-extend test $testctr: sequence 1 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(1); is($alistr, "AGCUCAGCGGUUACUUCUCGACACGGAAUUGUAAUUCUGAAAACCUUUCGGGGUUCGAAACCCGCGGGCGCC", "rfseed-extend test $testctr: sequence 2 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(2); is($alistr, "AGCGGUUACUUCGAGUACAUUGUAACCACCUCUCUGGGUGGUUCGAGACCCGCGGGUGCUUUCCAGCUCUUU", "rfseed-extend test $testctr: sequence 3 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(3); is($alistr, "GGCAGAGUUCGGCCCAACGCAUCCGCCUGCAGAGCGGAACCCCCGCCGGUUCAAAUCCGGCCCUUGGCUCCA", "rfseed-extend test $testctr: sequence 4 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(4); is($alistr, "UCCACUUACCACUCCCUAGAGAGUUUACAGAACCUGGAACACAUGAUUAUAAUGGAAACAGCAGAGAUGAUC", "rfseed-extend test $testctr: sequence 5 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(5); is($alistr, "CCCGCGGGUUUCGAACCCCGAAAGGUUUUCAGAAUUACAAUUCCGUGGUAUUUCGAGAAGUAACCGCUGA--", "rfseed-extend test $testctr: sequence 6 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(6); is($alistr, "GUCUCGAACAACCCAGACAGGUUGCUUGUUUCAAUUAAAGAACUGUCGAAGUAACCGCUGAGCUAAAGCCAG", "rfseed-extend test $testctr: sequence 7 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(7); is($alistr, "GGCCGGAGGGAUUCGAACCCCCGAUCGACUGAUCUGGAGUCAGUCGCCAUGCCGGGAUUGGCCACAGCC--C", "rfseed-extend test $testctr: sequence 8 trimmed into alignment correctly");
clean_up(\@unlinkA);

###########################################################################################
# Script test 6: Trimming a SEED 3' only 'rfseed-extend.pl -t -dbfile $dbfile -3 1'
###########################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-t -dbfile $dbfile -3 1", \@unlinkA, $testctr);
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "SEED", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 79, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/14000-14078", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/3-81",              "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/8-86",            "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/1-79",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14159-14081", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/91-14",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/81-4",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/77-3",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# test the aligned seqs
$alistr = $msa->get_sqstring_aligned(0); is($alistr, "GUUUCAGUGAGAUUCUAACAAUAUUCACAGCUUGGGAAGUCAUCCAUUCAACUCGGUAAUGGCUAAUUAGUGUAUGGUG", "rfseed-extend test $testctr: sequence 1 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(1); is($alistr, "UCAGCAACAGCUCAGCGGUUACUUCUCGACACGGAAUUGUAAUUCUGAAAACCUUUCGGGGUUCGAAACCCGCGGGCGC", "rfseed-extend test $testctr: sequence 2 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(2); is($alistr, "UUUAGCUCAGCGGUUACUUCGAGUACAUUGUAACCACCUCUCUGGGUGGUUCGAGACCCGCGGGUGCUUUCCAGCUCUU", "rfseed-extend test $testctr: sequence 3 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(3); is($alistr, "GCCAAGGUGGCAGAGUUCGGCCCAACGCAUCCGCCUGCAGAGCGGAACCCCCGCCGGUUCAAAUCCGGCCCUUGGCUCC", "rfseed-extend test $testctr: sequence 4 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(4); is($alistr, "CAGGUCAUUCCACUUACCACUCCCUAGAGAGUUUACAGAACCUGGAACACAUGAUUAUAAUGGAAACAGCAGAGAUGAU", "rfseed-extend test $testctr: sequence 5 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(5); is($alistr, "AGGUGGCGCCCGCGGGUUUCGAACCCCGAAAGGUUUUCAGAAUUACAAUUCCGUGGUAUUUCGAGAAGUAACCGCUGA-", "rfseed-extend test $testctr: sequence 6 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(6); is($alistr, "-CCCGCGGGUCUCGAACAACCCAGACAGGUUGCUUGUUUCAAUUAAAGAACUGUCGAAGUAACCGCUGAGCUAAAGCCA", "rfseed-extend test $testctr: sequence 7 trimmed into alignment correctly");
$alistr = $msa->get_sqstring_aligned(7); is($alistr, "U--GGUGGGGCCGGAGGGAUUCGAACCCCCGAUCGACUGAUCUGGAGUCAGUCGCCAUGCCGGGAUUGGCCACAGCC--", "rfseed-extend test $testctr: sequence 8 trimmed into alignment correctly");
clean_up(\@unlinkA);

######################################################################################################################
# Script test 7: -l option to specify a subset of sequences  'rfseed-extend.pl -l list.txt -dbfile $dbfile -5 5 -3 3'
######################################################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-l $listfile -dbfile $dbfile -5 5 -3 3", \@unlinkA, $testctr);
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "SEED", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 88, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/14000-14079", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/1-85",              "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/8-87",            "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/1-80",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14164-14077", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/91-14",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/81-3",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/77-2",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# test the aligned seqs
$alistr = $msa->get_sqstring_aligned(0); is($alistr, "-----GUUUCAGUGAGAUUCUAACAAUAUUCACAGCUUGGGAAGUCAUCCAUUCAACUCGGUAAUGGCUAAUUAGUGUAUGGUGA---", "rfseed-extend test $testctr: sequence 1 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(1); is($alistr, "---GGUCAGCAACAGCUCAGCGGUUACUUCUCGACACGGAAUUGUAAUUCUGAAAACCUUUCGGGGUUCGAAACCCGCGGGCGCCACC", "rfseed-extend test $testctr: sequence 2 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(2); is($alistr, "-----UUUAGCUCAGCGGUUACUUCGAGUACAUUGUAACCACCUCUCUGGGUGGUUCGAGACCCGCGGGUGCUUUCCAGCUCUUU---", "rfseed-extend test $testctr: sequence 3 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(3); is($alistr, "-----GCCAAGGUGGCAGAGUUCGGCCCAACGCAUCCGCCUGCAGAGCGGAACCCCCGCCGGUUCAAAUCCGGCCCUUGGCUCCA---", "rfseed-extend test $testctr: sequence 4 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(4); is($alistr, "CUACCCAGGUCAUUCCACUUACCACUCCCUAGAGAGUUUACAGAACCUGGAACACAUGAUUAUAAUGGAAACAGCAGAGAUGAUCUCA", "rfseed-extend test $testctr: sequence 5 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(5); is($alistr, "-----AGGUGGCGCCCGCGGGUUUCGAACCCCGAAAGGUUUUCAGAAUUACAAUUCCGUGGUAUUUCGAGAAGUAACCGCUGA-----", "rfseed-extend test $testctr: sequence 6 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(6); is($alistr, "------CCCGCGGGUCUCGAACAACCCAGACAGGUUGCUUGUUUCAAUUAAAGAACUGUCGAAGUAACCGCUGAGCUAAAGCCAG---", "rfseed-extend test $testctr: sequence 7 extended into alignment correctly");
$alistr = $msa->get_sqstring_aligned(7); is($alistr, "-----U--GGUGGGGCCGGAGGGAUUCGAACCCCCGAUCGACUGAUCUGGAGUCAGUCGCCAUGCCGGGAUUGGCCACAGCC--C---", "rfseed-extend test $testctr: sequence 8 extended into alignment correctly");
clean_up(\@unlinkA);

######################################################################################################################
# Script test 8: -i option to specify input seed file name 'rfseed-extend.pl -i $seedfile -dbfile $dbfile -5 5 -3 3'
######################################################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-i $seedfile -dbfile $dbfile -5 5 -3 3", \@unlinkA, $testctr);
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "SEED", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 88, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/13995-14082", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/1-85",              "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/3-88",            "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/1-80",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14164-14077", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/94-11",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/86-1",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/77-1",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# don't bother testing aligned seqs, we already did that in test 1
clean_up(\@unlinkA);

######################################################################################################################
# Script test 9: -o option to specify output seed file name 'rfseed-extend.pl -o test.seed -dbfile $dbfile -5 5 -3 3'
######################################################################################################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfseed-extend.pl", "-o test.seed -dbfile $dbfile -5 5 -3 3", \@unlinkA, $testctr);
push(@unlinkA, "test.seed");
# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
$msa = Bio::Easel::MSA->new({ fileLocation => "test.seed", });
isa_ok($msa, "Bio::Easel::MSA");
# test alignment length
is($msa->alen, 88, "rfseed-extend test $testctr: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
@sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/13995-14082", "rfseed-extend test $testctr: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/1-85",              "rfseed-extend test $testctr: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/3-88",            "rfseed-extend test $testctr: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/1-80",              "rfseed-extend test $testctr: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14164-14077", "rfseed-extend test $testctr: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/94-11",             "rfseed-extend test $testctr: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/86-1",            "rfseed-extend test $testctr: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/77-1",              "rfseed-extend test $testctr: sequence 8 named correctly");
# test the actual sequences we fetched are correct (match with freshly fetched seqs)
$fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
$msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend test $testctr: sequences extended with correct residues");
# don't bother testing aligned seqs, we already did that in test 1
clean_up(\@unlinkA);

exit 0;

###############
# SUBROUTINES #
###############
sub run_command {
  if(scalar(@_) != 1) { die "ERROR run_command entered with wrong number of input args"; }
  my ($cmd) = (@_);
  system($cmd);
  if($? != 0) { die "ERROR command $cmd failed"; }
  return;
}
###############
sub clean_up {
  if(scalar(@_) != 1) { die "ERROR clean_up entered with wrong number of input args"; }
  my ($unlinkAR) = (@_);
  foreach my $file (@{$unlinkAR}) { 
    if(-e $file) { unlink $file; }
    if(-e $file) { die "ERROR, unable to unlink $file"; }
  }
  return;
}
###############
sub copy_orig_files {
  if(scalar(@_) != 2) { die "ERROR copy_orig_files entered with wrong number of input args"; }
  my ($datadir, $fileAR) = (@_);
  foreach my $file (@{$fileAR}) { 
    if(! -e $datadir . "/" . $file) { die "ERROR, unable to copy required file $file from $datadir"; }
    run_command("cp $datadir/$file .");
  }
  return;
}
###############
sub run_script { 
  if(scalar(@_) != 4) { die "ERROR run_script entered with wrong number of input args"; }
  my ($script, $options, $unlinkAR, $testctr) = (@_);
  run_command("$script $options");

  # parse output, find files we output and add them to unlinkAR
  my $logfile = $script;
  $logfile =~ s/^.+\///;
  $logfile =~ s/\.pl/\.log/;
  my @newfilesA = parse_log_for_output_files($logfile);
  push(@{$unlinkAR}, @newfilesA);

  return;
}
###############
sub parse_log_for_output_files {
  if(scalar(@_) != 1) { die "ERROR parse_log_for_output_files entered with wrong number of input args"; }
  my ($logfile) = (@_);
  
  my @newfileA = ();

  open(LOG, $logfile) || die "ERROR unable to open $logfile";
  my $found_outfiles = 0;
  my $line;
  while($line = <LOG>) { 
    if($line =~ m/^\# file name\s+description\s*\n$/) { 
      $found_outfiles = 1;
      $line = <LOG>; # next line is just ====== ======= line
      $line = <LOG>;
      while($line !~ m/^\#/) { 
        chomp $line;
        $line =~ s/^\s+//;
        $line =~ s/\s+.*$//;
        push(@newfileA, $line);
        $line = <LOG>;
      }
    }
  }
  if(! $found_outfiles) { die "ERROR, unable to find output file list in $logfile"; }

  return (@newfileA);
}
###############
