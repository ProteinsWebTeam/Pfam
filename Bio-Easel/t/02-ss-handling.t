use strict;
use warnings FATAL => 'all';
use Test::More tests => 18;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

my ($alnfile, $msa, $has_ss_cons, $ss_cons, $acc, $name);
$alnfile = "./t/data/test.sto";

# do all tests twice, once in digital and once in text mode
for(my $mode = 0; $mode <= 1; $mode++) { 
  $msa = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     forceText    => $mode,
  });
  isa_ok($msa, "Bio::Easel::MSA");

  # test has_ss_cons
  $has_ss_cons = $msa->has_ss_cons;
  is($has_ss_cons, 1);

  # test get_ss_cons
  $ss_cons = $msa->get_ss_cons;
  is($ss_cons, ".::<<<____>->>:<<-<.___.>>>.");
  undef $msa;

  # test set_blank_ss_cons 
  # read in new copy of $alnfile into $msa2
  $msa = Bio::Easel::MSA->new({
       fileLocation => $alnfile, 
       forceText    => $mode,
  });
  isa_ok($msa, "Bio::Easel::MSA");
  $msa->set_blank_ss_cons;

  # test get_ss_cons
  $ss_cons = $msa->get_ss_cons;
  is($ss_cons, "............................");

  # test set_accession
  $acc = "RFXXXXX";
  $msa->set_accession($acc);
  is($msa->get_accession, $acc);

  # test set_name
  $name = "myRNA";
  $msa->set_name($name);
  is($msa->get_name, $name);
  undef $msa;
}

# test rfam_qc_stats, only in digital mode (it only works in digital mode)
$msa = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
});

$msa->set_name("testfamily");

# test rfam_qc_stats()
my $famout  = "./t/data/ss-stats-perfamily";
my $seqout  = "./t/data/ss-stats-persequence";
my $pairout = "./t/data/ss-stats-perbasepair";

$msa->rfam_qc_stats($famout, $seqout, $pairout);

# read in files and make sure they are as expected
# ss-stats-perfamily file
open(IN, $famout) || die "ERROR unable to open $famout";
my $line = <IN>;
$line = <IN>;
chomp $line;
is($line, "testfamily                              1.00000      1.22222        3          28       6       74     0.482    0.583    0.320    24.667       25       24        0.881   0.257   0.311   0.243   0.189  M:0.568         0.554", "rfam_qc_stats() properly calculated ss-stats-perfamily file");
close(IN);

# ss-stats-persequence file
open(IN, $seqout) || die "ERROR unable to open $famout";
$line = <IN>;
$line = <IN>;
chomp $line;
is($line, "testfamily            human                                        1.00000     24   0.250   0.333   0.250   0.167  M:0.583         0.583", "rfam_qc_stats() properly calculated ss-stats-persequence file");
close(IN);

# ss-stats-perbasepair file
open(IN, $pairout) || die "ERROR unable to open $famout";
$line = <IN>;
$line = <IN>;
chomp $line;
is($line, "testfamily                4:14                   1.0000       1.3333", "rfam_qc_stats() properly calculated ss-stats-perbasepair file");
close(IN);

undef $msa;

unlink $famout;
unlink $seqout;
unlink $pairout;


