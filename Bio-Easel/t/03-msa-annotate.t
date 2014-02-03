use strict;
use warnings FATAL => 'all';
use Test::More tests => 9;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

my ($alnfile, $line, $msa, $outfile, $has_rf, $alnfile2);
$alnfile = "./t/data/test.sto";

# do all tests twice, once in digital and once in text mode
for(my $mode = 0; $mode <= 1; $mode++) { 
   my $msa = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     forceText    => $mode,
   });
   isa_ok($msa, "Bio::Easel::MSA");

   # test addGF
   $msa->addGF("BM", "cmbuild CM SEED");
   $outfile = "./t/data/test-msa-annot.out";

   $msa->write_msa($outfile);

   open(IN, $outfile) || die "ERROR unable to open $outfile";
   $line = <IN>;
   $line = <IN>;
   chomp $line;
   is($line, "#=GF BM cmbuild CM SEED", "addGF properly added GF annotation (mode $mode)");
   unlink $outfile;

   # test has_rf
   $has_rf = $msa->has_rf;
   is($has_rf, "0", "has_rf correctly noted absence of RF annotation (mode $mode)");
   undef $msa;

   $alnfile2 = "./t/data/test.rf.sto";
   $msa = Bio::Easel::MSA->new({
     fileLocation => $alnfile2, 
     forceText    => $mode,
   });

   $has_rf = $msa->has_rf;
   is($has_rf, "1", "has_rf correctly detected RF annotation (mode $mode)");
}
