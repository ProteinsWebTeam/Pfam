use strict;
use warnings FATAL => 'all';
use Test::More tests => 33;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

my ($alnfile, $line, $msa, $outfile, $has_rf, $alnfile2, $has_gc, $test_gc, $foo_gc, $tagidx, $tag, $tagnum);
my @gcA  = ();
my @gcA2 = ();
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

  # test addGC, getGC_given_tag and hasGC
  @gcA  = ("1","1","1","1","1", "2","2","2","2","2",  "3","3","3","3","3",  "4","4","4","4","4",  "5","5","5","5","5",   "6","6","6");
  @gcA2 = ("7","7","7","7","7", "2","2","2","2","2",  "3","3","3","3","3",  "4","4","4","4","4",  "J","J","J","J","J",   "6","6","6");

  $has_gc = $msa->hasGC("SS_cons");
  is($has_gc, "1", "hasGC correctly notes presence of SS_cons annotation");

  $has_gc = $msa->hasGC("PP_cons");
  is($has_gc, "0", "hasGC correctly notes absence of PP_cons annotation");

  $has_gc = $msa->hasGC("test");
  is($has_gc, "0", "hasGC correctly notes absence of GC annotation");

  $msa->addGC("test", \@gcA);

  $has_gc = $msa->hasGC("test");
  is($has_gc, "1", "hasGC correctly notes presence of GC annotation");

  $msa->addGC("foo", \@gcA2);

  $has_gc = $msa->hasGC("foo");
  is($has_gc, "1", "hasGC correctly notes presence of GC annotation");

  $test_gc = $msa->getGC_given_tag("test");
  is($test_gc, "1111122222333334444455555666", "addGC and getGC_given_tag correctly add and get GC annotation");

  $foo_gc = $msa->getGC_given_tag("foo");
  is($foo_gc, "77777222223333344444JJJJJ666", "addGC and getGC_given_tag correctly add and get GC annotation");

  # test getGC_tagidx, getGC_tag, getGC_given_idx
  $tagidx = $msa->getGC_tagidx("test");
  is($tagidx, "0", "getGC_tagidx seems to work");

  $tagidx = $msa->getGC_tagidx("foo");
  is($tagidx, "1", "getGC_tagidx seems to work");

  $tagnum = $msa->getGC_number();  
  is($tagnum, "2", "getGC_number seems to work");

  $tag = $msa->getGC_tag(1);  
  is($tag, "foo", "getGC_tag seems to work");

  $foo_gc = $msa->getGC_given_idx(1);
  is($foo_gc, "77777222223333344444JJJJJ666", "getGC_given_idx correctly gets GC annotation");
  

}
