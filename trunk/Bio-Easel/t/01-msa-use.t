use strict;
use warnings FATAL => 'all';
use Test::More tests => 135;

BEGIN {
    use_ok( 'Bio::Easel::MSA' ) || print "Bail out!\n";
}

#####################################################################
# We do all tests twice, once reading the MSA read in digital mode, #
# and again reading the MSA in text mode - that's what the big for  #
# loop is for.                                                      #
#####################################################################
my $alnfile     = "./t/data/test.sto";
my $rf_alnfile  = "./t/data/test.rf.sto";
my $rf_alnfile2 = "./t/data/test2.rf.sto";
my $gap_alnfile = "./t/data/test-gap.sto";
my ($msa1, $msa2);
my ($path, $nseq, $sqname, $sqidx, $any_gaps, $len, $avglen, $outfile, $id, $checksum, $format, $is_digitized);
my ($line, $line1, $line2, $mode, $msa_str, $trash);
my ($sub_nseq, $sub_alen, $isres, $alen);
my ($avg_pid, $min_pid, $min_idx, $max_pid, $max_idx);
my @keepmeA;
my @usemeA;
my @orderA;

# first test new without a forcetext value
$msa1 = Bio::Easel::MSA->new({
    fileLocation => $alnfile, 
});
isa_ok($msa1, "Bio::Easel::MSA");
undef $msa1;

# now do all tests with in both digital (mode == 0) and text (mode == 1) modes
for($mode = 0; $mode <= 1; $mode++) { 
  # test new 
  $msa1 = Bio::Easel::MSA->new({
      fileLocation => $alnfile, 
      forceText    => $mode,
  });
  isa_ok($msa1, "Bio::Easel::MSA");

  # test new with required stockholm format
  undef $msa1;
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     reqdFormat   => "stockholm",
     forceText    => $mode,
  });
  isa_ok($msa1, "Bio::Easel::MSA");

  # test msa 
  $msa2 = $msa1->msa;
  isa_ok($msa2, "ESL_MSA");
  # TODO: check that $msa1 and $msa2 are identical

  # test format
  $format = $msa1->format;
  is($format, "Stockholm", "format accessor worked (mode $mode).");

  # test is_digitized;
  $is_digitized = $msa1->is_digitized;
  if($mode == 0) { # digitized
    is($is_digitized, 1, "is_digitized returned correct value (mode $mode)");
  }   
  else { # text 
    is($is_digitized, 0, "is_digitized returned correct value (mode $mode)");
  }   

  # test path
  $path = $msa1->path;
  is($path, "./t/data/test.sto", "path accessor worked (mode $mode).");

  # test nseq
  $nseq = $msa1->nseq;
  is($nseq, 3, "nseq method returned correct number (mode $mode).");

  # test checksum
  $checksum = $msa1->checksum;
  # digitized checksum differs from text checksum:
  if($mode == 0) { # digitized
    is($checksum, "3414500222", "checksum returned correct value (mode $mode)");
  }   
  else { # text 
    is($checksum, "679166796", "checksum returned correct value (mode $mode)");
  }   

  # test get_sqname
  $sqname = $msa1->get_sqname(2);
  is($sqname, "orc", "get_sqname method returned correct value (mode $mode).");

  # test get_sqidx
  $sqidx = $msa1->get_sqidx("mouse");
  is($sqidx, "1", "get_sqidx method returned correct value for valid sequence (mode $mode).");
  $sqidx = $msa1->get_sqidx("non-existent");
  is($sqidx, "-1", "get_sqidx method returned correct value for invalid sequence (mode $mode).");

  # test set_sqname
  $msa1->set_sqname(2, "Sauron");
  $sqname = $msa1->get_sqname(2);
  is($sqname, "Sauron", "get_sqname method returned correct value following set_sqname() call (mode $mode).");

  # any_allgap_columns
  $any_gaps = $msa1->any_allgap_columns;
  is($any_gaps, 0, "any_allgap_columns returned correct value (mode $mode)");
  undef $msa2;
  $msa2 = Bio::Easel::MSA->new({
      fileLocation => $gap_alnfile,
      forceText    => $mode,
  });
  isa_ok($msa2, "Bio::Easel::MSA");
  $any_gaps = $msa2->any_allgap_columns;
  is($any_gaps, 1, "any_allgap_columns returned correct value (mode $mode)");

  # average_id
  $avg_pid = $msa1->average_id(100);
  $avg_pid = int(($avg_pid * 100) + 0.5);
  is($avg_pid, 48, "average_id function worked (mode $mode).");

  # get_sqlen
  $len = $msa1->get_sqlen(0);
  is($len, 24, "get_sqlen returned correct value (mode $mode)");

  $avglen = $msa1->average_sqlen();
  $avglen = int($avglen + 0.5);
  is($avglen, 25, "average_sqlen returned correct value (mode $mode)");

  # test addGC_identity
  $msa1->addGC_identity(1); # '1' says: indicated identical columns with conserved residue, not a '*'

  # test write_msa
  $outfile = "./t/data/test-msa.out";
  $msa1->write_msa($outfile);

  # read it in
  undef $msa1;
  $msa1 = Bio::Easel::MSA->new({
    fileLocation => $outfile,
    forceText    => $mode,
  });
  isa_ok($msa2, "Bio::Easel::MSA");

  #make sure ID annotation correctly set by addGC_identity
  open(IN, $outfile);
  $trash = <IN>;
  $trash = <IN>;
  $trash = <IN>;
  $trash = <IN>;
  $trash = <IN>;
  $trash = <IN>;
  $id    = <IN>;
  chomp $id;
  is($id, "#=GC ID      .....CUUC.G......C....A.....", "addGC_identity method properly calculated and/or set ID annotation (mode $mode)");
  #unlink $outfile;

  # test nseq
  $nseq = $msa1->nseq;
  is($nseq, 3, "nseq method returned correct number (pass 2) (mode $mode)");

  # test write_msa with afa format
  $outfile = "./t/data/test-msa-afa.out";
  $msa1->write_msa($outfile, "afa");
  open(IN, $outfile);
  $line1 = <IN>;
  $line2 = <IN>;
  close(IN);
  is($line1, ">human\n", "write_msa() output AFA properly (mode $mode)");
  # digitized seq differs from text seq:
  if($mode == 0) { # digitized
    is($line2, "-AAGACUUCGGAUCUGGCG-ACA-CCC-\n", "write_msa() output AFA properly (mode $mode)");
  }   
  else { # text 
    is($line2, ".AAGACUUCGGAUCUGGCG.ACA.CCC.\n", "write_msa() output AFA properly (mode $mode)");
  }   
  unlink $outfile;

  # test write_msa with unaligned fasta format
  $outfile = "./t/data/test-msa-fa.out";
  $msa1->write_msa($outfile, "fasta");
  open(IN, $outfile);
  $line1 = <IN>;
  $line2 = <IN>;
  close(IN);
  is($line1, ">human\n", "write_msa() output fasta (mode $mode)");
  is($line2, "AAGACUUCGGAUCUGGCGACACCC\n", "write_msa() output fasta (mode $mode)");
  unlink $outfile;

  # test write_single_unaligned_seq
  $outfile = "./t/data/test-msa-fa.out";
  $msa1->write_single_unaligned_seq(0, $outfile);
  open(IN, $outfile);
  $line1 = <IN>;
  $line2 = <IN>;
  close(IN);
  is($line1, ">human\n", "write_single_unaligned_seq() output fasta (mode $mode)");
  is($line2, "AAGACUUCGGAUCUGGCGACACCC\n", "write_single_unaligned_seq() output correctly (mode $mode)");
  unlink $outfile;

  #######################################################
  # test functions that replace msa with a new ESL_MSA:
  # sequence_subset()
  # 
  undef $msa1;
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     forceText    => $mode,
  });
  isa_ok($msa1, "Bio::Easel::MSA");
  @keepmeA = (1, 0, 1);
  undef $msa2;
  $msa2 = $msa1->sequence_subset(\@keepmeA);
  isa_ok($msa1, "Bio::Easel::MSA");
  $sub_nseq = $msa2->nseq;
  is($sub_nseq, "2", "sequence_subset worked (mode $mode)");

  # write it out
  $outfile = "./t/data/test-msa-fa.out";
  $msa2->write_msa($outfile, "stockholm");
  open(IN, $outfile);
  $line1 = <IN>;
  $line1 = <IN>;
  $line1 = <IN>;
  $line1 = <IN>;
  close(IN);
  # digitized and text seqs differ
  if($mode == 0) { # digitized
    is($line1, "orc          -AGGUCUUC-GCACGGGCAGCCACUUC-\n", "sequence_subset worked (mode $mode)");
  }
  else { 
    is($line1, "orc          .AGGUCUUC-GCACGGGCAgCCAcUUC.\n", "sequence_subset worked (mode $mode)");
  }
  unlink $outfile;

  # now test remove_all_gap_columns
  $msa2->remove_all_gap_columns(0);
  $sub_alen = $msa2->alen;
  is($sub_alen, "26", "remove_all_gap_columns worked");
  $msa2->write_msa($outfile, "stockholm");
  open(IN, $outfile);
  $line1 = <IN>;
  $line1 = <IN>;
  $line1 = <IN>;
  $line1 = <IN>;
  close(IN);
  # digitized and text seqs differ
  if($mode == 0) { # digitized
    is($line1, "orc          AGGUCUUC-GCACGGGCAGCCACUUC\n", "remove_all_gap_columns worked (mode $mode)");
  }   
  else { 
    is($line1, "orc          AGGUCUUC-GCACGGGCAgCCAcUUC\n", "remove_all_gap_columns worked (mode $mode)");
  }
  unlink $outfile;

  # test clone and column_subset
  undef $msa1;
  $msa1 = $msa2->clone_msa();
  isa_ok($msa1, "Bio::Easel::MSA");
  @usemeA = (1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0);
  $msa1->column_subset(\@usemeA);
  $msa1->write_msa($outfile, "stockholm");
  open(IN, $outfile);
  $line1 = <IN>;
  $line1 = <IN>;
  $line1 = <IN>;
  $line1 = <IN>;
  close(IN);
  # digitized and text seqs differ
  if($mode == 0) { # digitized
    is($line1, "orc          AGCU-CCGCGCCU\n", "column_subset worked (mode $mode)");
  }
  else { 
    is($line1, "orc          AGCU-CCGCgCcU\n", "column_subset worked (mode $mode)");
  }
  unlink $outfile;

  # test remove_rf_gap_columns
  undef $msa1;
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $rf_alnfile, 
     forceText    => $mode,
  });
  isa_ok($msa1, "Bio::Easel::MSA");
  $msa1->remove_rf_gap_columns();
  $sub_alen = $msa1->alen;
  is($sub_alen, "24", "remove_rf_gap_columns worked (mode $mode)");

  $msa1->revert_to_original();
  $alen = $msa1->alen;
  is($alen, "28", "revert_to_original() worked (mode $mode)");

  isa_ok($msa1, "Bio::Easel::MSA");
  $msa1->remove_rf_gap_columns("~-");
  # no gaps should be removed
  $sub_alen = $msa1->alen;
  is($sub_alen, "28", "remove_rf_gap_columns worked (mode $mode)");

  ###############################
  # is_residue
  $isres = $msa1->is_residue(2, 9);
  is($isres, "1", "is_residue worked (mode $mode)");

  $isres = $msa1->is_residue(2, 10);
  is($isres, "0", "is_residue worked (mode $mode)");


  #######################################################
  # avg_min_max_pid_to_seq
  undef $msa1;
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     forceText    => $mode,
  });
  ($avg_pid, $min_pid, $min_idx, $max_pid, $max_idx) = $msa1->avg_min_max_pid_to_seq(1);
  # round pids
  $avg_pid = int(($avg_pid * 100) + 0.5);
  $min_pid = int(($min_pid * 100) + 0.5);
  $max_pid = int(($max_pid * 100) + 0.5);
  is($avg_pid, 43, "avg_min_max_pid_to_seq calculated avg_pid correctly (mode $mode)");
  is($min_pid, 32, "avg_min_max_pid_to_seq calculated min_pid correctly (mode $mode)");
  is($min_idx, 2,  "avg_min_max_pid_to_seq calculated min_idx correctly (mode $mode)");
  is($max_pid, 54, "avg_min_max_pid_to_seq calculated max_pid correctly (mode $mode)");
  is($max_idx, 0,  "avg_min_max_pid_to_seq calculated max_idx correctly (mode $mode)");

  @usemeA = ('1', '1', '0');
  ($avg_pid, $min_pid, $min_idx, $max_pid, $max_idx) = $msa1->avg_min_max_pid_to_seq(1, \@usemeA);
  # round pids
  $avg_pid = int(($avg_pid * 100) + 0.5);
  $min_pid = int(($min_pid * 100) + 0.5);
  $max_pid = int(($max_pid * 100) + 0.5);
  is($avg_pid, 54, "avg_min_max_pid_to_seq calculated avg_pid correctly (mode $mode)");
  is($min_pid, 54, "avg_min_max_pid_to_seq calculated min_pid correctly (mode $mode)");
  is($min_idx, 0,  "avg_min_max_pid_to_seq calculated min_idx correctly (mode $mode)");
  is($max_pid, 54, "avg_min_max_pid_to_seq calculated max_pid correctly (mode $mode)");
  is($max_idx, 0,  "avg_min_max_pid_to_seq calculated max_idx correctly (mode $mode)");

  #######################################################
  # create_from_string
  undef $msa1;
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     forceText    => $mode,
  });
  $outfile = "./t/data/test-msa.out";
  $msa1->write_msa($outfile);
  open(IN, $outfile) || die "ERROR, unable to open $outfile for reading";
  $msa_str = "";
  while($line = <IN>) { $msa_str .= $line; }
  close(IN);
  undef $msa1;
  my $do_digitize = ($mode == 0) ? 1 : 0;
  $msa1 = Bio::Easel::MSA::create_from_string($msa_str, undef, "rna", $do_digitize);
  isa_ok($msa1, "Bio::Easel::MSA");
  is($nseq, 3, "create_from_string method worked (mode $mode)");
  $nseq = $msa1->nseq;
  unlink $outfile;

  # try again with afa format
  $msa1->write_msa($outfile, "afa");
  open(IN, $outfile) || die "ERROR, unable to open $outfile for reading";
  $msa_str = "";
  while($line = <IN>) { $msa_str .= $line; }
  close(IN);
  undef $msa1;
  $msa1 = Bio::Easel::MSA::create_from_string($msa_str, "afa", "dna", $do_digitize);
  isa_ok($msa1, "Bio::Easel::MSA");
  $nseq = $msa1->nseq;
  is($nseq, 3, "create_from_string method worked (mode $mode)");
  unlink $outfile;

  ################################################
  # reorder_all
  undef $msa1;
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     forceText    => $mode,
  });
  @orderA = ();
  $orderA[0] = $msa1->get_sqname(2);
  $orderA[1] = $msa1->get_sqname(0);
  $orderA[2] = $msa1->get_sqname(1);
  $msa1->reorder_all(\@orderA);

  # write it out 
  $msa1->write_msa($outfile);
  undef $msa1;

  # read it back in
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $outfile,
     forceText    => $mode,
  });
  is($orderA[0], $msa1->get_sqname(0), "reorder_msa seems to be working (mode: $mode)");
  is($orderA[1], $msa1->get_sqname(1), "reorder_msa seems to be working (mode: $mode)");
  is($orderA[2], $msa1->get_sqname(2), "reorder_msa seems to be working (mode: $mode)");
  undef $msa1;
  unlink $outfile;

  ################################################
  # sequence_subset_and_reorder
  undef $msa1;
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     forceText    => $mode,
  });
  @orderA = ();
  $orderA[0] = $msa1->get_sqname(1);
  $orderA[1] = $msa1->get_sqname(0);
  undef $msa2;
  $msa2 = $msa1->sequence_subset_and_reorder(\@orderA);
  undef $msa1;

  # write it out 
  $msa2->write_msa($outfile);
  undef $msa2;

  # read it back in
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $outfile,
     forceText    => $mode,
  });
  is($msa1->nseq(), 2, "sequence_subset_and_reorder() seems to be working");
  is($orderA[0], $msa1->get_sqname(0), "sequence_subset_and_reorder() seems to be working (mode: $mode)");
  is($orderA[1], $msa1->get_sqname(1), "sequence_subset_and_reorder() seems to be working (mode: $mode)");
  undef $msa1;
  unlink $outfile;

  ################################################
  # get_column
  $msa1 = Bio::Easel::MSA->new({
     fileLocation => $alnfile, 
     forceText    => $mode,
  });
  my $col = $msa1->get_column(20);
  if($mode == 0) { # digital mode
    is($col, "--G", "get_column() seems to be working");
  }   
  else { # text mode 
    is($col, "..g", "get_column() seems to be working");
  }   

  ################################################
  # set_rf
  my $rfstr = ".abcdefghijklmnopqr.stu.vwx.";
  $msa1->set_rf($rfstr);
  is($msa1->get_rf(), $rfstr, "set_rf() seems to be working");

  ################################################
  # set_ss_cons
  my $ss_cons_str = "<<<<<<<<<<<<<<<>>>>>>>>>>>>>";
  $msa1->set_ss_cons($ss_cons_str);
  is($msa1->get_ss_cons(), $ss_cons_str, "set_ss_cons() seems to be working");

  ################################################
  # capitalize_based_on_rf
  if($mode == 1) { # text mode
    undef $msa1;
    $msa1 = Bio::Easel::MSA->new({
      fileLocation => $rf_alnfile2,
      forceText    => $mode,
    });
    $msa1->capitalize_based_on_rf();
    $outfile = "./t/data/test-msa.out";
    $msa1->write_msa($outfile);
    undef $msa1;

    # read in the alignment
    open(IN, $outfile);
    $line = <IN>;
    $line = <IN>;
    $line = <IN>; chomp $line;
    is($line, "human        .AAGACUUCGGAUCUGGCG.ACA.CCC.", "capitalize_based_on_rf() seems to be working");
    $line = <IN>; chomp $line;
    is($line, "mouse        aUACACUUCGGAUG-CACC.AAA.GUGa", "capitalize_based_on_rf() seems to be working");
    $line = <IN>; chomp $line;
    is($line, "orc          .AGGUCUUC-GCACGGGCAgCCAcUUC.", "capitalize_based_on_rf() seems to be working");
    close(IN);
    unlink $outfile;
  }
}

