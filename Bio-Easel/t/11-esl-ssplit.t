#! /usr/bin/perl
#
# Tests for esl-ssplit.pl: a script that uses BioEasel to split up a fasta file
# into smaller files.
#
# EPN, Thu Jan 16 09:49:03 2014
use strict;
use warnings FATAL => 'all';
use Test::More tests => 14;

BEGIN {
  use_ok( 'Bio::Easel::SqFile' ) || print "Bail out!\n";
}

my $datadir   = "./t/data/esl-ssplit";
my $scriptdir = "./scripts/";
my $miniappdir = "./src/easel/miniapps/";

# first run three main modes with three different input files.
my @arg1A     = ("rna-10Kb.fa", "rna-1Mb.fa", "aa-10k.fa"); # the three test files, command line argument for test 1
my @arg2A     = ("3",           "7",          "100");       # command line argument for each test file
my @nfiles1A  = ("7",           "22",         "101");       # number of files generated for each test file for default options
my @nfiles2A  = ("3",           "7",          "100");       # number of files generated for each test file for -n option
my @nfiles3A  = ("3",           "7",          "100");       # number of files generated for each test file for -n and -r option
my $ntestfiles = 3;

my @unlinkA    = ();     # array of files to unlink after each test
my @reqdfilesA = @arg1A; # list of files to copy to current dir 
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);

for(my $f = 0; $f < $ntestfiles; $f++) { 
  my $arg1 = $arg1A[$f];
  my $arg2 = $arg2A[$f];

  # test default parameters 
  run_command($scriptdir . "/esl-ssplit.pl $arg1 $arg2");
  # test the output by concatenating, reformatting and diff'ing against original file
  my $diff = concatenate_reformat_and_diff($miniappdir, $arg1, $arg1, $nfiles1A[$f]);
  is($diff, "", "esl-ssplit $arg1 split correctly with default parameters");

  # test -n
  run_command($scriptdir . "/esl-ssplit.pl -n $arg1 $arg2");
  $diff = concatenate_reformat_and_diff($miniappdir, $arg1, $arg1, $nfiles2A[$f]);
  is($diff, "", "esl-ssplit $arg1 split correctly with -n option");

  # test -n and -r 
  run_command($scriptdir . "/esl-ssplit.pl -n -r $arg1 $arg2");
  $diff = concatenate_reformat_and_diff($miniappdir, $arg1, $arg1, $nfiles3A[$f]);
  is($diff, "", "esl-ssplit $arg1 split correctly with -n and -r options");
}

# now test other options: -d -oroot and -odir
my $arg1 = $arg1A[0];
my $arg2 = $arg2A[0];

# test -d
run_command($scriptdir . "/esl-ssplit.pl -d $arg1 $arg2");
my $diff = concatenate_reformat_and_diff($miniappdir, $arg1, $arg1, $nfiles1A[0]);
is($diff, "", "esl-ssplit $arg1 split correctly with -d");
my $ssi_exists = (-e $arg1.".ssi") ? 1 : 0;
is($ssi_exists, 1, "esl-ssplit -d correctly leaves .ssi file with -d option.");
push(@unlinkA, $arg1.".ssi");

# test -oroot
my $oroot = "root";
run_command($scriptdir . "/esl-ssplit.pl -oroot $oroot $arg1 $arg2");
$diff = concatenate_reformat_and_diff($miniappdir, $arg1, $oroot, $nfiles1A[0]);
is($diff, "", "esl-ssplit $arg1 split correctly with -oroot $oroot option");

# test -odir
my $odir = $datadir . "/";
run_command($scriptdir . "/esl-ssplit.pl -odir $odir $arg1 $arg2");
$diff = concatenate_reformat_and_diff($miniappdir, $arg1, $odir . $arg1, $nfiles1A[0]);
is($diff, "", "esl-ssplit $arg1 split correctly with -odir $odir option");

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
sub concatenate_reformat_and_diff { 
  if(scalar(@_) != 4) { die "ERROR concatenate_reformat_and_diff entered with wrong number of input args"; }
  my ($miniappdir, $origfile, $smallfileroot, $nfiles) = (@_);

  my $testfile    = "test.fa";
  my $rf_testfile = "test.rf.fa";
  
  my @unlinkA = ();

  my $cmd = "cat ";
  for(my $i = 1; $i <= $nfiles; $i++) { 
    $cmd .= " $smallfileroot.$i";
    push(@unlinkA, "$smallfileroot.$i");
  }
  $cmd .= " > $testfile";
  run_command($cmd);

  # reformat with esl-reformat
  $cmd = $miniappdir . "esl-reformat fasta $testfile > $rf_testfile";
  run_command($cmd);

  # compare with diff
  my $diff_file = "$testfile.diff";
  $cmd = "diff $origfile $rf_testfile > $diff_file";
  run_command($cmd);
  if(! -e $diff_file) { die "ERROR diff output file $diff_file was not created"; }
  if(  -s $diff_file) { die "ERROR diff output file is not empty, script failed to correctly split file"; }
  # read in $diff_file and return it's text (should be "")
  open(IN, $diff_file) || die "ERROR unable to open $diff_file";
  my $diff_output = "";
  while(my $line = <IN>) { 
    $diff_output .= $line;
  }

  push(@unlinkA, ($diff_file, $testfile, $rf_testfile));
  clean_up(\@unlinkA);
  
  return $diff_output;
}
###############
