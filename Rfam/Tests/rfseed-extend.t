#! /usr/bin/perl
#
# Tests for rfseed-extend.pl
#
# EPN, Mon Dec  2 13:22:05 2013
use strict;
use warnings FATAL => 'all';
use Test::More tests => 14;

BEGIN {
    use_ok( 'Bio::Easel::MSA'    ) || print "Bail out!\n";
    use_ok( 'Bio::Easel::SqFile' ) || print "Bail out!\n";
    use_ok( 'Bio::Rfam::Utils'   ) || print "Bail out!\n";
}

my $datadir   = "./data/rfseed-extend";
my $scriptdir = "../Scripts/make/";

my @unlinkA = ();
my @reqdfilesA = ("SEED", "DESC", "list.txt");

my $testctr = 1;
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
my $dbfile = $datadir . "/testtinydb.fa";
run_script($scriptdir . "/rfseed-extend.pl", "-dbfile $dbfile -5 5 -3 3", \@unlinkA, $testctr++);

# open MSA and do some checks on it
# make sure we have the correct output, by comparing it to what we expect
my $msa = Bio::Easel::MSA->new({
   fileLocation => "SEED",
});
isa_ok($msa, "Bio::Easel::MSA");
my ($alen, $sqname);
# test alignment length
$alen = $msa->alen; 
is($alen, 88, "rfseed-extend: output alignment correct length");
# test all sequence names, this ensures the renaming code after fetching extended sequence works properly
my $i;
my @sqnameA = ();
for($i = 0; $i < $msa->nseq; $i++) { push(@sqnameA, $msa->get_sqname($i)); }
is($sqnameA[0], "AAHX01094554.1/13995-14082", "rfseed-extend: sequence 1 named correctly");
is($sqnameA[1], "Z11765.1/1-85",              "rfseed-extend: sequence 2 named correctly");
is($sqnameA[2], "AF045144.1/3-88",            "rfseed-extend: sequence 3 named correctly");
is($sqnameA[3], "X02584.1/1-80",              "rfseed-extend: sequence 4 named correctly");
is($sqnameA[4], "AAHX01094554.1/14164-14077", "rfseed-extend: sequence 5 named correctly");
is($sqnameA[5], "Z11770.1/94-11",             "rfseed-extend: sequence 6 named correctly");
is($sqnameA[6], "AF045143.1/86-1",            "rfseed-extend: sequence 7 named correctly");
is($sqnameA[7], "K02528.1/77-1",              "rfseed-extend: sequence 8 named correctly");
# test the actual sequences we fetched are correct
my $fetched_seqstring = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@sqnameA, $dbfile, -1, "", undef, 0); # -1: unlimited line len
my $msa_seqstring = "";
for($i = 0; $i < $msa->nseq; $i++) { $msa_seqstring .= ">$sqnameA[$i]\n" . $msa->get_sqstring_unaligned($i) . "\n"; }
$fetched_seqstring =~ tr/T/U/;
is($msa_seqstring, $fetched_seqstring, "rfseed-extend: sequences extended correctly");

clean_up(\@unlinkA);

printf("All commands completed successfully.\n");
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
  my $output = `$script $options`;

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
