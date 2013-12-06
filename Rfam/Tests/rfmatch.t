#! /usr/bin/perl
#
# Tests for rfmatch.pl
#
# EPN, Thu Dec  5 14:28:27 2013
use strict;
use warnings FATAL => 'all';
use Test::More tests => 20;

BEGIN {
#    use_ok( 'Bio::Easel::MSA'    ) || print "Bail out!\n";
#    use_ok( 'Bio::Easel::SqFile' ) || print "Bail out!\n";
#    use_ok( 'Bio::Rfam::Utils'   ) || print "Bail out!\n";
}

my $datadir   = "./data/rfmatch";
my $scriptdir = "../Scripts/make/";

my @unlinkA    = (); # array of files to unlink after each test
my @reqdfilesA = ("SEED", "DESC", "CM", "outlist", "species"); # list of files to copy to current dir before each test

my $testctr   = 0; # counter over script runs
my $matchfile = $datadir . "/tinymatch.fa"; # the database file
my ($oline, $oname, $opid);
my ($sline, $sname, $spid);

###################################
# Script test 1: Default parameters
###################################
$testctr++;
@unlinkA = ();
copy_orig_files($datadir, \@reqdfilesA);
push(@unlinkA, @reqdfilesA);
# run the script
run_script($scriptdir . "/rfmatch.pl", "-f $matchfile test > /dev/null", "rfmatch.test.log", \@unlinkA, $testctr);
# verify that test.outlist and test.species are correct
open(OUTLIST, "test.outlist") || die "ERROR unable to open test.outlist";
open(SPECIES, "test.species") || die "ERROR unable to open test.species";

# chew up comment lines
$oline = <OUTLIST>;
$sline = <SPECIES>;
$oline = <OUTLIST>;
$sline = <SPECIES>;
$oline = <OUTLIST>;
$sline = <SPECIES>;

$oline = <OUTLIST>;
$sline = <SPECIES>;
($oname, $opid) = split(/\s+/, $oline);
($sname, $spid) = split(/\s+/, $sline);
is($oname, "match-to-seq-1-5-mismatches/1-89", "rfmatch test $testctr: match 1 named correctly");
is($opid,  "94",                               "rfmatch test $testctr: match 1 percent id calc'ed correctly");
is($sname, $oname,                             "rfmatch test $testctr: match 1 named correctly");
is($spid,  $opid,                              "rfmatch test $testctr: match 1 percent id calc'ed correctly");

$oline = <OUTLIST>;
$sline = <SPECIES>;
($oname, $opid) = split(/\s+/, $oline);
($sname, $spid) = split(/\s+/, $sline);
is($oname, "match-to-seq-2-2-mismatches/1-101","rfmatch test $testctr: match 2 named correctly");
is($opid,  "98",                               "rfmatch test $testctr: match 2 percent id calc'ed correctly");
is($sname, $oname,                             "rfmatch test $testctr: match 2 named correctly");
is($spid,  $opid,                              "rfmatch test $testctr: match 2 percent id calc'ed correctly");

$oline = <OUTLIST>;
$sline = <SPECIES>;
($oname, $opid) = split(/\s+/, $oline);
($sname, $spid) = split(/\s+/, $sline);
is($oname, "-",                                "rfmatch test $testctr: match 3 named correctly");
is($opid,  "-",                                "rfmatch test $testctr: match 3 percent id calc'ed correctly");
is($sname, $oname,                             "rfmatch test $testctr: match 3 named correctly");
is($spid,  $opid,                              "rfmatch test $testctr: match 3 percent id calc'ed correctly");

$oline = <OUTLIST>;
$sline = <SPECIES>;
($oname, $opid) = split(/\s+/, $oline);
($sname, $spid) = split(/\s+/, $sline);
is($oname, "-",                                "rfmatch test $testctr: match 4 named correctly");
is($opid,  "-",                                "rfmatch test $testctr: match 4 percent id calc'ed correctly");
is($sname, $oname,                             "rfmatch test $testctr: match 4 named correctly");
is($spid,  $opid,                              "rfmatch test $testctr: match 4 percent id calc'ed correctly");

$oline = <OUTLIST>;
$sline = <SPECIES>;
($oname, $opid) = split(/\s+/, $oline);
($sname, $spid) = split(/\s+/, $sline);
is($oname, "match-to-seq-5-0-mismatches/1-111","rfmatch test $testctr: match 5 named correctly");
is($opid,  "100",                              "rfmatch test $testctr: match 5 percent id calc'ed correctly");
is($sname, $oname,                             "rfmatch test $testctr: match 5 named correctly");
is($spid,  $opid,                              "rfmatch test $testctr: match 5 percent id calc'ed correctly");

close(OUTLIST);
close(SPECIES);

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
  if(scalar(@_) != 5) { die "ERROR run_script entered with wrong number of input args"; }
  my ($script, $options, $logfile, $unlinkAR, $testctr) = (@_);
  run_command("$script $options");

  # parse output, find files we output and add them to unlinkAR
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
