#! /usr/bin/perl
#
# Tests for rfmatch.pl
#
# EPN, Thu Dec  5 14:28:27 2013
use strict;
use warnings FATAL => 'all';
use Test::More tests => 20;
require "./tests.pm";

my $datadir   = "./data/rfmatch";
my $scriptdir = "../Scripts/make/";

my @unlinkA    = (); # array of files to unlink after each test
my @reqdfilesA = ("SEED", "DESC", "CM", "outlist", "species"); # list of files to copy to current dir before each test

my $testctr   = 0; # counter over script runs
my $matchfile = $datadir . "/tinymatch.fa"; # the database file
my ($oline, $oname, $opid);
my ($sline, $sname, $spid);
my $status; # return value from a T* test subroutine, if != "", we've got an ERROR and we need to do the drill for cleaning up and exiting.

###################################
# Script test 1: Default parameters
###################################
$testctr++;
@unlinkA = ();
push(@unlinkA, @reqdfilesA);
if(($status = TcopyFiles($datadir, \@reqdfilesA)) ne "") { TcleanUpAndDie($status, \@unlinkA); }
# run the script
$status = TrunScript($scriptdir . "/rfmatch.pl", "-f $matchfile test > /dev/null", "rfmatch.test.log", \@unlinkA, $testctr);
if($status ne "") { TcleanUpAndDie($status, \@unlinkA); }
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

$status = TcleanUp(\@unlinkA);
if($status ne "") { TcleanUpAndDie($status, \@unlinkA); }

exit 0;

