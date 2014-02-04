#! /usr/bin/perl
#
# Tests for rfmake.pl.
#
# EPN, Wed Jan 22 13:34:54 2014
use strict;
use warnings FATAL => 'all';
use Test::More tests => 22;
require "./tests.pm";

BEGIN {
    use_ok( 'Bio::Rfam::FamilyIO'   ) || print "Bail out!\n";
}

my $datadir1  = "./data/RF00006";
my $scriptdir = "../Scripts/make/";

my @unlinkA    = (); # array of files to unlink after each test
my @reqdfilesA = ("SEED", "DESC", "CM", "TBLOUT"); # list of files to copy to current dir before each test

my $testctr   = 0; # counter over script runs
my ($oline, $oname, $opid);
my ($sline, $sname, $spid);
my $status; # return value from a T* test subroutine, if != "", we've got an ERROR and we need to do the drill for cleaning up and exiting.


my @optA           = ("",      "-t 40.0",   "-e 1E-6");
my @gaA            = ("29.00", "40.00",     "57.00"  );
my @tcA            = ("29.00", "40.20",     "57.00"  );
my @ncA            = ("28.90", "39.90",     "56.70"  );
my @nabove_ga_expA = (687,     550,         372      );
my @ntaxinfo_expA  = (57,      56,          56       );

#######################################################################
# Script test 1, 2, and 3: (Default parameters, -t 40.00 and -e 1E-6)
#######################################################################
@unlinkA = ();
for(my $t = 1; $t <= 3; $t++) { 
  my $opt           = $optA[$t-1];
  my $nabove_ga_exp = $nabove_ga_expA[$t-1];
  my $ga_exp        = $gaA[$t-1];
  my $nc_exp        = $ncA[$t-1];
  my $tc_exp        = $tcA[$t-1];
  my $ntaxinfo_exp  = $ntaxinfo_expA[$t-1];

  push(@unlinkA, @reqdfilesA);
  if(($status = TcopyFiles($datadir1, \@reqdfilesA)) ne "") { TcleanUpAndDie($status, \@unlinkA); }
  # run the script
  TrunScript($scriptdir . "/rfmake.pl $opt", "> /dev/null", "rfmake.log", \@unlinkA, $t);

  # test 1: verify 'species' and 'outlist'
  # process the 'species' and 'outlist' files, this will verify their format 
  my $io = Bio::Rfam::FamilyIO->new;
  isa_ok($io, "Bio::Rfam::FamilyIO");
  my @nameOA   = ();
  my %groupOHA = ();
  $io->parseOutlistAndSpecies("outlist", "species", 1000, 20, $ga_exp, undef, \@nameOA, \%groupOHA);
  #                                                emax   ga  minsc      infoHH nameOA   groupOHA
  my $nabove_ga_obs = scalar(@nameOA);
  is($nabove_ga_obs, $nabove_ga_exp, "correct number of hits above GA in outlist/species files (test: $t)");

  # test 2: verify 'SCORES' has number of expected hits
  ($status, $nabove_ga_obs) = TnumLinesInFile("SCORES");
  if($status ne "") { TcleanUpAndDie($status, \@unlinkA); }
  is($nabove_ga_obs, $nabove_ga_exp, "correct number of hits above GA in SCORES files (test: $t)");

  # test 3: make sure 'outlist.pdf' and 'species.pdf' files exist -- note that we don't check their contents at all
  if(($status = TverifyNonEmptyFileExists("outlist.pdf")) ne "") { TcleanUpAndDie($status, \@unlinkA); }
  if(($status = TverifyNonEmptyFileExists("species.pdf")) ne "") { TcleanUpAndDie($status, \@unlinkA); }

  # test 4: make sure the DESC file includes the correct GA/TC/NC thresholds.
  my $desc = $io->parseDESC("DESC");
  is($desc->CUTGA, $ga_exp, "GA in DESC is correct (test: $t)");
  is($desc->CUTNC, $nc_exp, "NC in DESC is correct (test: $t)");
  is($desc->CUTTC, $tc_exp, "TC in DESC is correct (test: $t)");

  # test 5: taxinfo file should exist and have 57 lines
  if(($status = TverifyNonEmptyFileExists("taxinfo")) ne "") { TcleanUpAndDie($status, \@unlinkA); }
  my $ntaxinfo_obs;
  ($status, $ntaxinfo_obs) = TnumLinesInFile("taxinfo");
  if($status ne "") { TcleanUpAndDie($status, \@unlinkA); }
  is($ntaxinfo_obs, $ntaxinfo_exp, "correct number of lines in taxinfo file (test: $t)");

  TcleanUp(\@unlinkA);
}
exit 0;

