#!/usr/bin/env perl 

# rfseed.pl - Add sequences to or remove sequences from a SEED.
use strict;
use warnings;
use Cwd;
use Getopt::Long;
use File::Copy;
use Data::Printer;
use Carp;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use Bio::Rfam::Infernal;
use Bio::Rfam::Utils;

use Bio::Easel::MSA;
use Bio::Easel::SqFile;
use Bio::Easel::Random;

###################################################################
# Preliminaries:
# - set default values that command line options may change
# - process command line options
# - set input/output file names, and ensure input files exist
# - process DESC file

my $start_time = time();
my $executable = $0;
my $dbchoice = "rfamseq";       # TODO: read this from DESC's SM

# set default values that command line options may change
# other options
my $do_stdout = 1;              # TRUE to output to STDOUT
my $do_quiet  = 0;              # TRUE to not output anything to STDOUT
my $do_delete = 0;              # TRUE to delete seqs instead of adding them
my $do_name   = 0;              # TRUE if input file only includes name/start-end names
my $do_tblout = 0;              # TRUE if input file is in cmsearch --tblout format
my $do_local  = 0;              # TRUE to align locally w.r.t. the CM
my $do_prob   = 0;              # TRUE to include PPs in output alignment
my $do_help   = 0;              # TRUE to print help and exit, if -h used

my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;
my $exec_description = "add seqs to or remove seqs from a SEED";

my $options_okay = &GetOptions( "d"          => \$do_delete,
                                "n"          => \$do_name,
                                "t"          => \$do_tblout,
                                "l"          => \$do_local,
                                "p"          => \$do_prob,
                                "quiet",     => \$do_quiet,
                                "h|help"     => \$do_help );
if(! $options_okay) { 
  &help($exec_description); 
  die "ERROR, unrecognized option; "; 
}

$do_stdout = ($do_quiet) ? 0 : 1;
# copy rfseed.log sideways if it exists
if (-e "rfseed.log") { copy("rfseed.log", "rfseed.log.$$"); }
open($logFH, ">rfseed.log") || die "ERROR unable to open rfseed.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, $exec_description, $do_stdout);

# read in command line variables
my $infile = "";
if(scalar(@ARGV) != 1) { 
  $do_help = 1; 
}
else { 
  $infile = $ARGV[0];
}

if ( $do_help ) {
  &help($exec_description);
  exit(1);
}

# output header
my $user  = getpwuid($<);
if (! defined $user || length($user) == 0) { 
  die "FATAL: failed to run [getpwuid($<)]!\n[$!]";
}

# setup variables 
my $io     = Bio::Rfam::FamilyIO->new;
my $famObj = Bio::Rfam::Family->new(
                                    'SEED' => {
                                               fileLocation => "SEED",
                                               aliType      => 'seed'
                                              },
                                    'TBLOUT' => { 
                                                 fileLocation => "TBLOUT",
                                                },
                                    'DESC'   => $io->parseDESC("DESC"),
                                    'CM'     => $io->parseCM("CM"),
                                   );
my $oseedmsa = $famObj->SEED;
my $desc     = $famObj->DESC;
my $cm       = $famObj->CM;
my $id       = $desc->ID;
my $acc      = $desc->AC;
my $ndelete  = 0; # number of seqs to delete
my $nadd     = 0; # number of seqs to add

# extra processing of command-line options 
if($do_delete) { 
  if($do_local) { die "ERROR -l option is incompatible with -d"; }
  if($do_prob)  { die "ERROR -p option is incompatible with -d"; }
}
if($do_name && $do_tblout) { 
  die "ERROR -t and -n are incompatible, choose one.";
}

# setup dbfile 
my $dbconfig  = $config->seqdbConfig($dbchoice);
my $fetchfile = $dbconfig->{"fetchPath"};

# output preamble: user, date, location, etc.
my $cwidth = 55;
Bio::Rfam::Utils::log_output_preamble($logFH, $cwidth, $user, $config, $desc, $do_stdout);

if($do_delete) { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# delete mode; removing seqs instead of adding them:", "yes [-d]"), $do_stdout); }
if($do_name)   { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# input file is only sequence names:",                 "yes [-n]"), $do_stdout); }
if($do_local)  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# align new sequences locally w.r.t. CM: ",            "yes [-l]"), $do_stdout); }
if($do_prob)   { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# include post probs in new seed: ",                   "yes [-p]"), $do_stdout); }
if($do_quiet)  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# quiet mode: ",                                       "on  [-quiet]"), $do_stdout); }
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "#\n", $do_stdout);

# make sure we have a CM file, and that it's newer than the SEED
if(! -s 'CM') { die "ERROR: CM does not exist, did you run rfsearch.pl?"; }
if(Bio::Rfam::Utils::youngerThan("SEED", "CM")) { die "ERROR SEED is younger than CM, did you run rfsearch.pl (possibly with -onlybuild)?"; }
                                                  
# create hash of potential output files
my %outfileH = ();
my @outfile_orderA = ("SEED.$$", "CM.$$");
if(! $do_delete) { push (@outfile_orderA, "seedalignout"); }
push(@outfile_orderA, "rfseed.log.$$");
$outfileH{"SEED.$$"}       = "old seed alignment, copy of 'SEED' from before this script was run";
$outfileH{"CM.$$"}         = "old CM file, copy of 'CM' from before this script was run";
$outfileH{"rfseed.log.$$"} = "old rfseed.log file, copy of 'rfseed.log' from before this script was run";
if(! $do_delete) { 
  $outfileH{"seedalignout"} = "tabular cmalign output for 'SEED' (cmalign --mapali 'SEED.$$'...)";
}

my @unlinkA = ();

Bio::Rfam::Utils::log_output_progress_column_headings($logFH, sprintf("per-stage progress:"), $do_stdout);

####################################################################
# Step 1 of 3: get array of name/start-ends to either add or delete
####################################################################
my @nseA = (); # array of name/start-ends
if($do_name) { 
  Bio::Rfam::Utils::fileToArray($infile, \@nseA, 1); # 1 says, remove newlines from each line
}
else { # tblout format or outlist format
  Bio::Rfam::FamilyIO::nseArrayFromOutlistOrTblout($infile, 
                                                   ($do_tblout) ? "tblout" : "outlist", # either tblout or outlist format
                                                   "", \@nseA); # "" means: do not enforce a minimum score
  if(! $do_delete) { 
    # verify no seqs to add overlap with any currently in the SEED
    die_if_overlap_with_seed($oseedmsa, \@nseA);
    # verify no two seqs to add overlap with each other
    die_if_two_nse_overlap(\@nseA);
  }
}
if(scalar(@nseA) == 0) { die "ERROR, no sequences read from $infile"; }

####################################################
# Step 2 of 3: delete seqs from or add seqs to SEED
####################################################
# first copy SEED sideways, we do this after we parse the outlist, which checks for overlaps between new seqs and seed seqs
if (-e "SEED") { copy("SEED", "SEED.$$"); }

if($do_delete) { # delete mode
  # initialize keepmeA to all '1' ('1' means: don't remove me)
  my @keepmeA  = ();
  Bio::Rfam::Utils::setArray(\@keepmeA, 1, $oseedmsa->nseq);
  # update keepmeA[i] to '0' for any seqs i we want to delete
  foreach my $nse (@nseA) { 
    my $seed_idx = get_seed_idx($oseedmsa, $nse);
    if(! $keepmeA[$seed_idx]) { die "ERROR, trying to delete $nse twice, it must be duplicated in $infile"; }
    $keepmeA[$seed_idx] = 0;
    $ndelete++;
  }
  # delete the seqs
  my $newmsa = $oseedmsa->sequence_subset(\@keepmeA);
  # remove any columns that are now all gaps
  $newmsa->remove_all_gap_columns(0);
  # output msa
  $newmsa->write_msa("SEED", "stockholm");
}
else { # adding seqs
  # fetch sequences to add
  my $fafile = "$$.fa";
  my ($nseq, $nres) = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@nseA, $fetchfile, $fafile, $logFH, $do_stdout);
  $nadd = $nseq;

  # align new sequences to CM, using --mapali and --mapstr with original SEED
  my $cmalign_file = "seedalignout";
  my $err_file     = "a.$$.err";
  my $align_opts = "-o SEED --mapali SEED.$$ --mapstr";
  if(! $do_prob)  { $align_opts .= " --noprob"; }
  if(! $do_local) { $align_opts .= " -g"; }
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $align_opts, "CM", $fafile, $cmalign_file, $err_file, $nseq, $nres, 1, 0, "", -1, $logFH, $do_stdout);
  push(@unlinkA, ($err_file, $fafile));
}

# unlink files we don't want to keep
foreach my $file (@unlinkA) { if(-s $file) { unlink $file } }

# pause 1 second before building CM, so we can tell CM file was created after SEED file
sleep(1);

################################################
# Step 3 of 3: create new CM file from new SEED
################################################
# first copy existing CM sideways
if (-e "CM")   { copy("CM",   "CM.$$"); }

# build new CM, using BM from DESC
my $build_opts = $desc->BM;
$build_opts =~ s/^cmbuild\s+//; # remove leading 'cmbuild'
$build_opts =~ s/CM\s+SEED$//;   # remove trailing 'CM SEED';
my $build_start_time = time();
Bio::Rfam::Infernal::cmbuild_wrapper($config, $build_opts, "CM", "SEED", "b.$$.out");
unlink "b.$$.out";
Bio::Rfam::Utils::log_output_progress_local($logFH, "cmbuild", time() - $build_start_time, 0, 1, "", $do_stdout);

###############################################
# finished all work, print output file summary
###############################################
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, $do_stdout);
# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
foreach my $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, $do_stdout);
 }
}
if(-e "SEED") { 
  if($do_delete) { Bio::Rfam::Utils::log_output_file_summary($logFH, "SEED", sprintf("new seed alignment; %d seqs removed", $ndelete), $do_stdout); }
  else           { Bio::Rfam::Utils::log_output_file_summary($logFH, "SEED", sprintf("new seed alignment; %d seqs added with cmalign [NOTE: inserts in old seqs may have shifted!]", $nadd), $do_stdout); }
}
if(-e "CM") { 
  Bio::Rfam::Utils::log_output_file_summary($logFH, "CM", "new CM, built from new SEED (not yet calibrated)", $do_stdout);
}
if( -e "rfseed.log") { 
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfseed.log", "log file (*this* output)", $do_stdout);
}

Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("#\n"), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# Total time elapsed: %s\n", Bio::Rfam::Utils::format_time_string(time() - $start_time)), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# [ok]\n"), $do_stdout);

close($logFH);
exit 0;

###############
# SUBROUTINES #
###############

#########################################################
# die_if_overlap_with_seed($oseedmsa, $nseAR)
#
# Check if any name/start-ends in an array (referred
# to by $nseAR) overlaps with any sequence already in the seed
# and die if any do.
#
sub die_if_overlap_with_seed {
  my ($oseedmsa, $nseAR) = @_;

  foreach my $nse (@{$nseAR}) { 
    my ($overlap_name, $overlap_fraction) = $oseedmsa->nse_overlap($nse);
    if($overlap_name ne "") { die "ERROR new sequence to add $nse overlaps with sequence already in SEED ($overlap_name)"; }
  }

  return;
}

#########################################################
# die_if_two_nse_overlap: 
# check if any two name/start-ends in an array (referred
# to by $nseAR) overlap, and die if they do.
#
sub die_if_two_nse_overlap { 
  my ($nseAR) = @_;

  my $nnse = scalar(@{$nseAR});
  
  for(my $i = 0; $i < $nnse; $i++) { 
    for(my $j = $i+1; $j < $nnse; $j++) { 
      my $overlap_fraction = Bio::Rfam::Utils::overlap_fraction_two_nse($nseAR->[$i], $nseAR->[$j]);
      if($overlap_fraction > 0.) { die "ERROR two sequences to add overlap with each other: $nseAR->[$i] and $nseAR->[$j]"; }
    }
  }

  return;
}

#########################################################
# get_seed_idx($oseedmsa, $nse)
#
# Return the index of the sequence named $nse in the SEED.
# Die if it does not exist. We do this laboriously
# by comparing every seq name, this should be okay since
# SEEDs are typically small, and only very rarely exceed
# 200 or so seqs.
#
sub get_seed_idx {
  my ($oseedmsa, $nse) = @_;

  my $ret_idx = -1;
  my $nseq = $oseedmsa->nseq;
  for(my $i = 0; $i < $nseq; $i++) { 
    if($oseedmsa->get_sqname($i) eq $nse) { 
      if($ret_idx != -1) { die "ERROR two sequences in the SEED with the same name... shouldn't happen."; }
      $ret_idx = $i;
    }
  }
  if($ret_idx == -1) { die "ERROR unable to find $nse in SEED"; }

  return $ret_idx;
}

#########################################################

sub help {
  my ($description) = @_;

  print STDERR <<EOF;
  
  rfseed.pl - $description

Usage:      rfseed.pl [options] <file with subset of outlist lines for sequences to add to/delete from SEED>

Options:    -d        delete sequences instead of adding them
            -n        <file> includes only sequence names in name/start-end format, one per line
            -t        <file> is in cmsearch --tblout format
            -l        align locally w.r.t the CM [default: globally]
            -p        include posterior probabilities in new SEED [default: do not]
            -quiet    be quiet; do not output anything to stdout (rfseed.log still created)
            -h|-help  print this help, then exit

EOF
}
