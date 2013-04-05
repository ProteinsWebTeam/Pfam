#!/usr/bin/env perl

use strict;
use Cwd;
use Getopt::Long;
use File::stat;
use Data::Printer;
use Carp;

use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family::MSA;
use Bio::Rfam::Infernal;
use Bio::Rfam::Utils;

my $start_time = time();
my $executable = $0;

###################################################################
# Preliminaries:
# - set default values that command line options may change
# - process command line options
# - set input/output file names, and ensure input files exist
# - process DESC file

# set default values that command line options may change
# build related options
my $force_build = 0;            # TRUE to force build
my $do_nostruct = 0;            # TRUE to allow building of CMs with no structure
# calibration related options
my $force_calibrate = 0;        # TRUE to force calibration
my $ncpus_cmcalibrate;          # number of CPUs for cmsearch calls
# search related options
my $evalue;                     # cmsearch E-value to use
my $dbchoice = "rfamseq";       # dbchoice, by default rfamseq
my $no_search = 0;              # TRUE to do cmsearch
my $ncpus_cmsearch;             # number of CPUs for cmsearch calls
my @cmosA = ();                 # extra single '-' cmsearch options (e.g. -g)
my @cmodA = ();                 # extra double '--' cmsearch options (e.g. --cyk)
# other options
my $do_help = 0;                # TRUE to print help and exit, if -h used
my $do_nosearch = 0;            # TRUE to set --nosearch

my $date = scalar localtime();
my $logFH;
open($logFH, ">rfsearch.log") || die "ERROR unable to open rfsearch.log for writing";
output_rfam_banner($logFH,   $executable, "build, calibrate, and search a CM against a database", 1);

&GetOptions( "b"          => \$force_build,
	     "nostruct"   => \$do_nostruct,
	     "c"          => \$force_calibrate,
	     "ccpu"       => \$ncpus_cmcalibrate,
             "e=s",       => \$evalue,
	     "dbchoice=s" => \$dbchoice,
	     "nosearch"   => \$no_search,
	     "scpu"       => \$ncpus_cmsearch,
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmodA,
	     "h|help"     => \$do_help );

if ( $do_help ) {
  &help();
  exit(1);
}

# output header
my $user  = getlogin() || getpwuid($<);
if (! defined $user || length($user) == 0) { 
  die "FATAL: failed to run [getlogin or getpwuid($<)]!\n[$!]";
}
output_header($logFH, $user, $date, $dbchoice, 1);

# setup variables 
my $config = Bio::Rfam::Config->new;
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
                                   );
my $msa  = $famObj->SEED;
my $desc = $famObj->DESC;

# extra processing of command-line options 
if ($do_nosearch) { # --nosearch, verify incompatible options are not set
  if (defined $ncpus_cmsearch) { die "ERROR --nosearch and --scpu are incompatible"; }
  if (defined $evalue)         { die "ERROR --nosearch and -e are incompatible"; }
  if (@cmosA)                  { die "ERROR --nosearch and --cmosA are incompatible"; }
  if (@cmodA)                  { die "ERROR --nosearch and --cmodA are incompatible"; }
}

# ncpus_cmsaerch and ncpus_cmcalibrate must be >= 0
if (defined $ncpus_cmsearch    && $ncpus_cmsearch    < 0) {
  die "ERROR with --scpu <n>, <n> must be >= 0";
}
if (defined $ncpus_cmcalibrate && $ncpus_cmcalibrate < 0) {
  die "ERROR with --ccpu <n>, <n> must be >= 0";
}

if ($do_nostruct) { # --nostruct: verify SEED either has SS_cons with 0 bps or does not have SS_cons
  if ($msa->has_sscons) {
    my $nbps = $msa->num_basepairs;
    if ($nbps > 0) { die "ERROR with --nostruct, SEED must have zero bp SS_cons, or no SS_cons"; }
    else {  # --nostruct enabled, but msa does not have one, add one
      $msa->set_blank_sscons;
    }
  }
}

# no all gap columns allowed in SEED
if ($msa->any_allgap_columns) { 
  die "ERROR all gap columns exist in SEED";
}

###################################################################################################
output_progress_column_headings($logFH, 1);

##############
# Build step #
##############
# Get CM
my $seedfile = "SEED";
my $cmfile   = "CM";
my $cm;
if (-s $cmfile) { 
  $famObj->CM($io->parseCM($cmfile));
  $cm = $famObj->CM($io->parseCM($cmfile));
}

my $is_cm_calibrated = 0;
if (defined $cm && $cm->is_calibrated) { 
  $is_cm_calibrated = 1;
}

# figure out if we have to build
my $do_build = 0;
if (($force_build)        ||             # user set -b on command line
    (! defined $cm)       ||             # 'CM' does not exist
    (! $is_cm_calibrated) ||             # 'CM' is not calibrated
    (youngerThan($seedfile, $cmfile))) { # SEED is younger than CM file
  $do_build = 1;
}

my $build_wall_secs = 0;
my $build_elp_secs = 0;
my $did_build = 0;
if ($do_build) { 
  # get cmbuild options 
  my $build_start_time = time();
  my $buildopts = $desc->{'BM'};

  if (! defined $buildopts) {
    $buildopts = "";
  }
  $buildopts =~ s/\s*-F\s*/ /;
  $buildopts =~ s/\s*CM\s*/ /;
  $buildopts =~ s/\s*SEED\s*/ /;
  $buildopts =~ s/cmbuild//;
  # if --rf exists in BM:
  # if MSA RF exists:         replace --rf in $buildopts with --hand
  # if MSA RF does not exist: remove --rf or --hand from buildopts
  if ($msa->has_rf) { 
    $buildopts =~ s/\-\-rf\s*/\-\-hand /;
  } else {  # no RF
    $buildopts =~ s/\-\-hand\s*//;
    $buildopts =~ s/\-\-rf\s*//;
  }
  $buildopts =~ s/\s+/ /g; # replace multiple spaces with single spaces
  $buildopts =~ s/\s+$//;  # remove trailing spaces
  $buildopts =~ s/^\s+//;  # remove leading spaces
    
  # clean up any files from previous runs
  if (-e "CM.xxx") { unlink "CM.xxx"; }

  # run cmbuild to create new CM
  my $outfile = "b.$$.out";
  $build_elp_secs = Bio::Rfam::Infernal::cmbuild_wrapper($config, "$buildopts", $cmfile, $seedfile, $outfile);
  unlink $outfile;
  $famObj->DESC->BM("cmbuild -F $buildopts CM SEED");

  # define (or possibly redefine) $cm
  $famObj->CM($io->parseCM("CM"));
  $cm = $famObj->CM($io->parseCM("CM"));
  $is_cm_calibrated = 0;

  $build_wall_secs = time() - $build_start_time;
  $did_build = 1;
  output_progress_local($logFH,   "cmbuild", $build_wall_secs, $start_time, 1);
} # end of if($do_build)
else { 
  $did_build = 0;
  output_progress_skipped($logFH,   "cmbuild", 1);
}

####################
# Calibration step #
####################
my $calibrate_wall_secs     = 0;
my $calibrate_cpu_secs      = 0;
my $calibrate_elp_secs      = 0;
my $calibrate_max_wait_secs = 0;
my $cmcalibrate_ncpu        = 81;
my $calibrateO     = "c.$$.out"; # cmcalibrate output file
my $calibrate_errO = "c.$$.err"; # error output
my $did_calibrate = 0;
if ($force_calibrate || (! $is_cm_calibrated)) { 
  my $calibrate_start_time = time();
  my $predicted_minutes = Bio::Rfam::Infernal::cmcalibrate_wrapper($config, 
                                                                   "c.$$",             # job name
                                                                   "",                 # options for cmcalibrate 
                                                                   "CM",               # path to CM file
                                                                   $calibrateO,        # path to output file 
                                                                   $calibrate_errO,    # path to error output file 
                                                                   $cmcalibrate_ncpu); # number of processors
  my @jobnameA = ("c.$$");
  my @outnameA = ("c.$$.out");
  $calibrate_max_wait_secs = Bio::Rfam::Utils::wait_for_cluster($user, \@jobnameA, \@outnameA, "[ok]", "cmcalibrate-mpi", $logFH);
  if(-s $calibrate_errO) { die "Error output from cmcalibrate, see $calibrate_errO"; }
  # if we get here, err file was empty, so we keep going
  unlink $calibrate_errO; # this file is empty anyway

  $famObj->DESC->CB("cmcalibrate --mpi CM");

  $calibrate_wall_secs = time() - $calibrate_start_time;
  Bio::Rfam::Infernal::process_cpu_times($calibrateO, "# CPU time:", undef, undef, \$calibrate_cpu_secs, \$calibrate_elp_secs);
  my $mpi_overhead_secs = 10;
  $calibrate_elp_secs += $mpi_overhead_secs; # MPI slows things down, and we don't want our efficiency to be lower due to this
  $calibrate_cpu_secs *= $cmcalibrate_ncpu; # cmcalibrate doesn't report total running time
  unlink $calibrateO;

  # done the calibration, we need to redefine $cm
  $famObj->CM($io->parseCM("CM"));
  $cm = $famObj->CM($io->parseCM("CM"));
  if($cm->is_calibrated) { $is_cm_calibrated = 1; }
  else                   { die "Error, CM not calibrated following apparently successful cmcalibrate run"; }

  $did_calibrate = 1;
}
else { 
  $did_calibrate = 0;
  output_progress_skipped($logFH,   "cmcalibrate", 1);
}
  
###############
# Search step #
###############
my $search_wall_secs     = 0;
my $search_cpu_secs      = 0;
my $search_max_wait_secs = 0;
my $search_max_elp_secs  = 0;
my $did_search           = 0;
if (! $no_search) { 
  # setup dbfile 
  my $search_start_time = time();
  my $dbconfig     = $config->seqdbConfig($dbchoice);
  my $Z            = $dbconfig->{"dbSize"};
  my $nfiles       = $dbconfig->{"nSearchFiles"};
  my $file_prefix  = $dbconfig->{"searchPathPrefix"};
  my $file_suffix  = $dbconfig->{"searchPathSuffix"};
  my $acc          = $famObj->DESC->AC;
  my $idx;    # index for database file we're currently searching
  my $dbfile; # file we're searching

  # Determine bit score or E-value threshold to use for cmsearch.
  #
  # This section is complex, but should simplify post Rfam 12.0
  # when we should be able to use a single E-value threshold (e.g. 1000)
  # for all searches. Currently though, with the transition to 
  # Infernal 1.1, we want to set thresholds similar to how they 
  # were before the switch.
  #
  # 4 possible cases:
  # Case 1: If user set -cme <f> option, use that with -E <f>.
  # If user did not use -cme:
  # Case 2:      if GA-2 corresponds to an E-value <= 1000  then use -E 1000
  # Case 3: else if GA-2 corresponds to an E-value >= 50000 then use -E 50000
  # Case 4: else use -T <x>, where <x> = GA-2.

  my $use_cmsearch_evalue;    # true to use -E $cmsearch_eval, false to use -T $cmsearch_bitsc
  my $cmsearch_evalue;        # E-value to use with cmsearch
  my $cmsearch_bitsc;         # bit score thr to use, irrelevant unless $use_cmsearch_eval is set to 0 below
  my $e_bitsc        = 0;     # bit score corresponding to $cmsearch_eval
  my $ga_bitsc       = 0;     # GA bitscore for this model
  my $ga_evalue      = 0;     # E-value corresponding to GA bit score
  my $max_evalue     = 50000; # hard-coded max E-value allowed, not applied if -cme used
  my $min_bitsc      = 0;     # bit score corresponding to $max_eval, set below
  my $min_evalue     = 1000;  # hard-coded min E-value allowed, not applied if -cme used

  if (defined $evalue) {      # -cme option set on cmdline
    $use_cmsearch_evalue = 1;
    $cmsearch_evalue     = $evalue;
  } else {                    # -cme not set 
    # set default as Case 2, we'll change if nec below:
    $cmsearch_evalue     = 1000;
    $use_cmsearch_evalue = 1;

    # get GA and check to see if cases 3 or 4 apply
    $ga_bitsc  = $famObj->DESC->CUTGA; 
    $e_bitsc   = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $cmsearch_evalue, $Z);
    $min_bitsc = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $max_evalue,      $Z);
    if (($ga_bitsc-2) < $min_bitsc) { # case 3
      $evalue = $max_evalue; 
    } elsif (($ga_bitsc-2) < $e_bitsc) { # case 4
      $cmsearch_bitsc = $ga_bitsc-2;
      $use_cmsearch_evalue = 0;
    }
  }

  # define other options for cmsearch
  my $ncpus; 
  if (! defined $ncpus_cmsearch) { $ncpus_cmsearch = 4; }
  my $searchopts = " -Z $Z --cpu $ncpus_cmsearch --verbose ";
  if ($use_cmsearch_evalue) { $searchopts .= " -E $cmsearch_evalue "; }
  else                      { $searchopts .= " -T $cmsearch_bitsc ";  }

  my $extra_options = Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  $searchopts .= $extra_options;
  $searchopts =~ s/\s+/ /g; # replace multiple spaces with single spaces
  $searchopts =~ s/\s+$//;  # remove trailing spaces
  $searchopts =~ s/^\s+//;  # remove leading spaces

  # submit jobs
  my @jobnameA = ();
  my @tblOA    = ();
  my @cmsOA    = (); 
  my @errOA    = ();
  my $file_idx; 
  for($idx = 0; $idx < $nfiles; $idx++) { 
    $file_idx = $idx + 1; # off-by-one w.r.t $idx, because database file names are 1..$nfiles, not 1..$nfiles-1
    $jobnameA[$idx] = "s.$$.$file_idx";  
    $tblOA[$idx]    = "s.$$.$file_idx.tbl";
    $cmsOA[$idx]    = "s.$$.$file_idx.cmsearch";
    $errOA[$idx]    = "s.$$.$file_idx.err";
    $dbfile = $file_prefix . $file_idx . $file_suffix;
    Bio::Rfam::Infernal::cmsearch_wrapper($config, $jobnameA[$idx], "--tblout " . $tblOA[$idx] . " " . $searchopts, $cmfile, $dbfile, $cmsOA[$idx], $errOA[$idx]);
  }
  
  # wait for cluster jobs to finish
  $search_max_wait_secs = Bio::Rfam::Utils::wait_for_cluster($user, \@jobnameA, \@tblOA, "# [ok]", "cmsearch", $logFH);
  $search_wall_secs = time() - $search_start_time;

  # concatenate files (no need to validate output, we already did that in wait_for_cluster())
  my $all_errO = "searcherr";
  my $all_tblO = "TBLOUT";
  my $all_cmsO = "searchout";
  # first, create the concatenated error file, if it's not empty we'll die before creating TBLOUT
  Bio::Rfam::Utils::concatenate_files(\@errOA, $all_errO, 1); # '1' says delete original files after concatenation
  if(-s $all_errO) { die "Error output by >=1 cmsearch runs, see $all_errO"; }
  # if we get here, all err files were empty, so we keep going
  unlink $all_errO; # this file is empty anyway
  Bio::Rfam::Utils::concatenate_files(\@tblOA, $all_tblO, 1); # '1' says delete original files after concatenation
  Bio::Rfam::Utils::concatenate_files(\@cmsOA, $all_cmsO, 1); # '1' says delete original files after concatenation

  # update DESC with search method
  $famObj->DESC->SM("cmsearch $searchopts CM SEQDB");

  # timing info
  $search_wall_secs = time() - $search_start_time;
  Bio::Rfam::Infernal::process_cpu_times($all_cmsO, "Total runtime:", undef, \$search_max_elp_secs, \$search_cpu_secs, undef);
  $did_search = 1;

  # write TBLOUT-dependent files
  $io->writeTbloutDependentFiles($famObj, $config->rfamlive, $famObj->SEED, $famObj->DESC->CUTGA, $config->RPlotScriptPath);
}
# update DESC
if($did_build || $did_calibrate || $did_search) { 
  $io->writeDESC($famObj->DESC);
}

# finished all work, print output file summary
output_file_summary_column_headings($logFH, 1);
my $description;
if($did_build || $did_calibrate) { 
  $description = sprintf("covariance model file (%s)", $did_build ? "built and calibrated" : "calibrated only");
  output_file_summary($logFH,   "CM", $description, 1);
}
if($did_build || $did_calibrate || $did_search) { 
  $description = sprintf("desc file (updated:%s%s%s)", 
                         ($did_build)     ? " BM" : "", 
                         ($did_calibrate) ? " CB" : "", 
                         ($did_search)    ? " SM" : "");
  output_file_summary($logFH,   "DESC", $description, 1);
}
if($did_search) { 
  # TBLOUT
  $description = sprintf("concatenated --tblout output from all cmsearch jobs");
  output_file_summary($logFH,   "TBLOUT", $description, 1);
  # searchout
  $description = sprintf("concatenated standard output from all cmsearch jobs");
  output_file_summary($logFH,   "searchout", $description, 1);
  # outlist
  $description = sprintf("sorted list of all hits from TBLOUT");
  output_file_summary($logFH,   "outlist", $description, 1);
  # species
  $description = sprintf("same as outlist, but with additional taxonomic information");
  output_file_summary($logFH,   "species", $description, 1);
  # outlist.pdf
  $description = sprintf("bit score histograms of all hits");
  output_file_summary($logFH,   "outlist.pdf", $description, 1);
  # species.pdf
  $description = sprintf("bit score histogram of hits, colored by taxonomy");
  output_file_summary($logFH,   "species.pdf", $description, 1);

}
$description = sprintf("log file (*this* output, printed to stdout)");
output_file_summary($logFH,   "rfsearch.log", $description, 1);

# output time summary
output_timing_summary_column_headings($logFH, 1);

my $total_wall_secs = time() - $start_time;
my $total_cpu_secs  = $build_wall_secs + $calibrate_cpu_secs + $search_cpu_secs;
my $total_elp_secs  = $build_elp_secs + $calibrate_elp_secs + $search_max_elp_secs;

if($did_build) { 
  output_timing_summary($logFH,   "cmbuild", $build_wall_secs, $build_elp_secs, "-", $build_elp_secs, 1);
}
if($did_calibrate) { 
  output_timing_summary($logFH,   "cmcalibrate", $calibrate_wall_secs, $calibrate_cpu_secs, $calibrate_max_wait_secs, $calibrate_elp_secs, 1);
}
if($did_search) { 
  output_timing_summary($logFH,   "cmsearch", $search_wall_secs, $search_cpu_secs, $search_max_wait_secs, $search_max_elp_secs, 1);
}
if($did_build || $did_calibrate || $did_search) { 
  output_timing_summary($logFH,   "total", $total_wall_secs, $total_cpu_secs, "-", $total_elp_secs, 1);
}
printf("# [ok]\n");
printf $logFH ("# [ok]\n");
close($logFH);
exit(0);


######################################################################
# output_* subroutines: for outputting to stdout and to a log file.
######################################################################

sub output_rfam_banner { 
  my ($fh, $executable, $banner, $also_stdout) = @_;

  my $str;
  $str = sprintf ("# %s :: %s\n", Bio::Rfam::Utils::file_tail($executable), $banner);
  print $fh $str; if($also_stdout) { print $str; }
  #printf $fp ("# RFAM\n");
  #printf $fp ("# COPYRIGHT INFO GOES HERE\n");
  #printf $fp ("# LICENSE INFO GOES HERE\n");
  $str = "# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n";
  print $fh $str; if($also_stdout) { print $str; }

  return;
}

sub output_header { 
  my ($fh, $user, $date, $dbchoice, $also_stdout) = @_;

  my $str;
  $str = sprintf ("# user:               %s\n", $user);
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# date:               %s\n", $date);
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# pwd:                %s\n", getcwd);
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# db:                 %s\n", $dbchoice);
  print $fh $str; if($also_stdout) { print $str; }
  $str = ("# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
  print $fh $str; if($also_stdout) { print $str; }

  return;
}

sub output_progress_column_headings { 
  my ($fh, $also_stdout) = @_;

  my $str;
  $str = "#\n";
  print $fh $str; if($also_stdout) { print $str; }
  $str = "# Per-stage progress:\n#\n";
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# %-15s  %-10s  %10s  %10s  %10s  %10s\n", "stage",           "type",       "\#finished", "\#running",  "\#waiting",  "stage-time");
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# %-15s  %-10s  %10s  %10s  %10s  %10s\n", "===============", "==========", "==========", "==========", "==========", "==========");
  print $fh $str; if($also_stdout) { print $str; }

  return;
}

sub output_progress_skipped { 
  my ($fh, $stage, $also_stdout) = @_;

  my $str = sprintf ("  %-15s  %-10s  %10s  %10s  %10s  %10s\n", $stage, "skipped", "-", "-", "-", "-");
  print $fh $str; if($also_stdout) { print $str; }
  
  return;
}  

sub output_progress_local {
  my ($fh, $stage, $run_secs, $also_stdout) = @_;

  my $str = sprintf ("  %-15s  %-10s  %10s  %10s  %10s  %10s\n", $stage, "local", "1", "0", "0", Bio::Rfam::Utils::format_time_string($run_secs));
  print $fh $str; if($also_stdout) { print $str; }
  
  return;
}  

sub output_file_summary_column_headings { 
  my ($fh, $also_stdout) = @_;

  my $str;
  $str = "#\n";
  print $fh $str; if($also_stdout) { print $str; }
  $str = "# Output file summary:\n#\n";
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# %-12s    %-60s\n", "file name",  "description");
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# %-12s    %-60s\n", "============", "============================================================");
  print $fh $str; if($also_stdout) { print $str; }

  return;
}

sub output_file_summary { 
  my ($fh, $filename, $desc, $also_stdout) = @_;

  my $str = sprintf ("  %-12s    %-60s\n", $filename, $desc);
  print $fh $str; if($also_stdout) { print $str; }

  return;
}

sub output_timing_summary_column_headings { 
  my ($fh, $also_stdout) = @_;

  my $str;
  $str = "#\n";
  print $fh $str; if($also_stdout) { print $str; }
  $str = "# Timing summary:\n#\n";
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# %-15s  %-10s  %10s  %10s  %10s  %10s\n", "stage",          "wall time",  "ideal time",  "cpu time",   "wait time",  "efficiency");
  print $fh $str; if($also_stdout) { print $str; }
  $str = sprintf ("# %-15s  %-10s  %10s  %10s  %10s  %10s\n", "==============", "==========", "==========", "==========", "==========", "==========");
  print $fh $str; if($also_stdout) { print $str; }

  return;
}

sub output_timing_summary { 
  my ($fh, $stage, $wall_secs, $tot_cpu_secs, $wait_secs, $max_elp_secs, $also_stdout) = @_;

  my $efficiency = 1.0;
  if($wall_secs > 0 && ($wall_secs > $max_elp_secs)) { 
    $efficiency = $max_elp_secs / $wall_secs;
  }
  my $str = sprintf ("  %-15s  %10s  %10s  %10s  %10s  %10.2f\n",
                     $stage, 
                     Bio::Rfam::Utils::format_time_string($wall_secs),
                     Bio::Rfam::Utils::format_time_string($max_elp_secs),
                     Bio::Rfam::Utils::format_time_string($tot_cpu_secs),
                     ($wait_secs eq "-") ? "-" : Bio::Rfam::Utils::format_time_string($wait_secs),
                     $efficiency);
  print $fh $str; if($also_stdout) { print $str; }

  return;
}

######################################################################
# youngerThan(file1, file2): test if file1 is younger than file2 
sub youngerThan {
  my ($file1, $file2) = @_;
  my ($t1,$t2) = (0,0);
  $t1 = stat($file1)->mtime if -e $file1;
  $t2 = stat($file2)->mtime if -e $file2;
    
  if ($t1>$t2 or $t1==0 or $t2==0) {
    return 1;
  } else {
    return 0;
  }
}
######################################################################

sub help {
  print STDERR <<EOF;

rfsearch.pl: builds, calibrates and searches a CM against a sequence database.
             Run from within a directory containing "SEED" & "DESC" files. 
	     E.g., after running "rfupdate.pl RFXXXXX" or "rfco.pl RFXXXXX".
	     SEED contains a stockholm format alignment and DESC is an internal 
	     Rfam documentation describing each RNA family. 

Usage:      rfsearch.pl [options]

Options:    OPTIONS RELATED TO BUILD STEP (cmbuild):
	    -b          always run cmbuild (default: only run if 'CM' is v1.0 or doesn't exist)
	    --nostruct  set a zero basepair SS_cons in SEED prior to cmbuild (none must exist in SEED)

            OPTIONS RELATED TO CALIBRATION STEP (cmcalibrate):
	    -c          always run cmcalibrate (default: only run if 'CM' is not calibrated)
            --ccpu <n>  set number of CPUs for MPI cmcalibrate job to <n>

            OPTIONS RELATED TO SEARCH STEP (cmsearch):
            -e <f>         set cmsearch E-value threshold as <f> bits
            --dbchoice <s> set sequence database to search as <s> ('rfamseq' or 'testrfamseq')
            --nosearch     do not run cmsearch
            --scpu <n>     set number of CPUs for cmsearch jobs to <n>
	    --cmos <str>   add extra arbitrary option to cmsearch with '-<str>'. (Infernal 1.1, only option is '-g')
            --cmod <str>   add extra arbitrary options to cmsearch with '--<str>'. For multiple options use multiple
	                   -cmod lines. e.g. '-cmod toponly -cmod anytrunc' will run cmsearch with --toponly and --anytrunc.

            OTHER OPTIONS:
  	    -h|-help     print this help, then exit
EOF
}

