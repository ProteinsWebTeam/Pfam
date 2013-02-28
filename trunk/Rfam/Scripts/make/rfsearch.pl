#!/usr/local/bin/perl -w

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

my $starttime = time();

###################################################################
# PRELIMINARIES:
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
my $queue;                      # queue to submit all jobs to (default: auto-determined based on location)
my $dirty = 0;                  # TRUE to leave files on file system
my $verbose = 0;                # TRUE to be verbose with output
my $do_help = 0;                # TRUE to print help and exit, if -h used
my $do_nosearch = 0;            # TRUE to set --nosearch

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
	     "queue=s"    => \$queue,
	     "dirty"      => \$dirty,
	     "verbose"    => \$verbose,
	     "h|help"     => \$do_help );

if ( $do_help ) {
  &help();
  exit(1);
}

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
                                    #    'CM'     => $io->parseCM("CM"),
                                   );
my $msa  = $famObj->SEED;
my $desc = $famObj->DESC;

# extra processing of command-line options 
if ($do_nosearch) { # --nosearch, verify incompatible options are not set
  if (defined $ncpus_cmsearch) {
    die "ERROR --nosearch and --scpu are incompatible";
  }
  if (defined $evalue) {
    die "ERROR --nosearch and -e are incompatible";
  }
  if (@cmosA) {
    die "ERROR --nosearch and --cmosA are incompatible";
  }
  if (@cmodA) {
    die "ERROR --nosearch and --cmodA are incompatible";
  }
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
#Gather some info for logging:
my $pwd   = getcwd;
my $user  = getlogin() || getpwuid($<);
if (! defined $user || length($user) == 0) { 
  die "FATAL: failed to run [getlogin or getpwuid($<)]!\n[$!]";
}
&printlog( "USER: [$user] " );
&printlog( "RFSEARCH USING INFERNAL VERSION 1.1");
&printlog( "Using database $dbchoice\n");

# Deal with rfsearch.log and log directory
if (-e "rfsearch.log") { 
  if (! -w "rfsearch.log") {
    die("FATAL: check permissions on rfsearch.log");
  }
  unlink "rfsearch.log";
}

##############
# Build step #
##############
# Get CM
my $seedfile = "SEED";
my $cmfile   = "CM";
my $cm;
if (-e $cmfile) { 
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

if ($do_build) { 
  # get cmbuild options 
  my $buildopts = $desc->{'BM'};
  if (! defined $buildopts) {
    $buildopts = "";
  }
  $buildopts =~ s/-F\s+CM\s+SEED//;
  $buildopts =~ s/cmbuild//;
  # deal with RF related buildopts
  # if RF exists:         replace --rf in $buildopts with --hand
  # if RF does not exist: remove --rf or --hand from buildopts
  if ($msa->has_rf) { 
    $buildopts =~ s/\-\-rf\s*/\-\-hand /;
  } else {  # no RF
    $buildopts =~ s/\-\-hand\s*//;
    $buildopts =~ s/\-\-rf\s*//;
  }
  $buildopts =~ s/\s+/\s/g; # replace multiple spaces with single spaces
  $buildopts =~ s/\s+$//;   # remove trailing spaces
  $buildopts =~ s/^\s+//;   # remove leading spaces
    
  # clean up any files from previous runs
  if (-e "CM.xxx") { unlink "CM.xxx"; }

  # run cmbuild to create new CM
  #TODO don't use system call ask Rob about how he calls esl-sfetch
  Bio::Rfam::Infernal::cmbuild_wrapper($config, "-F $buildopts", $cmfile, $seedfile);
  $famObj->DESC->BM("cmbuild -F $buildopts CM");

  # define (or possibly redefine) $cm
  $famObj->CM($io->parseCM("CM"));
  $cm = $famObj->CM($io->parseCM("CM"));
  $is_cm_calibrated = 0;
} # end of if($do_build)
my $build_endtime = time();

####################
# Calibration step #
####################
if ($force_calibrate || (! $is_cm_calibrated)) { 
  Bio::Rfam::Infernal::cmcalibrate_wrapper($config, "CM");
  $famObj->DESC->CB("cmcalibrate --mpi CM");
}
my $calibrate_endtime = time();

###############
# Search step #
###############
if (! $no_search) { 
  # setup dbfile 
  my $dbconfig     = $config->seqdbConfig($dbchoice);
  my $Z            = $dbconfig->{"dbSize"};
  my $nfiles       = $dbconfig->{"nSearchFiles"};
  my $file_prefix  = $dbconfig->{"searchPathPrefix"};
  my $file_suffix  = $dbconfig->{"searchPathSuffix"};
  my $acc          = $famObj->DESC->AC;
  my $idx;    # index for database file we're currently searching
  my $dbfile; # file we're searching

  # determine bit score or E-value threshold to use for cmsearch.
  # 4 possible cases:
  # Case 1: If user set -cme <f> option, use that with -E <f>.
  # If user did not use -cme hn:
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

  if (defined $evalue) {      # -e option used on cmdline
    $use_cmsearch_evalue = 1;
    $cmsearch_evalue     = $evalue;
  } else {                    # -e not used 
    # set default as case 2:
    $cmsearch_evalue     = 1000;
    $use_cmsearch_evalue = 1;

    # get GA from that and check to see if cases 3 or 4 apply
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
  my $searchopts = " -Z $Z --cpu $ncpus_cmsearch ";
  if ($use_cmsearch_evalue) { $searchopts .= " -E $cmsearch_evalue "; }
  else                      { $searchopts .= " -T $cmsearch_bitsc ";  }

  my $extra_options = Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  $searchopts .= $extra_options;
  $searchopts =~ s/\s+/\s/g; # replace multiple spaces with single spaces
  $searchopts =~ s/\s+$//;   # remove trailing spaces
  $searchopts =~ s/^\s+//;   # remove leading spaces

  # submit jobs
  my ($tblO, $cmsO); # --tblout output and cmsearch default output
  for($idx = 0; $idx < $nfiles; $idx++) { 
    $tblO   = "$idx.tbl";
    $cmsO   = "$idx.cmsearch";
    $dbfile = $file_prefix . $idx . $file_suffix;
    Bio::Rfam::Infernal::cmsearch_wrapper($config, $cmfile, $dbfile, $tblO, $cmsO, $searchopts, $ncpus_cmsearch, $acc, $idx);
  }
  
  die "DONE SUBMITTING JOBS, TIME TO WRITE CODE FOR WAITING FOR FARM";

  # Concatenate files, as we go, check output files to make sure all jobs finished,
  my $all_tblO = "TBLOUT";
  my $all_cmsO = "searchout";
  my $seen_ok = 0;
  my $line;
  my @unlinkA = ();
  open(ALLTBL, ">" . $all_tblO) || die "ERROR unable to open $all_tblO for writing"; 
  open(ALLCMS, ">" . $all_cmsO) || die "ERROR unable to open $all_cmsO for writing"; 
  for($idx = 0; $idx < $nfiles; $idx++) { 
    $tblO = "$idx.tbl";
    $cmsO = "$idx.cmsearch";
    $seen_ok = 0;

    open(TBL, $tblO) || die "ERROR unable to open $tblO for reading";
    while(<TBL>) { print ALLTBL $_; $line = $_; }
    close(TBL);
    push(@unlinkA, $tblO);
    # final line should be "# [ok]"
    chomp $line; if($line ne "# [ok]") { die "ERROR $tblO does not end in \"# [ok]\" line"; }

    open(CMS, $cmsO) || die "ERROR unable to open $cmsO for reading";
    while(<CMS>) { print ALLCMS $_; }
    close(CMS);
    push(@unlinkA, $cmsO);
  }
  close(ALLTBL);
  close(ALLCMS);

  # update DESC with search method
  $famObj->DESC->SM("cmsearch $searchopts CM SEQDB");
}

# update DESC
$io->writeDESC($famObj->DESC);

exit(0);
######################################################################

sub printlog {
  my $m = join( '', @_ );
  my  $time = localtime();
  open( LOG, ">>rfsearch.log" ) or die;
  if ( $m ) {
    printf LOG    "%s [%s]\n", $m, $time;
    printf STDERR "%s [%s]\n", $m, $time;
  } else {
    print LOG "\n";
  }
  close LOG;
}

######################################################################
#youngerThan(file1, file2): test if file1 is younger than file2 
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
	    -b      always run cmbuild (default: only run if 'CM' is v1.0 or doesn't exist)
	    --nostruct     set a zero basepair SS_cons in SEED prior to cmbuild (none must exist in SEED)

            OPTIONS RELATED TO CALIBRATION STEP (cmcalibrate):
	    -c      always run cmcalibrate (default: only run if 'CM' is not calibrated)
            --ccpu <n>     set number of CPUs for MPI cmcalibrate job to <n>

            OPTIONS RELATED TO SEARCH STEP (cmsearch):
            -e <f>  set cmsearch E-value threshold as <f> bits
            --dbchoice <s> set sequence database to search as <s> ('rfamseq' or 'testrfamseq')
            --nosearch     do not run cmsearch
            --scpu <n>     set number of CPUs for cmsearch jobs to <n>
	    --cmos <str> add extra arbitrary option to cmsearch with '-<str>'. (Infernal 1.1, only option is '-g')
            --cmod <str> add extra arbitrary options to cmsearch with '--<str>'. For multiple options use multiple
	                 -cmod lines. Eg. '-cmod cyk -cmod sub' will run cmalign with --cyk and --sub.

            OTHER OPTIONS:
            --queue <s>    submit all jobs to queue <s> 
	    --dirty      leave temporary files, don't clean up
  	    --verbose    print loads of cruft
  	    -h|-help     print this help, then exit
EOF
}

