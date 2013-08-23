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

###################################################################
# Preliminaries:
# - set default values that command line options may change
# - process command line options
# - set input/output file names, and ensure input files exist
# - process DESC file

my $start_time = time();
my $executable = $0;

# set default values that command line options may change
# build related options
my $force_build = 0;            # TRUE to force build
my $do_nostruct = 0;            # TRUE to allow building of CMs with no structure
# calibration related options
my $force_calibrate = 0;        # TRUE to force calibration
my $ncpus_cmcalibrate;          # number of CPUs for cmcalibrate call
# search related options
my $no_search = 0;              # TRUE to skip search
my $no_rev_search = 0;          # TRUE to skip reversed search
my $evalue;                     # cmsearch E-value to use, defined by GetOptions, if -E
my $ncpus_cmsearch;             # number of CPUs for cmsearch calls
my @cmosA = ();                 # extra single '-' cmsearch options (e.g. -g)
my @cmodA = ();                 # extra double '--' cmsearch options (e.g. --cyk)
my @ssoptA = ();                # strings to add to cmsearch qsub/bsub commands
my $ssopt_str = "";             # string to add to cmsearch qsub/bsub commands
# other options
my $q_opt = "";                 # <str> from -queue <str>
my $do_dirty = 0;               # TRUE to not unlink files
my $do_help = 0;                # TRUE to print help and exit, if -h used

# database related options:
my $dbchoice = "r79rfamseq";    # dbchoice, by default 'r79rfamseq'
my $dbfile;                     # defined by GetOptions() if -dbfile is enabled
my $dbdir;                      # defined by GetOptions() if -dbdir is enabled
my $dblist;                     # defined by GetOptions() if -dblist is enabled
my $noZ = 0;                    # set to 1 if -noZ used
my $rev_dbfile;                 # defined by GetOptions() if -rdbfile is enabled
my $rev_dbdir;                  # defined by GetOptions() if -rdbdir is enabled
my $Zuser;                      # defined by GetOptions() if -Z is enabled
my $rev_Zuser;                  # defined by GetOptions() if -rZ is enabled

my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;

open($logFH, ">rfsearch.log") || die "ERROR unable to open rfsearch.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, "build, calibrate, and search a CM against a database", 1);

&GetOptions( "b"          => \$force_build,
	     "nostruct"   => \$do_nostruct,
	     "c"          => \$force_calibrate,
	     "ccpu=s"     => \$ncpus_cmcalibrate,
             "E=s",       => \$evalue,
	     "dbchoice=s" => \$dbchoice,
	     "dbfile=s"   => \$dbfile, 
	     "dbdir=s"    => \$dbdir, 
	     "dblist=s"   => \$dblist, 
	     "noZ"        => \$noZ,
             "Z=s"        => \$Zuser,
	     "rdbfile=s"  => \$rev_dbfile, 
	     "rdbdir=s"   => \$rev_dbdir, 
             "rZ=s"       => \$rev_Zuser,
	     "nosearch"   => \$no_search,
	     "norev"      => \$no_rev_search, 
	     "scpu=s"     => \$ncpus_cmsearch,
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmodA,
             "ssopt=s@"   => \@ssoptA,
	     "dirty"      => \$do_dirty,
             "queue=s"    => \$q_opt, 
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
                                   );
my $msa  = $famObj->SEED;
my $desc = $famObj->DESC;
my $id   = $desc->ID;
my $acc  = $desc->AC;

# extra processing of command-line options 
if ($no_search) { # -nosearch, verify incompatible options are not set
  if (defined $ncpus_cmsearch) { die "ERROR -nosearch and -scpu are incompatible"; }
  if (defined $evalue)         { die "ERROR -nosearch and -E are incompatible"; }
  if (@cmosA)                  { die "ERROR -nosearch and -cmosA are incompatible"; }
  if (@cmodA)                  { die "ERROR -nosearch and -cmodA are incompatible"; }
}

# by default we list user, date, pwd, family, and db choice,
# and information for any command line flags set by
# the user. This block should stay consistent with 
# the GetOptions() call above, and with the help()
# subroutine.
my $cwidth = 40;
my $str;
my $opt;
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# user:", $user));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# date:", $date));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# pwd:", getcwd));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# location:", $config->location));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# family-id:", $id));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# family-acc:", $acc));

if   (defined $dbfile)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# seq db file:",                        "$dbfile" . " [-dbfile]")); }
elsif(defined $dbdir)          { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# seq db dir:",                         "$dbdir" . " [-dbdir]")); }
elsif(defined $dblist)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# seq db list file:",                   "$dblist" . " [-dblist]")); }
else                           { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# seq db:",                             $dbchoice)); }
if($force_build)               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# force cmbuild step:",                 "yes [-b]")); }
if($do_nostruct)               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# allow zero basepair model:",          "yes [-nostruct]")); }
if($force_calibrate)           { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# force cmcalibrate step:",             "yes [-c]")); }
if(defined $ncpus_cmcalibrate) { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# num processors for MPI cmcalibrate:", "$ncpus_cmcalibrate [-ccpu]")); }
if(defined $evalue)            { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# E-value cutoff:",                     $evalue . " [-E]")); }
if(defined $Zuser)             { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# Z (dbsize in Mb):",                   $Zuser . " [-Z]")); }
if(defined $rev_dbfile)        { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# reversed db file:",                   $rev_dbfile . " [-rdbfile]")); }
if(defined $rev_dbdir)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# reversed db dir:",                    $rev_dbdir . " [-rdbdir]")); }
if(defined $rev_Zuser)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# Z (dbsize in Mb) for reversed db:",   $rev_Zuser . " [-rZ]")); }
if($noZ)                       { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# per-database-file E-values:",         "on [-noZ]")); }
if($no_search)                 { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# skip cmsearch stage:",                "yes [-nosearch]")); }
if($no_rev_search)             { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# omit reversed db search:",            "yes [-norev]")); }
if(defined $ncpus_cmsearch)    { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# number of CPUs for cmsearch jobs:",   "$ncpus_cmsearch [-scpu]")); }
$str = ""; foreach $opt (@cmosA) { $str .= $opt . " "; }
if(scalar(@cmosA) > 0)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# single dash cmsearch options:",       $str . "[-cmos]")); }
$str = ""; foreach $opt (@cmodA) { $str .= $opt . " "; }
if(scalar(@cmodA) > 0)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# double dash cmsearch options:",       $str . "[-cmod]")); }
$ssopt_str = ""; foreach $opt (@ssoptA) { $ssopt_str .= $opt . " "; }
if(scalar(@ssoptA) > 0)        { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# add to cmsearch submit commands:",    $ssopt_str . "[-ssopt]")); }
Bio::Rfam::Utils::printToFileAndStdout($logFH, "# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
if($do_dirty)                  { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# do not unlink intermediate files:",   "yes [-dirty]")); }
if($q_opt ne "")               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# submit to queue:",                    "$q_opt [-queue]")); }

# create hash of potential output files
my %outfileH = ();
my @outfile_orderA = ("TBLOUT", "REVTBLOUT", "searchout", "revsearchout", "outlist", "revoutlist", "species", "revspecies", "outlist.pdf", "species.pdf");
$outfileH{"TBLOUT"}       = "concatenated --tblout output from all searches";
$outfileH{"REVTBLOUT"}    = "concatenated --tblout output from reversed searches";
$outfileH{"searchout"}    = "concatenated standard output from all searches";
$outfileH{"revsearchout"} = "concatenated standard output from reversed searches";
$outfileH{"outlist"}      = "sorted list of all hits from TBLOUT";
$outfileH{"revoutlist"}   = "sorted list of all hits from REVTBLOUT";
$outfileH{"species"}      = "same as outlist, but with additional taxonomic information";
$outfileH{"revspecies"}   = "same as revoutlist, but with additional taxonomic information";
$outfileH{"outlist.pdf"}  = "bit score histograms of all hits";
$outfileH{"species.pdf"}  = "bit score histogram hits, colored by taxonomy";

# remove any of these files that currently exist, they're no invalid, since we're now rerunning the search
my $outfile;
foreach $outfile (@outfile_orderA) {
  if (-e $outfile) { 
    unlink $outfile; 
  } 
}

# ncpus_cmsearch and ncpus_cmcalibrate must be >= 0
if (defined $ncpus_cmsearch    && $ncpus_cmsearch    < 0) {
  die "ERROR with -scpu <n>, <n> must be >= 0";
}
if (defined $ncpus_cmcalibrate && $ncpus_cmcalibrate < 0) {
  die "ERROR with -ccpu <n>, <n> must be >= 0";
}

if ($do_nostruct) { # -nostruct: verify SEED either has SS_cons with 0 bps or does not have SS_cons
  if ($msa->has_sscons) {
    my $nbps = $msa->num_basepairs;
    if ($nbps > 0) { die "ERROR with -nostruct, SEED must have zero bp SS_cons, or no SS_cons"; }
    else {  # -nostruct enabled, but msa does not have one, add one
      $msa->set_blank_sscons;
    }
  }
}

if(defined $dbdir)     { $dbdir     =~ s/\/$//; } # remove trailing '/'
if(defined $rev_dbdir) { $rev_dbdir =~ s/\/$//; } # remove trailing '/'

# -Z and -noZ are exclusive
if(defined $Zuser && $noZ) { 
  die "ERROR only one of -Z and -noZ can be used"; 
}
# -dbdir and -dbfile are exclusive
if(defined $dbdir && defined $dbfile) { 
  die "ERROR only one of -dbdir and -dbfile can be used"; 
}
# -dbdir and -dblist are exclusive
if(defined $dbdir && defined $dblist) { 
  die "ERROR only one of -dbdir and -dblist can be used"; 
}
# -dblist and -dbfile are exclusive
if(defined $dblist && defined $dbfile) { 
  die "ERROR only one of -dblist and -dbfile can be used"; 
}
# -rdbdir and -rdbfile are exclusive
if(defined $rev_dbdir && defined $rev_dbfile) { 
  die "ERROR only one of -rdbdir and -rdbfile can be used"; 
}
# if -dbdir used, -Z or -noZ must also be set
if(defined $dbdir && (! defined $Zuser) && (! $noZ)) { 
  die "ERROR -dbdir requires that -Z or -noZ also be used"; 
}
# if -dblist used, -Z or -noZ must also be set
if(defined $dblist && (! defined $Zuser) && (! $noZ)) { 
  die "ERROR -dblist requires that -Z or -noZ also be used"; 
}
# -dbdir and -dbfile are exclusive
if(defined $dbdir && defined $dbfile) { 
  die "ERROR only one of -dbdir and -dbfile can be used"; 
}
# if -rdbdir used, -rZ must also be set
if(defined $rev_dbdir && (! defined $rev_Zuser)) { 
  die "ERROR -rdbdir requires that -rZ also be used to define total reversed database size"; 
}
# if -rdbfile used, -rZ must also be set
if(defined $rev_dbfile && (! defined $rev_Zuser)) { 
  die "ERROR -rdbfile requires that -rZ also be used to define total reversed database size"; 
}
# if -dbdir and -rdbdir both selected they cannot be equal
if(defined $dbdir && defined $rev_dbdir && $dbdir eq $rev_dbdir) { 
  die "ERROR with -dbdir <d1> and -rdbdir <d2>, <d1> cannot be the same as <d2>"; 
}
# if -dbfile and -rdbfile both selected they cannot be equal
if(defined $dbfile && defined $rev_dbfile && $dbfile eq $rev_dbfile) { 
  die "ERROR with -dbfile <f1> and -rdbfile <f2>, <f1> cannot be the same as <f2>"; 
}
# no all gap columns allowed in SEED
if ($msa->any_allgap_columns) { 
  die "ERROR all gap columns exist in SEED";
}


###################################################################################################
Bio::Rfam::Utils::log_output_progress_column_headings($logFH, "per-stage progress:", 1);

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
    (Bio::Rfam::Utils::youngerThan($seedfile, $cmfile))) { # SEED is younger than CM file
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
  if(! $do_dirty) { unlink $outfile; }
  if($buildopts ne "") { $buildopts .= " "; } # add trailing single space so next line properly formats BM (and blank opts ("") will work too)
  $famObj->DESC->BM("cmbuild -F " . $buildopts . "CM SEED");

  # define (or possibly redefine) $cm
  $famObj->CM($io->parseCM("CM"));
  $cm = $famObj->CM($io->parseCM("CM"));
  $is_cm_calibrated = 0;

  $build_wall_secs = time() - $build_start_time;
  $did_build = 1;
  Bio::Rfam::Utils::log_output_progress_local($logFH,   "cmbuild", $build_wall_secs, 0, 1, "", 1);
} # end of if($do_build)
else { 
  $did_build = 0;
  Bio::Rfam::Utils::log_output_progress_skipped($logFH,   "cmbuild", 1);
}

####################
# Calibration step #
####################
my $calibrate_wall_secs     = 0;
my $calibrate_cpu_secs      = 0;
my $calibrate_elp_secs      = 0;
my $calibrate_max_wait_secs = 0;
my $calibrateO     = "c.$$.out"; # cmcalibrate output file
my $calibrate_errO = "c.$$.err"; # error output
my $did_calibrate = 0;
if(! defined $ncpus_cmcalibrate) { $ncpus_cmcalibrate = 81; }
if ($force_calibrate || (! $is_cm_calibrated)) { 
  my $calibrate_start_time = time();
#  Calibration prediction time not currently used, since we can't accurately predict search time anyway
  my $predicted_minutes = Bio::Rfam::Infernal::cmcalibrate_wrapper($config, 
                                                                  "c.$$",               # job name
                                                                   "",                  # options for cmcalibrate 
                                                                   "CM",                # path to CM file
                                                                   $calibrateO,         # path to output file 
                                                                   $calibrate_errO,     # path to error output file 
                                                                   $ncpus_cmcalibrate,  # number of processors
                                                                   $q_opt);             # queue to use, "" for default, ignored if location eq "EBI"
  my @jobnameA = ("c.$$");
  my @outnameA = ("c.$$.out");
  $calibrate_max_wait_secs = Bio::Rfam::Utils::wait_for_cluster($config->location, $user, \@jobnameA, \@outnameA, "[ok]", "cmcalibrate-mpi", $logFH, "[$ncpus_cmcalibrate processors]", -1);
  Bio::Rfam::Utils::checkStderrFile($config->location, $calibrate_errO);
  # if we get here, err file was empty, so we keep going
  if(! $do_dirty) { unlink $calibrate_errO; } # this file is empty anyway 

  $famObj->DESC->CB("cmcalibrate --mpi CM");

  $calibrate_wall_secs = time() - $calibrate_start_time;
  Bio::Rfam::Infernal::process_cpu_times($calibrateO, "# CPU time:", undef, undef, \$calibrate_cpu_secs, \$calibrate_elp_secs);
  my $mpi_overhead_secs = 10;
  $calibrate_elp_secs += $mpi_overhead_secs; # MPI slows things down, and we don't want our efficiency to be lower due to this
  $calibrate_cpu_secs *= $ncpus_cmcalibrate; # cmcalibrate doesn't report total running time
  if(! $do_dirty) { unlink $calibrateO; }

  # done the calibration, we need to redefine $cm
  $famObj->CM($io->parseCM("CM"));
  $cm = $famObj->CM($io->parseCM("CM"));
  if($cm->is_calibrated) { $is_cm_calibrated = 1; }
  else                   { die "Error, CM not calibrated following apparently successful cmcalibrate run"; }

  $did_calibrate = 1;
}
else { 
  $did_calibrate = 0;
  Bio::Rfam::Utils::log_output_progress_skipped($logFH,   "cmcalibrate", 1);
}
  
###############
# Search step #
###############
my $idx;                         # counter
my $search_wall_secs        = 0; # wall time (secs) for search
my $search_cpu_secs         = 0; # CPU time (secs) for all regular (non-reversed) db searches 
my $search_max_wait_secs    = 0; # max time (secs) a job waited on cluster
my $search_max_elp_secs     = 0; # max time (secs) elapsed a regular db search job took
my $rev_search_cpu_secs     = 0; # CPU time (secs) for all reversed db searches
my $rev_search_max_elp_secs = 0; # max time (secs) elapsed a reversed db search job took
my $did_search              = 0;
if (! $no_search) { 
  my $search_start_time = time();

  #################################################################################
  # Database setup; this block is complex because we allow the user to define
  # a database file or directory. If we removed the -dbfile and -dbdir options it
  # would greatly simplify this.

  # first, the regular (non-reversed) database:
  my $ndbfiles = 0;  # number of sequence files to search
  my @dbfileA  = (); # array of seq file names for regular search
  my $dbconfig;      # db info from config, defined if neither -dbfile nor -dbdir used on cmd line
  if(defined $dbfile) { # -dbfile used on command line 
    push(@dbfileA, $dbfile);
  }
  elsif(defined $dbdir) { # -dbdir used on command line
    push(@dbfileA, glob("$dbdir/*.fa $dbdir/*.fa.gz"));
  }
  elsif(defined $dblist) { # -dblist used on command line
    open(DBLIST, $dblist) || die "ERROR unable to open file $dblist (from -dblist option)"; 
    while(my $file = <DBLIST>) { 
      if($file =~ m/\w/) { 
        chomp $file; 
        push(@dbfileA, $file); 
        if(! -e $file) { die "ERROR file $file listed in $dblist does not exist"; }
      }
    }
    close(DBLIST);
  }
  else { # default case: neither -dbfile nor -dbdir used, use database defined in config
    $dbconfig = $config->seqdbConfig($dbchoice);
    for($idx = 0; $idx < $dbconfig->{"nSearchFiles"}; $idx++) { 
      $dbfileA[$idx] = $dbconfig->{"searchPathPrefix"} . ($idx+1) . $dbconfig->{"searchPathSuffix"};
    }
  }
  $ndbfiles = scalar(@dbfileA);

  # setup reversed database to search (this block is analogous to one above for regular (non-reversed) search)
  my $rev_ndbfiles;     # number of reversed seq files to search
  my @rev_dbfileA = (); # array of seq file names for reversed search
  my $rev_dbconfig;     # rev db info from config, defined only if $dbconfig already defined and "revMate" exists
  if(! $no_rev_search) { 
    if(defined $rev_dbfile) { # -dbfile used on command line 
      push(@rev_dbfileA, $rev_dbfile);
    }
    elsif(defined $rev_dbdir) { # -dbdir used on command line
      push(@rev_dbfileA, glob("$rev_dbdir/*.fa $rev_dbdir/*.fa.gz"));
    }
    elsif(defined $dbconfig && defined $dbconfig->{"revMate"}) { 
      # default case: neither -rdbfile nor -rdbdir used, use reversed db defined in config if available
      $rev_dbconfig = $config->revseqdbConfig($dbconfig->{"revMate"});
      for($idx = 0; $idx < $rev_dbconfig->{"nSearchFiles"}; $idx++) { 
        $rev_dbfileA[$idx] = $rev_dbconfig->{"searchPathPrefix"} . ($idx+1) . $rev_dbconfig->{"searchPathSuffix"}; 
      }
    }
  }
  $rev_ndbfiles = scalar(@rev_dbfileA);
  # note that if -dbdir, -dbdir or -dbfile used, no reversed searches are done unless -rdbfile or -rdbdir
  #
  # end of database setup
  #################################################################################

  #################################################################################
  # Determine cmsearch command line options:
  # 
  # First, determine database size, if nec
  my $dbsize;        # database size in Mb
  my $rev_dbsize;    # reversed database size in Mb
  my $Zopt;          # -Z option for cmsearch 
  my $rev_Zopt;      # -Z option for cmsearch of reversed DBs
  if (defined $Zuser)      { $dbsize = $Zuser;  }
  elsif(defined $dbconfig) { $dbsize = $dbconfig->{"dbSize"}; }

  if(defined $dbsize) { $Zopt = " -Z $dbsize "; }
  else                { $Zopt = ""; }

  if (defined $rev_Zuser)      { $rev_dbsize = $rev_Zuser;  }
  elsif(defined $rev_dbconfig) { $rev_dbsize = $rev_dbconfig->{"dbSize"}; }

  if(defined $rev_dbsize) { $rev_Zopt = " -Z $rev_dbsize "; }
  else                    { $rev_Zopt = ""; }
  # use -FZ <x> with <x> equal to regular database size, to ensure we use same filter settings
  if(defined $dbsize)     { $rev_Zopt .= " --FZ $dbsize "; }

  # Now that we know database size, determine bit score or E-value threshold to use.
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
  my $max_evalue     = 50000; # hard-coded max E-value allowed, not applied if -E used
  my $min_bitsc      = 0;     # bit score corresponding to $max_eval, set below
  my $min_evalue     = 1000;  # hard-coded min E-value allowed, not applied if -E used

  if (defined $evalue) {      # -cme option set on cmdline
    $use_cmsearch_evalue = 1;
    $cmsearch_evalue     = $evalue;
  }
  elsif(! defined $dbsize) { # if we don't know the database size (probably b/c -dbfile, -dbdir or -dblist was set), use -E 1000
    $use_cmsearch_evalue = 1;
    $cmsearch_evalue     = $min_evalue;
  } 
  else  { # -E not set, and database size is known
    # set default as Case 2, we'll change if nec below:
    $cmsearch_evalue     = 1000;
    $use_cmsearch_evalue = 1;
    # get GA and check to see if cases 3 or 4 apply
    $ga_bitsc  = $famObj->DESC->CUTGA; 
    $e_bitsc   = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $cmsearch_evalue, $dbsize);
    $min_bitsc = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $max_evalue,      $dbsize);
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

  # use same reporting threshold for regular and reversed searches
  my $searchopts     = "--cpu $ncpus_cmsearch --verbose";
  my $rev_searchopts = "--cpu $ncpus_cmsearch --verbose";
  if ($use_cmsearch_evalue) { $searchopts .= " -E $cmsearch_evalue "; }
  else                      { $searchopts .= " -T $cmsearch_bitsc ";  }

  $rev_searchopts  = $searchopts . $rev_Zopt;
  $searchopts     .= $Zopt;

  my $extra_options = Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  $searchopts     .= $extra_options;
  $rev_searchopts .= $extra_options;
  $searchopts      =~ s/\s+/ /g; # replace multiple spaces with single spaces
  $searchopts      =~ s/\s+$//;  # remove trailing spaces
  $searchopts      =~ s/^\s+//;  # remove leading spaces
  $rev_searchopts  =~ s/\s+/ /g; # replace multiple spaces with single spaces
  $rev_searchopts  =~ s/\s+$//;  # remove trailing spaces
  $rev_searchopts  =~ s/^\s+//;  # remove leading spaces
  # 
  # End of block defining cmsearch options
  #################################################################################

  #################################################################################
  # Submit cmsearch jobs, wait for them to finish, and process their output.
  
  my @jobnameA     = (); # names of jobs for regular searches
  my @tblOA        = (); # names of --tblout files for regular searches
  my @cmsOA        = (); # names of cmsearch output files for regular searches
  my @errOA        = (); # names of error files for regular searches
  my @rev_jobnameA = (); # names of jobs for reversed searches
  my @rev_tblOA    = (); # names of --tblout files for reversed searches
  my @rev_cmsOA    = (); # names of cmsearch output files for reversed searches
  my @rev_errOA    = (); # names of error files for reversed searches

  submit_cmsearch_jobs($config, $ndbfiles, "s.",  $searchopts, $cmfile, \@dbfileA, \@jobnameA, \@tblOA, \@cmsOA, \@errOA, $ssopt_str, $q_opt);
  if($rev_ndbfiles > 0) { 
    submit_cmsearch_jobs($config, $rev_ndbfiles, "rs.", $rev_searchopts, $cmfile, \@rev_dbfileA, \@rev_jobnameA, \@rev_tblOA, \@rev_cmsOA, \@rev_errOA, $ssopt_str, $q_opt);
  }
  my @all_jobnameA = @jobnameA;
  my @all_tblOA    = @tblOA;
  my @all_errOA    = @errOA;
  if($rev_ndbfiles > 0) { 
    push(@all_jobnameA, @rev_jobnameA);
    push(@all_tblOA,    @rev_tblOA);
    push(@all_errOA,    @rev_errOA);
  }
  # wait for cluster jobs to finish
  $search_max_wait_secs = Bio::Rfam::Utils::wait_for_cluster($config->location, $user, \@all_jobnameA, \@all_tblOA, "# [ok]", "cmsearch", $logFH, "", -1);
  $search_wall_secs     = time() - $search_start_time;
  
  # concatenate files (no need to validate output, we already did that in wait_for_cluster())
  my $all_errO     = "searcherr";
  my $all_tblO     = "TBLOUT";
  my $all_rev_tblO = "REVTBLOUT";
  my $all_cmsO     = "searchout";
  my $all_rev_cmsO = "revsearchout";
  # first, create the concatenated error file, if it's not empty we'll die before creating TBLOUT
  Bio::Rfam::Utils::concatenate_files(\@all_errOA, $all_errO, (! $do_dirty)); # '1' says delete original files after concatenation, unless -dirty
  if(-s $all_errO) { 
    Bio::Rfam::Utils::checkStderrFile($config->location, $all_errO); 
  }

  # if we get here, all err files were empty, so we keep going
  if(! $do_dirty) { unlink $all_errO; } # this file is empty anyway
  Bio::Rfam::Utils::concatenate_files(\@tblOA,     $all_tblO,     (! $do_dirty)); # '! $do_dirty' says delete original files after concatenation, unless -dirty
  Bio::Rfam::Utils::concatenate_files(\@cmsOA,     $all_cmsO,     (! $do_dirty)); # '! $do_dirty' says delete original files after concatenation, unless -dirty
  if($rev_ndbfiles > 0) { 
    Bio::Rfam::Utils::concatenate_files(\@rev_tblOA, $all_rev_tblO, (! $do_dirty)); # '! $do_dirty' says delete original files after concatenation, unless -dirty
    Bio::Rfam::Utils::concatenate_files(\@rev_cmsOA, $all_rev_cmsO, (! $do_dirty)); # '! $do_dirty' says delete original files after concatenation, unless -dirty
  }

  # update DESC with search method
  if($searchopts ne "") { $searchopts .= " "; } # add trailing single space so next line properly formats SM (and blank opts ("") will work too)
  $famObj->DESC->SM("cmsearch " . $searchopts . "CM SEQDB");

  # timing info
  $search_wall_secs = time() - $search_start_time;
  Bio::Rfam::Infernal::process_cpu_times($all_cmsO, "Total CPU time:", undef, \$search_max_elp_secs,     \$search_cpu_secs,     undef);
  Bio::Rfam::Infernal::process_cpu_times($all_cmsO, "Total CPU time:", undef, \$rev_search_max_elp_secs, \$rev_search_cpu_secs, undef);
  if($rev_search_max_elp_secs > $search_max_elp_secs) { $search_max_elp_secs = $rev_search_max_elp_secs; }
  $search_cpu_secs += $rev_search_cpu_secs;
  $did_search = 1;

  # write TBLOUT-dependent files
  my $require_tax = 0;
  if(defined $dbconfig) { $require_tax; } # we require tax info if we're doing standard search against a db in the config
  $io->writeTbloutDependentFiles($famObj, $config->rfamlive, $famObj->SEED, $famObj->DESC->CUTGA, $config->RPlotScriptPath, $require_tax);

  # End of block for submitting and processing cmsearch jobs
  #################################################################################
}
# update DESC
if($did_build || $did_calibrate || $did_search) { 
  $io->writeDESC($famObj->DESC);
}

# finished all work, print output file summary
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, 1);
my $description;
if($did_build || $did_calibrate) { 
  $description = sprintf("covariance model file (%s)", $did_build ? "built and calibrated" : "calibrated only");
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "CM", $description, 1);
}
if($did_build || $did_calibrate || $did_search) { 
  $description = sprintf("desc file (updated:%s%s%s)", 
                         ($did_build)     ? " BM" : "", 
                         ($did_calibrate) ? " CB" : "", 
                         ($did_search)    ? " SM" : "");
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "DESC", $description, 1);
}

# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
foreach $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, 1);
  }
}
$description = sprintf("log file (*this* output)");
Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfsearch.log", $description, 1);

# output time summary
Bio::Rfam::Utils::log_output_timing_summary_column_headings($logFH, 1);

my $total_wall_secs = time() - $start_time;
my $total_cpu_secs  = $build_wall_secs + $calibrate_cpu_secs + $search_cpu_secs;
my $total_elp_secs  = $build_elp_secs + $calibrate_elp_secs + $search_max_elp_secs;

if($did_build) { 
  Bio::Rfam::Utils::log_output_timing_summary($logFH,   "cmbuild", $build_wall_secs, $build_elp_secs, "-", $build_elp_secs, 1);
}
if($did_calibrate) { 
  Bio::Rfam::Utils::log_output_timing_summary($logFH,   "cmcalibrate", $calibrate_wall_secs, $calibrate_cpu_secs, $calibrate_max_wait_secs, $calibrate_elp_secs, 1);
}
if($did_search) { 
  Bio::Rfam::Utils::log_output_timing_summary($logFH,   "cmsearch", $search_wall_secs, $search_cpu_secs, $search_max_wait_secs, $search_max_elp_secs, 1);
}
if($did_build || $did_calibrate || $did_search) { 
  Bio::Rfam::Utils::log_output_timing_summary($logFH,   "total", $total_wall_secs, $total_cpu_secs, "-", $total_elp_secs, 1);
}

my $outstr = "#\n";
printf $outstr; print $logFH $outstr;

$outstr = sprintf("# Total time elapsed: %s\n", Bio::Rfam::Utils::format_time_string(time() - $start_time));
printf $outstr; print $logFH $outstr;

$outstr = "# [ok]\n";
printf $outstr; print $logFH $outstr;

close($logFH);
exit(0);


######################################################################

sub submit_cmsearch_jobs {
  my ($config, $ndbfiles, $prefix, $searchopts, $cmfile, $dbfileAR, $jobnameAR, $tblOAR, $cmsOAR, $errOAR, $ssopt_str, $q_opt) = @_;
  my ($idx, $file_idx, $dbfile);

  for($idx = 0; $idx < $ndbfiles; $idx++) { 
    $file_idx = $idx + 1; # off-by-one w.r.t $idx, because database file names are 1..$ndbfiles, not 1..$ndbfiles-1
    $jobnameAR->[$idx] = $prefix . "$$.$file_idx";  
    $tblOAR->[$idx]    = $prefix . "$$.$file_idx.tbl";
    $cmsOAR->[$idx]    = $prefix . "$$.$file_idx.cmsearch";
    $errOAR->[$idx]    = $prefix . "$$.$file_idx.err";
    Bio::Rfam::Infernal::cmsearch_wrapper($config, $jobnameAR->[$idx], "--tblout " . $tblOAR->[$idx] . " " . $searchopts, $cmfile, $dbfileAR->[$idx], $cmsOAR->[$idx], $errOAR->[$idx], $ssopt_str, $q_opt);  
  }
}

sub help {
  print STDERR <<EOF;

rfsearch.pl: builds, calibrates and searches a CM against a sequence database.
             Run from within a directory containing "SEED" & "DESC" files. 
	     E.g., after running "rfupdate.pl RFXXXXX" or "rfco.pl RFXXXXX".
	     SEED contains a stockholm format alignment and DESC is an internal 
	     Rfam documentation describing each RNA family. 

             By default, the 'rfamseq' database is searched and the 'revrfamseq' database
             is used as the 'reversed' database. The E-value of the top hit in the reversed 
             database will be annotated in the \'outlist\' file, as an indication of the 
             expected score of a high scoring false positive for this model. By default, if
             the -dbfile, -dbdir, or -dblist option is used then no reversed database will be
             searched, unless the -rdbfile or -rdbdir option is also used.

Usage:      rfsearch.pl [options]

Options:    OPTIONS RELATED TO BUILD STEP (cmbuild):
	    -b         always run cmbuild (default: only run if 'CM' is v1.0 or doesn't exist)
	    -nostruct  set a zero basepair SS_cons in SEED prior to cmbuild (none must exist in SEED)

            OPTIONS RELATED TO CALIBRATION STEP (cmcalibrate):
	    -c         always run cmcalibrate (default: only run if 'CM' is not calibrated)
            -ccpu <n>  set number of CPUs for MPI cmcalibrate job to <n>

            OPTIONS RELATED TO SEARCH STEP (cmsearch):
            -E <f>        set cmsearch E-value threshold as <f>
            -nosearch     do not run cmsearch
            -norev        do not run cmsearch on reversed database files
            -scpu <n>     set number of CPUs for cmsearch jobs to <n>
	    -cmos <str>   add extra arbitrary option to cmsearch with '-<str>'. (Infernal 1.1, only option is '-g')
            -cmod <str>   add extra arbitrary options to cmsearch with '--<str>'. For multiple options use multiple
	                   -cmod lines. e.g. '-cmod toponly -cmod anytrunc' will run cmsearch with --toponly and --anytrunc.

            OPTIONS SPECIFYING SEARCH DATABASE:
            -dbchoice  <s> set sequence database to search as <s> ('rfamseq', 'testrfamseq', 'r79rfamseq')
            -dbfile <s>    set sequence database to search as file <s>
            -dbdir <s>     set sequence database to search as all '.fa' and '.fa.gz' suffixed files in dir <s>
            -dblist <s>    set sequence database to search as all files listed in dir <s>
            -noZ           with -dbdir or -dblist, do not set database size, E-values will pertain to per-file searches
            
            OPTIONS SPECIFYING REVERSED SEQUENCE DATABASE TO SEARCH:
            -rdbfile <s>   set reversed sequence database to search as file <s>
            -rdbdir <s>    set reversed sequence database to search as all '.fa' and '.fa.gz' suffixed files in dir <s>

            OTHER OPTIONS:
            -ssopt <str> add extra arbitrary string <str> to qsub cmsearch commands, for multiple options use multiple -ssopt <s>
            -queue <str> specify queue to submit job to as <str> (EBI \'-q <str>\' JFRC: \'-l <str>=true\')
  	    -dirty       do not remove temporary/intermediate files that are normally removed
  	    -h|-help     print this help, then exit
EOF
}

