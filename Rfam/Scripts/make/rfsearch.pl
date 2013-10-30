#!/usr/bin/env perl
use strict;
use Cwd;
use Getopt::Long;
use File::stat;
use Data::Printer;
use File::Copy;
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
my $only_build  = 0;            # TRUE to only build and then exit
my $do_noss     = 0;            # TRUE to allow building of CMs with no structure
my $do_hand     = 0;            # TRUE to pass --hand to cmbuild
my $do_enone    = 0;            # TRUE to pass --enone to cmbuild
my $ignore_bm   = 0;            # TRUE to ignore BM in DESC for cmbuild options
# calibration related options
my $force_calibrate = 0;        # TRUE to force calibration
my $ncpus_cmcalibrate;          # number of CPUs for cmcalibrate call
# search related options
my $no_search = 0;              # TRUE to skip search
my $no_rev_search = 0;          # TRUE to skip reversed search
my $e_opt;                      # cmsearch E-value to use, defined by GetOptions, if -e
my $t_opt;                      # cmsearch bit score cutoff to use, defined by GetOptions, if -t
my $do_cutga = 0;               # TRUE to use GA threshold as bit score cutoff
my $ncpus_cmsearch;             # number of CPUs for cmsearch calls
my @cmosA = ();                 # extra single '-' cmsearch options (e.g. -g)
my @cmodA = ();                 # extra double '--' cmsearch options (e.g. --cyk)
my @ssoptA = ();                # strings to add to cmsearch qsub/bsub commands
my $ssopt_str = "";             # string to add to cmsearch qsub/bsub commands
my $ignore_sm = 0;              # TRUE to ignore BM in DESC for cmbuild options
# other options
my $q_opt = "";                 # <str> from -q <str>
my $do_dirty = 0;               # TRUE to not unlink files
my $do_stdout = 1;              # TRUE to output to STDOUT
my $do_quiet  = 0;              # TRUE to not output anything to STDOUT
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

&GetOptions( "b"          => \$force_build,
             "onlybuild"  => \$only_build,
	     "noss"       => \$do_noss,
	     "hand"       => \$do_hand,
	     "enone"      => \$do_enone,
	     "ignorebm"   => \$ignore_bm,
	     "c"          => \$force_calibrate,
	     "ccpu=s"     => \$ncpus_cmcalibrate,
             "e=s",       => \$e_opt,
             "t=s",       => \$t_opt,
	     "nosearch"   => \$no_search,
	     "norev"      => \$no_rev_search, 
	     "scpu=s"     => \$ncpus_cmsearch,
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmodA,
	     "ignoresm"   => \$ignore_sm,
	     "dbchoice=s" => \$dbchoice,
	     "dbfile=s"   => \$dbfile, 
	     "dbdir=s"    => \$dbdir, 
	     "dblist=s"   => \$dblist, 
	     "noZ"        => \$noZ,
             "Z=s"        => \$Zuser,
	     "rdbfile=s"  => \$rev_dbfile, 
	     "rdbdir=s"   => \$rev_dbdir, 
             "rZ=s"       => \$rev_Zuser,
             "q=s"        => \$q_opt, 
             "ssopt=s@"   => \@ssoptA,
             "quiet",     => \$do_quiet,
	     "dirty"      => \$do_dirty,
	     "h|help"     => \$do_help );

$do_stdout = ($do_quiet) ? 0 : 1;
open($logFH, ">rfsearch.log") || die "ERROR unable to open rfsearch.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, "build, calibrate, and search a CM against a database", $do_stdout);

if ( $do_help ) {
  &help();
  exit(1);
}

# output header
my $user  = getpwuid($<);
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
if ($only_build) { # -onlybuild, verify incompatible options are not set
  if (defined $ncpus_cmsearch) { die "ERROR -onlybuild and -scpu are incompatible"; }
  if (defined $e_opt)          { die "ERROR -onlybuild and -e are incompatible"; }
  if (defined $t_opt)          { die "ERROR -onlybuild and -t are incompatible"; }
  if (defined $do_cutga)       { die "ERROR -onlybuild and -cut_ga are incompatible"; }
  if (@cmosA)                  { die "ERROR -onlybuild and -cmosA are incompatible"; }
  if (@cmodA)                  { die "ERROR -onlybuild and -cmodA are incompatible"; }
}
if ($no_search) { # -nosearch, verify incompatible options are not set
  if (defined $ncpus_cmsearch) { die "ERROR -nosearch and -scpu are incompatible"; }
  if (defined $e_opt)          { die "ERROR -nosearch and -e are incompatible"; }
  if (defined $t_opt)          { die "ERROR -nosearch and -t are incompatible"; }
  if (defined $do_cutga)       { die "ERROR -nosearch and -cutga are incompatible"; }
  if (@cmosA)                  { die "ERROR -nosearch and -cmosA are incompatible"; }
  if (@cmodA)                  { die "ERROR -nosearch and -cmodA are incompatible"; }
}
if ((defined $e_opt) && (defined $t_opt)) { die "ERROR you can't use both -t and -e, pick one"; }
if ((defined $e_opt) && ($do_cutga))      { die "ERROR you can't use both -e and -cut_ga, pick one"; }
if ((defined $t_opt) && ($do_cutga))      { die "ERROR you can't use both -t and -cut_ga, pick one"; }
# A few complicated checks about the search thresholds, we want to do this here, before
# the build step (and not wait til the search step) so we exit early and don't waste 
# the user's time.
# if a threshold already exists in SM (-E or -T) and rfsearch.pl -e or -t was used without -ignoresm, then die
my $desc_searchopts  = strip_default_options_from_sm($desc->{'SM'});
my $t_sm = undef;
my $e_sm = undef;
if($desc_searchopts =~/\s*\-T\s+(\S+)\s+/) { $t_sm = $1; }
if($desc_searchopts =~/\s*\-E\s+(\S+)\s+/) { $e_sm = $1; }
if(defined $t_sm && defined $e_sm) { die "ERROR, DESC SM has both -T and -E!"; }

if(! $ignore_sm) { 
  if(defined $e_opt) { 
    if(defined $t_sm) { die "ERROR, multiple thresholds: -e set at cmdline, but -T $t_sm already exists in SM, consider -ignoresm"; }
    if(defined $e_sm) { die "ERROR, multiple thresholds: -e set at cmdline, but -E $e_sm already exists in SM, consider -ignoresm"; }
  }
  elsif(defined $t_opt) { 
    if(defined $t_sm) { die "ERROR, multiple thresholds: -t set at cmdline, but -T $t_sm already exists in SM, consider -ignoresm"; }
    if(defined $e_sm) { die "ERROR, multiple thresholds: -t set at cmdline, but -E $e_sm already exists in SM, consider -ignoresm"; }
  }
  elsif($do_cutga) { 
    if(defined $t_sm) { die "ERROR, multiple thresholds: -cut_ga set at cmdline, but -T $t_sm already exists in SM, consider -ignoresm"; }
    if(defined $e_sm) { die "ERROR, multiple thresholds: -cut_ga set at cmdline, but -E $e_sm already exists in SM, consider -ignoresm"; }
  }
}
# if a threshold DOES NOT exist in SM (-E or -T), then user MUST use -t, -e or -cut_ga
if((! defined $t_sm) && (! defined $e_sm)) { 
  if((! defined $e_opt) && (! defined $t_opt) && (! $do_cutga)) { 
    die "ERROR, no threshold set in SM, you must use one of: -t, -e, or -cut_ga"; 
  }
}
# make sure that user didn't specify -T, -E, --cut_ga, --cut_tc, --cut_nc with -cmos or -cmod
my $extra_searchopts = Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
if($extra_searchopts =~/\s*\-T^\s+(\S+)\s+/) { die "ERROR, -cmos T is not allowed, use -t"; }
if($extra_searchopts =~/\s*\-E^\s+(\S+)\s+/) { die "ERROR, -cmos E is not allowed, use -e"; }
if($extra_searchopts =~/\-\-cut_ga/) { die "ERROR, -cmod cut_ga is not allowed, use -cut_ga"; }
if($extra_searchopts =~/\-\-cut_nc/) { die "ERROR, -cmod cut_nc is not allowed, only -t, -e, or -cut_ga are allowed"; }
if($extra_searchopts =~/\-\-cut_tc/) { die "ERROR, -cmod cut_tc is not allowed, only -t, -e, or -cut_ga are allowed"; }
# ncpus_cmsearch and ncpus_cmcalibrate must be >= 0
if (defined $ncpus_cmsearch    && $ncpus_cmsearch    < 0) {
  die "ERROR with -scpu <n>, <n> must be >= 0";
}
if (defined $ncpus_cmcalibrate && $ncpus_cmcalibrate < 0) {
  die "ERROR with -ccpu <n>, <n> must be >= 0";
}
# if -hand, make sure SEED has RF annotation, if not die in error
if ($do_hand) { 
  if(! $msa->has_rf) { die "ERROR, -hand requires RF annotation in SEED, but none exists"; }
}
# if -noss used, rewrite SEED's SS_cons as blank, and exit
if ($do_noss) { # -noss: rewrite SEED's SS_cons as blank
  # copy existing SEED sideways
  if(-e "SEED") { copy("SEED", "SEED.$$"); }
  $msa->set_blank_ss_cons;
  $msa->write_msa("SEED");
  # TODO: update this so, we read SEED back in and keep going, instead of exiting
  Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, $do_stdout);
  Bio::Rfam::Utils::log_output_file_summary($logFH, "SEED.$$", "copy of old SEED file from before this rfsearch", $do_stdout);
  Bio::Rfam::Utils::log_output_file_summary($logFH, "SEED",    "rewritten SEED, with zero basepair SS_cons annotation", $do_stdout);
  close($logFH);
  exit(0);
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
# done with command line option processing
#
#
#
# determine if we'll udpate the DESC file at the end of the script
my $dbconfig = undef;     # db info from config, defined if neither -dbfile nor -dbdir used on cmd line
my $do_update_desc = 0;   # should we update DESC at end of script? Only if dbfile, dbdir, and dblist are all undefined and dbconfig->{"updateDesc"} is 1
if((! defined $dbfile) && # -dbfile not set
   (! defined $dbdir)  && # -dbdir not set
   (! defined $dblist))   # -dblist not set
{
  $dbconfig = $config->seqdbConfig($dbchoice);
  $do_update_desc = $dbconfig->{"updateDesc"};
}
#
# ============================================
# 
# by default we list user, date, pwd, family, and db choice,
# and information for any command line flags set by
# the user. This block should stay consistent with 
# the GetOptions() call above, and with the help()
# subroutine.
my $cwidth = 40;
my $str;
my $opt;
if($do_quiet) { $do_stdout = 0; }
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# user:", $user), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# date:", $date), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# pwd:", getcwd), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# location:", $config->location), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# family-id:", $id), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# family-acc:", $acc), $do_stdout);

if   (defined $dbfile)         { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# seq db file:",                        "$dbfile" . " [-dbfile]"), $do_stdout); }
elsif(defined $dbdir)          { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# seq db dir:",                         "$dbdir" . " [-dbdir]"), $do_stdout); }
elsif(defined $dblist)         { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# seq db list file:",                   "$dblist" . " [-dblist]"), $do_stdout); }
else                           { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# seq db:",                             $dbchoice), $do_stdout); }
if($force_build)               { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# force cmbuild step:",                 "yes [-b]"), $do_stdout); }
if($only_build)                { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# build-only mode:",                    "on [-onlybuild]"), $do_stdout); }
if($do_noss)                   { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# rewrite SEED with zero bp SS_cons:",  "yes [-noss]"), $do_stdout); }
if($do_hand)                   { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# pass --hand to cmbuild:",             "yes [-hand]"), $do_stdout); }
if($do_enone)                  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# pass --enone to cmbuild:",            "yes [-enone]"), $do_stdout); }
if($ignore_bm)                 { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# ignore DESC's BM line:",              "yes [-ignorebm]"), $do_stdout); }
if($force_calibrate)           { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# force cmcalibrate step:",             "yes [-c]"), $do_stdout); }
if(defined $ncpus_cmcalibrate) { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# num processors for MPI cmcalibrate:", "$ncpus_cmcalibrate [-ccpu]"), $do_stdout); }
if(defined $e_opt)             { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# E-value cutoff:",                     $e_opt . " [-e]"), $do_stdout); }
if(defined $t_opt)             { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# bit score cutoff:",                   $t_opt . " [-t]"), $do_stdout); }
if($do_cutga)                  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# use GA bit score threshold:",         "yes [-cut_ga]"), $do_stdout); }
if(defined $Zuser)             { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# Z (dbsize in Mb):",                   $Zuser . " [-Z]"), $do_stdout); }
if(defined $rev_dbfile)        { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# reversed db file:",                   $rev_dbfile . " [-rdbfile]"), $do_stdout); }
if(defined $rev_dbdir)         { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# reversed db dir:",                    $rev_dbdir . " [-rdbdir]"), $do_stdout); }
if(defined $rev_Zuser)         { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# Z (dbsize in Mb) for reversed db:",   $rev_Zuser . " [-rZ]"), $do_stdout); }
if($noZ)                       { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# per-database-file E-values:",         "on [-noZ]"), $do_stdout); }
if($no_search)                 { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# skip cmsearch stage:",                "yes [-nosearch]"), $do_stdout); }
if($no_rev_search)             { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# omit reversed db search:",            "yes [-norev]"), $do_stdout); }
if(defined $ncpus_cmsearch)    { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# number of CPUs for cmsearch jobs:",   "$ncpus_cmsearch [-scpu]"), $do_stdout); }
$str = ""; foreach $opt (@cmosA) { $str .= $opt . " "; }
if(scalar(@cmosA) > 0)         { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# single dash cmsearch options:",       $str . "[-cmos]"), $do_stdout); }
$str = ""; foreach $opt (@cmodA) { $str .= $opt . " "; }
if(scalar(@cmodA) > 0)         { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# double dash cmsearch options:",       $str . "[-cmod]"), $do_stdout); }
$ssopt_str = ""; foreach $opt (@ssoptA) { $ssopt_str .= $opt . " "; }
if(scalar(@ssoptA) > 0)        { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# add to cmsearch submit commands:",    $ssopt_str . "[-ssopt]"), $do_stdout); }
if($ignore_sm)                 { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# ignore DESC's SM line:",              "yes [-ignoresm]"), $do_stdout); }
if($do_quiet)                  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# quiet mode: ",                        "on  [-quiet]"), $do_stdout); }
if($do_dirty)                  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# do not unlink intermediate files:",   "yes [-dirty]"), $do_stdout); }
if($q_opt ne "")               { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# submit to queue:",                    "$q_opt [-q]"), $do_stdout); }
if(! $do_update_desc)          { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# updating DESC at end of script:",     "no"); }

Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n", $do_stdout);

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
$outfileH{"species.pdf"}  = "bit score histogram of all hits, colored by taxonomy";

# remove any of these files that currently exist, they're no invalid, since we're now rerunning the search
my $outfile;
foreach $outfile (@outfile_orderA) {
  if (-e $outfile) { 
    unlink $outfile; 
  } 
}

Bio::Rfam::Utils::log_output_progress_column_headings($logFH, "per-stage progress:", $do_stdout);

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
if (($force_build)        ||                # user set -b on command line
    ($only_build)         ||                # user set -onlybuild on command line
    (! defined $cm)       ||                # 'CM' does not exist
    (! $is_cm_calibrated) ||                # 'CM' is not calibrated
    ($do_noss || $do_hand || $do_enone)  || # -noss, -hand or -enone set on cmdline
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

  if (! defined $buildopts) { die "ERROR unable to read BM (build method) from DESC"; }
  if ($ignore_bm) { $buildopts = ""; } # ignore the BM line

  $buildopts =~ s/\s*-F\s*/ /;   # remove -F,     cmbuild_wrapper will automatically add this
  $buildopts =~ s/\s*CM\s*/ /;   # remove 'CM',   cmbuild_wrapper will automatically add this
  $buildopts =~ s/\s*SEED\s*/ /; # remove 'SEED', cmbuild_wrapper will automatically add this
  $buildopts =~ s/cmbuild//;     # remove 'cmbuild'
  
  # add in user options, two possibilities: --hand and --enone
  if($do_hand)  { $buildopts .= " --hand";  }
  if($do_enone) { $buildopts .= " --enone"; }

  # clean up buildopts string
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
  Bio::Rfam::Utils::log_output_progress_local($logFH,   "cmbuild", $build_wall_secs, 0, 1, "", $do_stdout);
} # end of if($do_build)
else { 
  $did_build = 0;
  Bio::Rfam::Utils::log_output_progress_skipped($logFH,   "cmbuild", $do_stdout);
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
my $do_calibrate = (! $only_build) && ($force_calibrate || (! $is_cm_calibrated)) ? 1 : 0;
if($do_calibrate) { 
  my $calibrate_start_time = time();
#  Calibration prediction time not currently used, since we can't accurately predict search time anyway
  my $predicted_minutes = Bio::Rfam::Infernal::cmcalibrate_wrapper($config, 
                                                                  "c.$$",               # job name
                                                                   "",                  # options for cmcalibrate, NOTE: we don't allow ANY 
                                                                   "CM",                # path to CM file
                                                                   $calibrateO,         # path to output file 
                                                                   $calibrate_errO,     # path to error output file 
                                                                   $ncpus_cmcalibrate,  # number of processors
                                                                   $q_opt);             # queue to use, "" for default, ignored if location eq "EBI"
  my @jobnameA = ("c.$$");
  my @outnameA = ("c.$$.out");
  $calibrate_max_wait_secs = Bio::Rfam::Utils::wait_for_cluster($config->location, $user, \@jobnameA, \@outnameA, "[ok]", "cmcalibrate-mpi", $logFH, 
                                                                sprintf("[$ncpus_cmcalibrate procs, should take ~%.0f minute(s)]", $predicted_minutes), -1, $do_stdout);
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
  Bio::Rfam::Utils::log_output_progress_skipped($logFH,   "cmcalibrate", $do_stdout);
}
  
###############
# Search step #
###############
my $idx;                         # counter
my $search_wall_secs        = 0; # wall time (secs) for search
my $search_cpu_secs         = 0; # CPU time (secs) for all regular (non-reversed) db searches 
my $search_max_wait_secs    = 0; # max time (secs) a job waited on cluster
my $search_max_cpu_secs     = 0; # max CPU time (secs) a regular db search job took
my $search_max_elp_secs     = 0; # max time (secs) elapsed a regular db search job took
my $rev_search_cpu_secs     = 0; # CPU time (secs) for all reversed db searches
my $rev_search_max_cpu_secs = 0; # max CPU time (secs) a reversed db search job took
my $rev_search_max_elp_secs = 0; # max time (secs) elapsed a reversed db search job took
my $ndbfiles                = 0; # number of db files searched (number of cmsearch calls)
my $rev_ndbfiles            = 0; # number of reversed db files searched (number of cmsearch calls)
my $did_search              = 0;

if ((! $only_build) && (! $no_search)) { 
  my $search_start_time = time();

  #################################################################################
  # Database setup; this block is complex because we allow the user to define
  # a database file or directory. If we removed the -dbfile and -dbdir options it
  # would greatly simplify this.

  # first, the regular (non-reversed) database:
  $ndbfiles = 0;          # number of sequence files to search
  my @dbfileA  = ();      # array of seq file names for regular search
  if(defined $dbfile) {  # -dbfile used on command line 
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
    # we defined $dbconfig earlier, when we determined if we need to update DESC at end of script or not
    for($idx = 0; $idx < $dbconfig->{"nSearchFiles"}; $idx++) { 
      $dbfileA[$idx] = $dbconfig->{"searchPathPrefix"} . ($idx+1) . $dbconfig->{"searchPathSuffix"};
    }
  }
  $ndbfiles = scalar(@dbfileA);

  # setup reversed database to search (this block is analogous to one above for regular (non-reversed) search)
  $rev_ndbfiles;     # number of reversed seq files to search
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
  if($rev_ndbfiles == 0) { $no_rev_search = 1; }
  # note that if -dbdir, -dbdir or -dbfile used, no reversed searches are done unless -rdbfile or -rdbdir
  #
  # end of database setup
  #################################################################################

  #################################################################################
  # Determine cmsearch command line options:
  # 
  # First, determine database size, if nec.
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
  if(defined $dbsize) { $rev_Zopt .= " --FZ $dbsize "; }

  # We want to use the options in DESC's SM unless user set -ignoresm
  # remove default options that always get set automatically by rfsearch:
  # -Z <f>, --FZ <f>, --cpu <n>, --verbose, --nohmmonly, 
  # we defined $desc_searchopts way above, when we were processing cmdline options

  # Currently we may need to convert a search E-value threshold in the DESC's SM
  # to a bit score. To do that we need to know the size of rfamseq, which is
  # the only type of search that the SM line can be set on.
  # Hopefully, in the future, will disallow -E in the SM lines, and always use
  # -T (in fact this script only writes -T to DESC's SM). This needs to be revisited
  # when all families are done being rethresholded (and all DESC's SM's contain -T
  # and not -E). EPN, Tue Oct 29 14:38:07 2013
  my $rfamseq_dbconfig = $config->seqdbConfig("r79rfamseq"); # db info for rfamseq, we may need this to determine database size for -E 
  my $rfamseq_dbsize   = $rfamseq_dbconfig->{"dbSize"}; 
  my $e_sm_bitsc       = undef;
  my $e_opt_bitsc      = undef;
  my $thr_searchopts   = undef;

  # are we going to ignore the SM methods? If not, then we will use the -E or -T
  # from that line. We checked for this above during option processing/checking.
  if($ignore_sm) { 
    $desc_searchopts = ""; 
    $t_sm = undef;
    $e_sm = undef;
  }
  if(defined $e_sm) { 
    # convert E-value to bit score in database size of rfamseq, 
    # note this only works because we know if it's in DESC's SM
    # than it MUST have been defined for a search of r79rfamseq,
    # because that's the only database we update on.
    $e_sm_bitsc = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $e_sm, $rfamseq_dbsize, $extra_searchopts);
  }
  if(defined $e_opt) { 
    # convert E-value to bit score in database size of rfamseq, 
    # note this only works because we know if it's in DESC's SM
    # than it MUST have been defined for a search of r79rfamseq,
    # because that's the only database we update on.
    $e_opt_bitsc = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $e_opt, $rfamseq_dbsize, $extra_searchopts);
  }
  # define threshold option for cmsearch, we ALWAYS use one 
  # and while we do this, perform a sanity check, only one 
  # of $t_sm, $e_sm_bitsc, $t_opt, $e_opt_bitsc should be defined.
  # If this isn't true then there's a bug in the code.
  if(defined $t_sm) { 
    if(defined $e_sm_bitsc || defined $t_opt || defined $e_opt_bitsc) { die "ERROR processing search threshold, bug in code (1)." }
    $thr_searchopts = "-T $t_sm";
  }
  elsif(defined $e_sm_bitsc) { 
    if(defined $t_sm || defined $t_opt || defined $e_opt_bitsc) { die "ERROR processing search threshold, bug in code (2)." }
    $thr_searchopts = "-T $e_sm_bitsc";
  }
  elsif(defined $t_opt) { 
    if(defined $t_sm || defined $e_sm_bitsc || defined $e_opt_bitsc) { die "ERROR processing search threshold, bug in code (3)." }
    $thr_searchopts = "-T $t_opt";
  }
  elsif(defined $e_opt_bitsc) { 
    if(defined $t_sm || defined $e_sm_bitsc || defined $t_opt) { die "ERROR processing search threshold, bug in code (4)." }
    $thr_searchopts = "-T $e_opt_bitsc";
  }
  else { 
    die "ERROR processing search threshold, bug in code (5)." 
  }
  # define other options for cmsearch
  my $ncpus; 
  if (! defined $ncpus_cmsearch) { $ncpus_cmsearch = 4; }

  # use same reporting threshold for regular and reversed searches
  my $searchopts     = "--cpu $ncpus_cmsearch --verbose --nohmmonly $thr_searchopts";
  my $rev_searchopts = "--cpu $ncpus_cmsearch --verbose --nohmmonly";

  $rev_searchopts  = $searchopts . $rev_Zopt;
  $searchopts     .= $Zopt;

  $searchopts     .= $extra_searchopts;
  $rev_searchopts .= $extra_searchopts;
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
  $search_max_wait_secs = Bio::Rfam::Utils::wait_for_cluster($config->location, $user, \@all_jobnameA, \@all_tblOA, "# [ok]", "cmsearch", $logFH, "", -1, $do_stdout);
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
  Bio::Rfam::Infernal::process_cpu_times($all_cmsO,     "Total CPU time:", \$search_max_cpu_secs,     \$search_max_elp_secs,     \$search_cpu_secs,     undef);
  if($rev_ndbfiles > 0) { 
    Bio::Rfam::Infernal::process_cpu_times($all_rev_cmsO, "Total CPU time:", \$rev_search_max_cpu_secs, \$rev_search_max_elp_secs, \$rev_search_cpu_secs, undef);
    if($rev_search_max_cpu_secs > $search_max_cpu_secs) { $search_max_cpu_secs = $rev_search_max_cpu_secs; }
    if($rev_search_max_elp_secs > $search_max_elp_secs) { $search_max_elp_secs = $rev_search_max_elp_secs; }
    $search_cpu_secs += $rev_search_cpu_secs;
  }
  $did_search = 1;

  # write TBLOUT-dependent files
  my $require_tax = 0;
  if(defined $dbconfig) { $require_tax; } # we require tax info if we're doing standard search against a db in the config
#TEMP#  $io->writeTbloutDependentFiles($famObj, $config->rfamlive, $famObj->SEED, $famObj->DESC->CUTGA, $config->RPlotScriptPath, $require_tax);

  # End of block for submitting and processing cmsearch jobs
  #################################################################################
}
# update DESC
if($do_update_desc && ($did_build || $did_calibrate || $did_search)) { 
  $io->writeDESC($famObj->DESC);
}

# finished all work, print output file summary
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, $do_stdout);
my $description;
if($did_build || $did_calibrate) { 
  $description = sprintf("covariance model file (%s)", $did_build ? "built and calibrated" : "calibrated only");
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "CM", $description, $do_stdout);
}
if($do_update_desc && ($did_build || $did_calibrate || $did_search)) { 
  $description = sprintf("desc file (updated:%s%s%s)", 
                         ($did_build)     ? " BM" : "", 
                         ($did_calibrate) ? " CB" : "", 
                         ($did_search)    ? " SM" : "");
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "DESC", $description, $do_stdout);
}

# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
foreach $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, $do_stdout);
  }
}
$description = sprintf("log file (*this* output)");
Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfsearch.log", $description, $do_stdout);

# output time summary
Bio::Rfam::Utils::log_output_timing_summary_column_headings($logFH, $do_stdout);

my $total_wall_secs = time() - $start_time;
my $total_cpu_secs  = $build_wall_secs + $calibrate_cpu_secs + $search_cpu_secs;
my $total_elp_secs  = $build_elp_secs + $calibrate_elp_secs + $search_max_elp_secs;
# define ideal_*_secs: the amount of time each stage takes if max efficiency parallelism achieved: all CPUs take equal time
my $ideal_build_secs     = 0;
my $ideal_calibrate_secs = 0;
my $ideal_search_secs    = 0;
my $ideal_tot_wall_secs  = 0;
my $tot_ncpus_cmsearch = $ncpus_cmsearch * ($ndbfiles + $rev_ndbfiles);

if($did_build) { 
  $ideal_build_secs = $build_elp_secs / 1.; 
  Bio::Rfam::Utils::log_output_timing_summary($logFH,   "cmbuild", $build_wall_secs, $build_elp_secs, "-", $build_elp_secs, $ideal_build_secs, $do_stdout);
  # Note: we fudge the timing a bit for cmbuild by using 'build_elp_secs' where we should use 'build_cpu_secs'
  # in the 3rd to last argument. This shouldn't make any significant difference though since it's safe to 
  # assume CPU and Elapsed time for single CPU cmbuild processes are approx equal.
}
if($did_calibrate) { 
  $ideal_calibrate_secs = $calibrate_cpu_secs / $ncpus_cmcalibrate;
  Bio::Rfam::Utils::log_output_timing_summary($logFH,   "cmcalibrate", $calibrate_wall_secs, $calibrate_cpu_secs, $calibrate_max_wait_secs, $calibrate_elp_secs, $ideal_calibrate_secs, $do_stdout);
}
if($did_search) { 
  $ideal_search_secs = $search_cpu_secs / $tot_ncpus_cmsearch;
  Bio::Rfam::Utils::log_output_timing_summary($logFH,   "cmsearch", $search_wall_secs, $search_cpu_secs, $search_max_wait_secs, $search_max_elp_secs, $ideal_search_secs, $do_stdout);
}
if($did_build || $did_calibrate || $did_search) { 
  $ideal_tot_wall_secs = $ideal_build_secs + $ideal_calibrate_secs + $ideal_search_secs;
  Bio::Rfam::Utils::log_output_timing_summary($logFH,   "total", $total_wall_secs, $total_cpu_secs, "-", $total_elp_secs, $ideal_tot_wall_secs, $do_stdout);
}

Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("#\n"), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# Total time elapsed: %s\n", Bio::Rfam::Utils::format_time_string(time() - $start_time)), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# [ok]\n"), $do_stdout);

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

######################################################################

sub strip_default_options_from_sm { 
  my $desc_sm = $_[0];

  my $desc_searchopts = $desc_sm;
  $desc_searchopts =~ s/\s*-Z\s+\S+\s*/ /;    # remove '-Z <f>'
  $desc_searchopts =~ s/\s*--FZ\s+\S+\s*/ /;  # remove '--FZ <f>'    
  $desc_searchopts =~ s/\s*--cpu\s+\d+\s*/ /; # remove '--cpu <n>'    
  $desc_searchopts =~ s/\s*--verbose\s*/ /;   # remove '--verbose'
  $desc_searchopts =~ s/\s*--nohmmonly\s*/ /; # remove '--nohmmonly'
  $desc_searchopts =~ s/\s*CM\s*/ /;          # remove 'CM',  
  $desc_searchopts =~ s/\s*SEQDB\s*/ /;       # remove 'SEQDB'
  $desc_searchopts =~ s/cmsearch//;           # remove 'cmsearch'
  
  return $desc_searchopts;
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
	    -b         always run cmbuild (default: only run if 'CM' is v1.0, is older than SEED or doesn't exist)
	    -onlybuild build CM and then exit, do not calibrate, do not search
	    -noss      rewrite SEED with zero basepairs, then exit; do not build, calibrate, or search
            -hand      pass --hand option to cmbuild, SEED must have nongap RF annotation
            -enone     pass --enone option to cmbuild
            -ignorebm  ignore build method (BM) in DESC

            OPTIONS RELATED TO CALIBRATION STEP (cmcalibrate):
	    -c         always run cmcalibrate (default: only run if 'CM' is not calibrated)
            -ccpu <n>  set number of CPUs for MPI cmcalibrate job to <n>

            OPTIONS RELATED TO SEARCH STEP (cmsearch):
            -e <f>        set cmsearch E-value threshold as <f>
            -t <f>        set cmsearch bit score threshold as <f>
            -cut_ga       set cmsearch bit score threshold as GA threshold
            -nosearch     do not run cmsearch
            -norev        do not run cmsearch on reversed database files
            -scpu <n>     set number of CPUs for cmsearch jobs to <n>
	    -cmos <str>   add extra arbitrary option to cmsearch with '-<str>'. (Infernal 1.1, only option is '-g')
            -cmod <str>   add extra arbitrary options to cmsearch with '--<str>'. For multiple options use multiple
	                   -cmod lines. e.g. '-cmod toponly -cmod anytrunc' will run cmsearch with --toponly and --anytrunc.
            -ignoresm     ignore the DESC SM command line options

            OPTIONS SPECIFYING SEARCH DATABASE:
            -dbchoice <s>  set sequence database to search as <s> ('rfamseq', 'testrfamseq', 'r79rfamseq')
            -dbfile <s>    set sequence database to search as file <s>
            -dbdir <s>     set sequence database to search as all '.fa' and '.fa.gz' suffixed files in dir <s>
            -dblist <s>    set sequence database to search as all files listed in dir <s>
            -noZ           with -dbdir or -dblist, do not set database size, E-values will pertain to per-file searches
            -Z <f>         set database size as <f>
            
            OPTIONS SPECIFYING REVERSED SEQUENCE DATABASE TO SEARCH:
            -rdbfile <s>   set reversed sequence database to search as file <s>
            -rdbdir <s>    set reversed sequence database to search as all '.fa' and '.fa.gz' suffixed files in dir <s>
            -rZ <f>        set reversed sequence database size as <f>

            OTHER OPTIONS:
            -q <str>     specify queue to submit job to as <str> (EBI \'-q <str>\' JFRC: \'-l <str>=true\')
                         (shortcuts: use <str>='p' for 'production-rh6', <str>='r' for 'research-rh6')
            -ssopt <str> add extra arbitrary string <str> to qsub cmsearch commands, for multiple options use multiple -ssopt <s>
            -quiet       be quiet; do not output anything to stdout (rfsearch.log still created)
  	    -dirty       do not remove temporary/intermediate files that are normally removed
  	    -h|-help     print this help, then exit
EOF
}

