#!/usr/bin/env perl
use strict;
use warnings;
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

use Bio::Easel::SqFile;

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
my $relax_about_seed = 0;       # TRUE to allow SEED sequences to not be in the database
# calibration related options
my $force_calibrate = 0;        # TRUE to force calibration
my $ncpus_cmcalibrate;          # number of CPUs for cmcalibrate call
# search related options
my $no_search = 0;              # TRUE to skip search
my $no_rev_search = 0;          # TRUE to skip reversed search
my $e_opt = undef;              # cmsearch E-value to use, defined by GetOptions, if -e
my $t_opt = undef;              # cmsearch bit score cutoff to use, defined by GetOptions, if -t
my $do_cutga = 0;               # TRUE to use GA threshold as bit score cutoff
my $ncpus_cmsearch;             # number of CPUs for cmsearch calls
my @cmosA = ();                 # extra single '-' cmsearch options (e.g. -g)
my @cmodA = ();                 # extra double '--' cmsearch options (e.g. --cyk)
my @ssoptA = ();                # strings to add to cmsearch qsub/bsub commands
my $ssopt_str = "";             # string to add to cmsearch qsub/bsub commands
my $ignore_sm = 0;              # TRUE to ignore BM in DESC for cmbuild options
# debugging options
my $do_hmmonly = 0;             # TRUE to run cmsearch in hmmonly mode
# other options
my $q_opt = "";                 # <str> from -q <str>
my $do_dirty = 0;               # TRUE to not unlink files
my $do_stdout = 1;              # TRUE to output to STDOUT
my $allow_no_desc = 0;          # TRUE to let rfsearch run without a DESC file (we'll create it)
my $do_quiet  = 0;              # TRUE to not output anything to STDOUT
my $do_help = 0;                # TRUE to print help and exit, if -h used

# database related options:
my $dbchoice = "rfamseq";       # dbchoice, by default 'rfamseq'
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
my $exec_description = "build, calibrate, and search a CM against a database.";

my $options_okay = 
    &GetOptions( "b"          => \$force_build,
                 "onlybuild"  => \$only_build,
                 "noss"       => \$do_noss,
                 "hand"       => \$do_hand,
                 "enone"      => \$do_enone,
                 "ignorebm"   => \$ignore_bm,
                 "relax"      => \$relax_about_seed,
                 "c"          => \$force_calibrate,
                 "ccpu=s"     => \$ncpus_cmcalibrate,
                 "e=s",       => \$e_opt,
                 "t=s",       => \$t_opt,
                 "cut_ga",    => \$do_cutga,
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
                 "hmmonly"    => \$do_hmmonly,
                 "q=s"        => \$q_opt, 
                 "ssopt=s@"   => \@ssoptA,
                 "nodesc"     => \$allow_no_desc,
                 "quiet",     => \$do_quiet,
                 "dirty"      => \$do_dirty,
                 "h|help"     => \$do_help );

if(! $options_okay) { 
  &help($exec_description); 
  die "ERROR, unrecognized option;"; 
}

if ( $do_help ) {
  &help($exec_description);
  exit(1);
}

$do_stdout = ($do_quiet) ? 0 : 1;
 open($logFH, ">rfsearch.log") || die "ERROR unable to open rfsearch.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, $exec_description, $do_stdout);

# output header
my $user  = getpwuid($<);
if (! defined $user || length($user) == 0) { 
  die "FATAL: failed to run [getlogin or getpwuid($<)]!\n[$!]";
}

# setup variables 
my $io     = Bio::Rfam::FamilyIO->new;

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

# deal with -nodesc option and check for DESC file
if($allow_no_desc) { # user wants us to allow no DESC file
  # make sure there isn't one
  if(-s "DESC")         { die "ERROR, -nodesc enabled, but a DESC file exists."; }
  if(! $do_update_desc) { die "ERROR, you can only create a DESC (-nodesc) if you're searching rfamseq\nOtherwise BM, CB, and SM would be invalid in new DESC"; }
  $io->writeEmptyDESC();
}
elsif(! -s "DESC") { 
  die "ERROR, no DESC file. If you want to create one, use the -nodesc option\n";
}

my $famObj = Bio::Rfam::Family->new(
                                    'SEED' => {
                                               fileLocation => "SEED",
                                               aliType      => 'seed'
                                              },
                                    'TBLOUT' => { 
                                                 fileLocation => "TBLOUT",
                                                },
                                    'DESC'   => ($do_hmmonly) ? $io->parseDESCallowHmmonly("DESC") : $io->parseDESC("DESC"),
                                   );
my $msa  = $famObj->SEED;
my $desc = $famObj->DESC;
my $id   = $desc->ID;
my $acc  = $desc->AC;

# extra processing of command-line options 
if ($only_build) { # -onlybuild, verify incompatible options are not set
  if (defined $ncpus_cmsearch) { die "ERROR -onlybuild and -scpu are incompatible"; }
  if (@cmosA)                  { die "ERROR -onlybuild and -cmosA are incompatible"; }
  if (@cmodA)                  { die "ERROR -onlybuild and -cmodA are incompatible"; }
}
if ($no_search) { # -nosearch, verify incompatible options are not set
  if (defined $ncpus_cmsearch) { die "ERROR -nosearch and -scpu are incompatible"; }
  if (defined $e_opt)          { die "ERROR -nosearch and -e are incompatible"; }
  if (defined $t_opt)          { die "ERROR -nosearch and -t are incompatible"; }
  if ($do_cutga)               { die "ERROR -nosearch and -cutga are incompatible"; }
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
my $extra_searchopts = "";
if($do_hmmonly) { $extra_searchopts = "--hmmonly "; }
$extra_searchopts .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
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
# -hmmonly requires -e
if($do_hmmonly && (! defined $e_opt)) { 
  die "ERROR -hmmonly requires -e also be used"; 
}

# done with command line option processing
#
# ============================================
# 
# output preamble: user, date, location, etc.
# first, determine maximum column width for pretty formatting
my @opt_lhsA = ();
my @opt_rhsA = ();
my $str;
my $opt;
if   (defined $dbfile)         { push(@opt_lhsA, "# seq db file: ");                        push(@opt_rhsA, "$dbfile" . " [-dbfile]"); }
elsif(defined $dbdir)          { push(@opt_lhsA, "# seq db dir: ");                         push(@opt_rhsA, "$dbdir" . " [-dbdir]"); }
elsif(defined $dblist)         { push(@opt_lhsA, "# seq db list file: ");                   push(@opt_rhsA, "$dblist" . " [-dblist]"); }
else                           { push(@opt_lhsA, "# seq db: ");                             push(@opt_rhsA, $dbchoice); }
if($force_build)               { push(@opt_lhsA, "# force cmbuild step: ");                 push(@opt_rhsA, "yes [-b]"); }
if($only_build)                { push(@opt_lhsA, "# build-only mode: ");                    push(@opt_rhsA, "on [-onlybuild]"); }
if($do_noss)                   { push(@opt_lhsA, "# rewrite SEED with zero bp SS_cons: ");  push(@opt_rhsA, "yes [-noss]"); }
if($do_hand)                   { push(@opt_lhsA, "# pass --hand to cmbuild: ");             push(@opt_rhsA, "yes [-hand]"); }
if($do_enone)                  { push(@opt_lhsA, "# pass --enone to cmbuild: ");            push(@opt_rhsA, "yes [-enone]"); }
if($ignore_bm)                 { push(@opt_lhsA, "# ignore DESC's BM line: ");              push(@opt_rhsA, "yes [-ignorebm]"); }
if($relax_about_seed)          { push(@opt_lhsA, "# allowing SEED seqs not in database: "); push(@opt_rhsA, "yes [-relax]"); }
if($force_calibrate)           { push(@opt_lhsA, "# force cmcalibrate step: ");             push(@opt_rhsA, "yes [-c]"); }
if(defined $ncpus_cmcalibrate) { push(@opt_lhsA, "# num processors for MPI cmcalibrate: "); push(@opt_rhsA, "$ncpus_cmcalibrate [-ccpu]"); }
if(defined $e_opt)             { push(@opt_lhsA, "# E-value cutoff: ");                     push(@opt_rhsA, $e_opt . " [-e]"); }
if(defined $t_opt)             { push(@opt_lhsA, "# bit score cutoff: ");                   push(@opt_rhsA, $t_opt . " [-t]"); }
if($do_cutga)                  { push(@opt_lhsA, "# use GA bit score threshold: ");         push(@opt_rhsA, "yes [-cut_ga]"); }
if($no_search)                 { push(@opt_lhsA, "# skip cmsearch stage: ");                push(@opt_rhsA, "yes [-nosearch]"); }
if($no_rev_search)             { push(@opt_lhsA, "# omit reversed db search: ");            push(@opt_rhsA, "yes [-norev]"); }
if(defined $ncpus_cmsearch)    { push(@opt_lhsA, "# number of CPUs for cmsearch jobs: ");   push(@opt_rhsA, "$ncpus_cmsearch [-scpu]"); }
$str = ""; foreach $opt (@cmosA) { $str .= $opt . " "; }
if(scalar(@cmosA) > 0)         { push(@opt_lhsA, "# single dash cmsearch options: ");       push(@opt_rhsA, $str . "[-cmos]"); }
$str = ""; foreach $opt (@cmodA) { $str .= $opt . " "; }
if(scalar(@cmodA) > 0)         { push(@opt_lhsA, "# double dash cmsearch options: ");       push(@opt_rhsA, $str . "[-cmod]"); }
$ssopt_str = ""; foreach $opt (@ssoptA) { $ssopt_str .= $opt . " "; }
if($ignore_sm)                 { push(@opt_lhsA, "# ignore DESC's SM line: ");              push(@opt_rhsA, "yes [-ignoresm]"); }
if($noZ)                       { push(@opt_lhsA, "# per-database-file E-values: ");         push(@opt_rhsA, "on [-noZ]"); }
if(defined $Zuser)             { push(@opt_lhsA, "# Z (dbsize in Mb): ");                   push(@opt_rhsA, $Zuser . " [-Z]"); }
if(defined $rev_dbfile)        { push(@opt_lhsA, "# reversed db file: ");                   push(@opt_rhsA, $rev_dbfile . " [-rdbfile]"); }
if(defined $rev_dbdir)         { push(@opt_lhsA, "# reversed db dir: ");                    push(@opt_rhsA, $rev_dbdir . " [-rdbdir]"); }
if(defined $rev_Zuser)         { push(@opt_lhsA, "# Z (dbsize in Mb) for reversed db: ");   push(@opt_rhsA, $rev_Zuser . " [-rZ]"); }
if($do_hmmonly)                { push(@opt_lhsA, "# searching in HMM mode (debugging):");   push(@opt_rhsA, "yes [-hmmonly]"); }
if($q_opt ne "")               { push(@opt_lhsA, "# submit to queue: ");                    push(@opt_rhsA, "$q_opt [-q]"); }
if(scalar(@ssoptA) > 0)        { push(@opt_lhsA, "# add to cmsearch submit commands: ");    push(@opt_rhsA, $ssopt_str . "[-ssopt]"); }
if($allow_no_desc)             { push(@opt_lhsA, "# create new DESC b/c none exists: ");    push(@opt_rhsA, "yes [-nodesc]"); }
if($do_quiet)                  { push(@opt_lhsA, "# quiet mode: ");                         push(@opt_rhsA, "on  [-quiet]"); }
if($do_dirty)                  { push(@opt_lhsA, "# do not unlink intermediate files: ");   push(@opt_rhsA, "yes [-dirty]"); }
if(! $allow_no_desc) { 
  if(! $do_update_desc)        { push(@opt_lhsA, "# updating DESC at end of script: ");     push(@opt_rhsA, "no"); }
}
my $nopt = scalar(@opt_lhsA);
my $cwidth = ($nopt > 0) ? Bio::Rfam::Utils::maxLenStringInArray(\@opt_lhsA, $nopt) : 0;
if($cwidth < 14) { $cwidth = 14; } ; # max length of lhs string in log_output_preamble
$cwidth++; # one extra space

# now we have column width output preamble
if($do_quiet) { $do_stdout = 0; }
Bio::Rfam::Utils::log_output_preamble($logFH, $cwidth, $user, $config, $desc, $do_stdout);
# and report options enabled by the user
for(my $z = 0; $z < $nopt; $z++) { 
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("%-*s%s\n", $cwidth, $opt_lhsA[$z], $opt_rhsA[$z]), $do_stdout);
}
Bio::Rfam::Utils::log_output_divider($logFH, $do_stdout);

my $seedfile        = "SEED";
my $copied_seedfile = $seedfile . ".$$";
my $cmfile          = "CM";

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

###########################################################################################################
# Preliminary check: verify that all sequences in the SEED derive from the database we're about to search #
###########################################################################################################
my $name2lookup;
my $nwarnings = 0;
my $fetch_sqfile;
$fetch_sqfile = Bio::Easel::SqFile->new({
  fileLocation => $dbconfig->{"fetchPath"}
});
for(my $i = 0; $i < $msa->nseq; $i++) { 
  my $sqname = $msa->get_sqname($i);
  my($is_nse, $name, $start, $end, $str) = Bio::Rfam::Utils::nse_breakdown($sqname);
  $name2lookup = ($is_nse) ? $name : $sqname;
  my $sqlen = $fetch_sqfile->fetch_seq_length_given_name($name2lookup);
  if($sqlen == -1) { # sequence not in database
    Bio::Rfam::Utils::printToFileAndStderr($logFH, "! WARNING: SEED sequence $sqname: $name2lookup not in database\n"); 
    $nwarnings++;
  }
  elsif($sqlen != 0) { # we have a valid sequence length
    if($start > $sqlen || $end > $sqlen) { # sequence in database but not long enough to cover $start-$end
      Bio::Rfam::Utils::printToFileAndStderr($logFH, "! WARNING: SEED sequence $sqname: $name2lookup exists in database but seq length is $sqlen\n");
      $nwarnings++;
    }
  }
}
if($nwarnings > 0 && (! $relax_about_seed)) { 
  die "ERROR: at least 1 sequence in SEED does not derive from database (permit this with -relax)\ndatabase file: $fetch_sqfile->{path}"; 
}

##############
# Build step #
##############
# Get CM
my $cm;
if (-s $cmfile) { 
  $cm = $famObj->CM($io->parseCM($cmfile));
}

my $is_cm_calibrated = 0;
if (defined $cm && $cm->is_calibrated) { 
  $is_cm_calibrated = 1;
}

# figure out if we have to build
my $do_build = 0;
if (($force_build)                       || # user set -b on command line
    ($only_build)                        || # user set -onlybuild on command line
    (! defined $cm)                      || # 'CM' does not exist
    (! $is_cm_calibrated)                || # 'CM' is not calibrated
    ($do_noss || $do_hand || $do_enone)  || # -noss, -hand or -enone set on cmdline
    ($allow_no_desc)                     || # -nodesc, DESC just created, we'll need a BM line so rebuild
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

  if (! defined $buildopts && ! $allow_no_desc) { die "ERROR unable to read BM (build method) from DESC"; }
  if ($ignore_bm || $allow_no_desc) { $buildopts = ""; } # ignore the BM line

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

  if(-e $seedfile) { copy($seedfile, $copied_seedfile); } else { die "ERROR $seedfile no longer exists"; }

  my $outfile       = "b.$$.out";
  my $cmbuildO_file = "b.$$.stk";
  # we'll save the output alignment so we can map the RF annotation

  # clean up any files from previous runs
  if (-e "CM.xxx") { unlink "CM.xxx"; }

  # run cmbuild to create new CM
  $build_elp_secs = Bio::Rfam::Infernal::cmbuild_wrapper($config, "$buildopts", $cmfile, $seedfile, $outfile);
  if(! $do_dirty) { unlink $outfile; }
  if($buildopts ne "") { $buildopts .= " "; } # add trailing single space so next line properly formats BM (and blank opts ("") will work too)
  $famObj->DESC->BM("cmbuild -F " . $buildopts . "CM SEED");

  # define (or possibly redefine) $cm
  $famObj->CM($io->parseCM("CM"));
  $cm = $famObj->CM($io->parseCM("CM"));
  $is_cm_calibrated = 0;

  # use cmalign --mapali to get an RF annotation version of the SEED that we 
  # can use to map the RF onto the original seed. Actually we'll get an alignment
  # that is the seed plus the consensus sequence, but then we'll remove the consensus
  # sequence in add_rf_and_ss_cons_given_cmalign_mapali_output().
  # first, generate $fafile using 'cmemit -c'
  my $emit_fafile         = "e.$$.fa";
  my $cmalign_mapali_file = "ma.$$.stk";
  Bio::Rfam::Infernal::cmemit_wrapper($config, "-c -o $emit_fafile", $cmfile, undef, 0);
  Bio::Rfam::Infernal::cmalign_wrapper($config, "", "", "--mapali SEED -o $cmalign_mapali_file", "CM", $emit_fafile, undef, "", 1, 100, 1, 0, "", 1, undef, 0);
  unlink $emit_fafile;

  # add RF annotation from cmalign --mapali output file to original SEED
  # to create a new SEED file 
  # (this subroutine expects the extra consensus sequence in the $cmalign_mapali_file alignment)
  add_rf_and_ss_cons_given_cmalign_mapali_output($copied_seedfile, $cmalign_mapali_file, $seedfile, $cm->{cmHeader}->{clen});
  unlink $cmalign_mapali_file;

  # rebuild CM from new SEED
  my $copied_cmfile = $cmfile . ".$$";
  if(-e $cmfile) { copy($cmfile, $copied_cmfile); } else { die "ERROR $cmfile no longer exists"; }
  $outfile = "b2.$$.out";
  $build_elp_secs += Bio::Rfam::Infernal::cmbuild_wrapper($config, "$buildopts", $cmfile, $seedfile, $outfile);

  # verify that this new CM is the same as the original
  verify_two_cms_have_identical_parameters($cmfile, $copied_cmfile, 0.00000001);

  # clean up
  if(! $do_dirty) { 
    if(-e $outfile)       { unlink $outfile; }
    if(-e $copied_cmfile) { unlink $copied_cmfile; }
  }

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
my $do_calibrate = 
    (! $only_build) &&         # -onlybuild NOT enabled
    (! $do_hmmonly) &&         # -hmmonly NOT enabled
    ($force_calibrate      ||  # -c used
     (! $is_cm_calibrated) ||  # CM not calibrated
     ($allow_no_desc))         # -nodesc enabled, we'll create a new DESC file and we'll want a CB line
    ? 1 : 0;
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

if ((! $only_build) && ((! $no_search) || ($allow_no_desc))) { 
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
  $rev_ndbfiles = 0;    # number of reversed seq files to search
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
  if(defined $dbsize && (! $do_hmmonly)) { $rev_Zopt .= " --FZ $dbsize "; }

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
  my $rfamseq_dbconfig = $config->seqdbConfig("rfamseq"); # db info for rfamseq, we may need this to determine database size for -E 
  my $rfamseq_dbsize   = $rfamseq_dbconfig->{"dbSize"}; 
  my $e_sm_bitsc       = undef;
  my $e_opt_bitsc      = undef;
  my $ga_opt_bitsc     = undef
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
    # than it MUST have been defined for a search of rfamseq,
    # because that's the only database we update on.
    $e_sm_bitsc = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $e_sm, $rfamseq_dbsize, $extra_searchopts);
  }
  if(defined $e_opt) { 
    # convert E-value to bit score in database size of rfamseq, 
    # note this only works because we know if it's in DESC's SM
    # than it MUST have been defined for a search of rfamseq,
    # because that's the only database we update on.
    $e_opt_bitsc = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $e_opt, $rfamseq_dbsize, $extra_searchopts);
  }

  # define threshold option for cmsearch, we ALWAYS use one 
  # and while we do this, perform a sanity check, only one 
  # of $t_sm, $e_sm_bitsc, $t_opt, $e_opt_bitsc should be defined.
  # If this isn't true then there's a bug in the code.
  if(defined $t_sm) { 
    if($do_cutga || defined $e_sm_bitsc || defined $t_opt || defined $e_opt_bitsc) { die "ERROR processing search threshold, bug in code (1)." }
    $thr_searchopts = sprintf("-T %.2f", $t_sm);
  }
  elsif(defined $e_sm_bitsc) { 
    if($do_cutga || defined $t_sm || defined $t_opt || defined $e_opt_bitsc) { die "ERROR processing search threshold, bug in code (2)." }
    $thr_searchopts = sprintf("-T %.2f", $e_sm_bitsc);
  }
  elsif(defined $t_opt) { 
    if($do_cutga || defined $t_sm || defined $e_sm_bitsc || defined $e_opt_bitsc) { die "ERROR processing search threshold, bug in code (3)." }
    $thr_searchopts = sprintf("-T %.2f", $t_opt);
  }
  elsif(defined $e_opt_bitsc) { 
    if($do_cutga || defined $t_sm || defined $e_sm_bitsc || defined $t_opt) { die "ERROR processing search threshold, bug in code (4)." }
    $thr_searchopts = sprintf("-T %.2f", $e_opt_bitsc);
  }
  elsif($do_cutga) { 
    if(defined $t_sm || defined $e_sm_bitsc || defined $t_opt || defined $e_opt_bitsc) { die "ERROR processing search threshold, bug in code (5)." }
    $thr_searchopts = "-T " . $desc->CUTGA;
  }
  else { 
    die "ERROR processing search threshold, bug in code (6)." 
  }
  # define other options for cmsearch
  my $ncpus; 
  if (! defined $ncpus_cmsearch) { $ncpus_cmsearch = 4; }

  # use same reporting threshold for regular and reversed searches
  my $nohmm_opt = ($do_hmmonly) ? "" : "--nohmmonly "; # if $do_hmmonly we've already added --hmmonly to $extra_searchopts above
  my $searchopts     = "--cpu $ncpus_cmsearch --verbose " . $nohmm_opt . $thr_searchopts;
  my $rev_searchopts = "--cpu $ncpus_cmsearch --verbose " . $nohmm_opt;

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
  if(defined $dbconfig) { $require_tax = 1; } # we require tax info if we're doing standard search against a db in the config
  $io->writeTbloutDependentFiles($famObj, $config->rfamlive, $famObj->SEED, $famObj->DESC->CUTGA, $config->RPlotScriptPath, $require_tax, $logFH);

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
# first report about SEED, we may have modified it, if so we made a copy first
my $seed_changed = 0;
if(! -e "$seedfile") { die "ERROR, $seedfile does not exist at end of script..."; }
if((! -e $copied_seedfile) || (! Bio::Rfam::Utils::checkIfTwoFilesAreIdentical($seedfile, $copied_seedfile))) { 
  # SEED was changed, keep copy
  Bio::Rfam::Utils::log_output_file_summary($logFH, $seedfile.".$$", "old seed alignment, copy of '$seedfile' from before this script was run", $do_stdout);
  Bio::Rfam::Utils::log_output_file_summary($logFH, $seedfile,       "new seed alignment, RF potentially added, SS_cons potentially updated", $do_stdout);
}
else { 
  if(-e $copied_seedfile) { unlink $copied_seedfile; } # SEED unchanged, delete copy 
  Bio::Rfam::Utils::log_output_file_summary($logFH, $seedfile, "seed alignment (unchanged by this script)", $do_stdout);
} 
# report about CM
if($did_build || $did_calibrate) { 
  $description = sprintf("covariance model file (%s)", $did_build ? "built and calibrated" : "calibrated only");
  if   ($did_build && $did_calibrate) { $description = "covariance model file (built and calibrated)"; }
  elsif(              $did_calibrate) { $description = "covariance model file (calibrated only)"; }
  else                                { $description = "covariance model file (built but not calibrated)"; }
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "CM", $description, $do_stdout);
}
# report about DESC
if($allow_no_desc) { 
  $description = "desc file (created, with some nonsense values that you should change)";
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "DESC", $description, $do_stdout);
}
elsif($do_update_desc && ($did_build || $did_calibrate || $did_search)) { 
  if((-e "DESC.$$") && (Bio::Rfam::Utils::checkIfTwoFilesAreIdentical("DESC", "DESC.$$"))) {
    unlink "DESC.$$"; 
    Bio::Rfam::Utils::log_output_file_summary($logFH,   "DESC", "desc file (unchanged by this script)", $do_stdout);
  } # DESC.$$ is same as DESC, no need to keep the former
  else { 
    $description = sprintf("desc file (updated:%s%s%s)", 
                           ($did_build)     ? " BM" : "", 
                           ($did_calibrate) ? " CB" : "", 
                           ($did_search)    ? " SM" : "");
    Bio::Rfam::Utils::log_output_file_summary($logFH,   "DESC", $description, $do_stdout);
  }
}

# output brief descriptions of the files we just created, 
# for files in @outfile_orderA, we know that if these files exist that we just created them, because we deleted them at the beginning of the script if they existed
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
    Bio::Rfam::Infernal::cmsearch_wrapper($config, $jobnameAR->[$idx], "--tblout " . $tblOAR->[$idx] . " " . $searchopts, $cmfile, $dbfileAR->[$idx], $cmsOAR->[$idx], $errOAR->[$idx], $ssopt_str, $q_opt, 0);  
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
  $desc_searchopts =~ s/\s*--hmmonly\s*/ /;   # remove '--hmmonly'
  $desc_searchopts =~ s/\s*CM\s*/ /;          # remove 'CM',  
  $desc_searchopts =~ s/\s*SEQDB\s*/ /;       # remove 'SEQDB'
  $desc_searchopts =~ s/cmsearch//;           # remove 'cmsearch'
  
  return $desc_searchopts;
}

######################################################################

# add_rf_and_ss_cons_given_cmalign_mapali_output():
# Given an original SEED alignment ($orig_infile) used to build a CM, and an alignment
# output from cmalign --mapali ($cmalign_mapali_infile) with a single extra sequence other
# than the original SEED, add RF and SS_cons annotation to the original SEED alignment 
# and save it as $outfile.
sub add_rf_and_ss_cons_given_cmalign_mapali_output {

  my ($orig_infile, $cmalign_mapali_infile, $outfile, $clen) = @_;

  # read in original seed
  my $orig_seed = Bio::Easel::MSA->new({
    fileLocation => $orig_infile,
    forceText    => 1
      });
  
  # read in new seed plus consensus sequence aligned (created with cmalign --mapali SEED CM c.fa)
  my $tmp_seed  = Bio::Easel::MSA->new({
    fileLocation => $cmalign_mapali_infile,
    forceText    => 1
   });
  
  # remove final sequence, the consensus sequence, to get a new alignment
  my @usemeA = ();
  my $i;
  for($i = 0;                  $i < $orig_seed->nseq; $i++) { $usemeA[$i] = 1; }
  for($i = $orig_seed->nseq(); $i < $tmp_seed->nseq;  $i++) { $usemeA[$i] = 0; }
  my $new_seed = $tmp_seed->sequence_subset(\@usemeA);
  undef $tmp_seed;
  
  # verify sequences are in the same order in each MSA
  for(my $i = 0; $i < $orig_seed->nseq(); $i++) { 
    if($orig_seed->get_sqname($i) ne $new_seed->get_sqname($i)) { 
      die "ERROR sequences in SEED and SEED.1 are in a different order.";
    }
  }

  # Map the two alignments: for each nongap RF column in the $new_seed,
  # we should be able to unambiguously match an identical column in the $orig_seed.
  # We'll use this map to create the RF and SS_cons strings for the original
  # alignments.
  my $orig_alen = $orig_seed->alen();
  my $new_alen  = $new_seed->alen();
  
  my $new_rf       = $new_seed->get_rf();
  my @new_rfA      = split("", $new_rf);
  
  my $new_ss_cons  = $new_seed->get_ss_cons();
  my @new_ss_consA = split("", $new_ss_cons);
  
  my $cpos         = 0;
  my $new_apos     = 0;
  my $orig_apos    = 0;
  my $orig_rf      = "";
  my $orig_ss_cons = "";
  while(($new_apos < $new_alen) && ($cpos < $clen)) { 
    while($new_rfA[$new_apos] !~ m/[a-zA-Z]/) { 
      $new_apos++;
    }
    $cpos++;
    #printf("cpos: $cpos new_apos: $new_apos orig_apos: $orig_apos\n");
    # get this column of the original seed
    my $new_col = $new_seed->get_column($new_apos+1);
    # find identical column in new seed
    my $found_match = 0;
    while((! $found_match) && ($orig_apos < $orig_alen)) { 
      my $orig_col = $orig_seed->get_column($orig_apos+1);
      $orig_col =~ tr/a-z/A-Z/;     # translate to upper case
      $orig_col =~ tr/T/U/;         # translate Ts to Us
      $orig_col =~ s/[^A-Za-z]/-/g; # translate non-alphacharacters to Infernal consensus gap char: '-'
      if($orig_col eq $new_col) { 
        $found_match = 1; 
        # printf("\tfound match new:orig $new_apos:$orig_apos\n"); 
        $orig_rf      .= "$new_rfA[$new_apos]";
        $orig_ss_cons .= "$new_ss_consA[$new_apos]";
      }
      else { 
        $orig_rf      .= ".";
        $orig_ss_cons .= ".";
      }
      $orig_apos++;
    }
    $new_apos++;
    if((! $found_match) && ($orig_apos == $orig_alen)) { die "ERROR unable to find match for consensus position $cpos"; }
  }
  # we've reached clen, deal with possibility of inserts after final cpos
  while(length($orig_rf) < $orig_alen) { $orig_rf .= "."; $orig_ss_cons .= "."; }

  $orig_seed->set_rf($orig_rf);
  $orig_seed->set_ss_cons($orig_ss_cons);
  $orig_seed->capitalize_based_on_rf();
  
  $orig_seed->write_msa($outfile);
  
  return;
}


#################################
# verify_two_cms_have_identical_parameters()
# Given the names of two CM files, verify that they 
# have identical model parameters, including profile
# HMM filter parameters. Do not check that other information
# is identical, e.g. checksum, or E-value statistics.

sub verify_two_cms_have_identical_parameters { 

  my ($cmfile1, $cmfile2, $thr) = @_;

  my ($line1, $line2, $i, $nlines);
  my $cm1 = $io->parseCM($cmfile1);
  my $cm2 = $io->parseCM($cmfile2);

  # check CM parameters
  $nlines = scalar(@{$cm1->{cmBody}});
  for($i = 0; $i < $nlines; $i++) { 
    $line1 = $cm1->{cmBody}->[$i];
    $line2  = $cm2->{cmBody}->[$i];
    if($line1 ne $line2) { 
      validate_line_given_precision_threshold($line1, $line2, $thr);
      # this will die if lines are not equal with allowed precision difference
    }
  }

  # check profile HMM filter parameters
  $nlines = scalar(@{$cm1->{hmmBody}});
  for(my $i = 0; $i < $nlines; $i++) { 
    $line1 = $cm1->{hmmBody}->[$i];
    $line2  = $cm2->{hmmBody}->[$i];
    if($line1 ne $line2) { 
      validate_line_given_precision_threshold($line1, $line2, $thr); 
      # this will die if lines are not equal with allowed precision difference
    }
  }

  # printf("$cmfile1 and $cmfile2 have identical model parameters.\n");
  return;
}


##########################################
# validate_line_given_precision_threshold
# Given two parameter lines of a CM and a precision threshold,
# ensure that they are 
sub validate_line_given_precision_threshold { 
  my($line1, $line2, $thr) = @_;

  if($thr >= 1) { die "in validate_line_given_precision_threshold() threshold too high"; }
  
  my @elA1 = split(/\s+/, $line1);
  my @elA2 = split(/\s+/, $line2);

  my $nel1 = scalar(@elA1);
  my $nel2 = scalar(@elA2);
  if($nel1 != $nel2) { die "validate_line_given_precision_threshold() different number of elements on lines:\n$line1\n$line2"; }

  for(my $i = 0; $i < $nel1; $i++) { 
    my $el1 = $elA1[$i];
    my $el2 = $elA2[$i];
    if($el1 ne $el2) { 
      # check if it's a number
      if($el1 =~ /(\d+)(\.\d+)/) { 
        my($n1a, $n1b) = ($1, $2);
        if($el2 =~ /(\d+)(\.\d+)/) { 
          my($n2a, $n2b) = ($1, $2);
          if($n1a ne $n2a) { die "validate_line_given_precision_threshold() number mismatch $n1a != $n2a in ($el1 vs $el2) on lines:\n$line1\n$line2"; }
          if($n1b ne $n2b) { 
            if(abs($n1b - $n2b) > $thr) { 
              die "validate_line_given_precision_threshold() number mismatch given precision $thr, $n1b != $n2b in ($el1 vs $el2) on lines:\n$line1\n$line2"; 
            }
          }
        }
      }
      else { 
        die "validate_line_given_precision_threshold() mismatch that isn't a real number on lines:\n$line1\n$line2"; 
      }
    }
  }
  return;
}
######################################################################

sub help {
  my ($exec_description) = (@_);
  print STDERR <<EOF;

rfsearch.pl: $exec_description
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
	    -b         : always run cmbuild (default: only run if 'CM' is v1.0, is older than SEED or doesn't exist)
	    -onlybuild : build CM and then exit, do not calibrate, do not search
	    -noss      : rewrite SEED with zero basepairs, then exit; do not build, calibrate, or search
            -hand      : pass --hand option to cmbuild, SEED must have nongap RF annotation
            -enone     : pass --enone option to cmbuild
            -ignorebm  : ignore build method (BM) in DESC
            -relax     : relax requirement that all SEED seqs exist in target database

            OPTIONS RELATED TO CALIBRATION STEP (cmcalibrate):
	    -c         : always run cmcalibrate (default: only run if 'CM' is not calibrated)
            -ccpu <n>  : set number of CPUs for MPI cmcalibrate job to <n>

            OPTIONS RELATED TO SEARCH STEP (cmsearch):
            -e <f>      : set cmsearch E-value threshold as <f>
            -t <f>      : set cmsearch bit score threshold as <f>
            -cut_ga     : set cmsearch bit score threshold as GA threshold
            -nosearch   : do not run cmsearch
            -norev      : do not run cmsearch on reversed database files
            -scpu <n>   : set number of CPUs for cmsearch jobs to <n>
	    -cmos <str> : add extra arbitrary option to cmsearch with '-<str>'. (Infernal 1.1, only option is '-g')
            -cmod <str> : add extra arbitrary options to cmsearch with '--<str>'. For multiple options use multiple
	                   -cmod lines. e.g. '-cmod toponly -cmod anytrunc' will run cmsearch with --toponly and --anytrunc.
            -ignoresm   : ignore the DESC SM command line options

            OPTIONS SPECIFYING SEARCH DATABASE:
            -dbchoice <s> : set sequence database to search as <s> ('rfamseq', 'testrfamseq')
            -dbfile <s>   : set sequence database to search as file <s>
            -dbdir <s>    : set sequence database to search as all '.fa' and '.fa.gz' suffixed files in dir <s>
            -dblist <s>   : set sequence database to search as all files listed in dir <s>
            -noZ          : with -dbdir or -dblist, do not set database size, E-values will pertain to per-file searches
            -Z <f>        : set database size as <f>
            
            OPTIONS SPECIFYING REVERSED SEQUENCE DATABASE TO SEARCH:
            -rdbfile <s> : set reversed sequence database to search as file <s>
            -rdbdir <s>  : set reversed sequence database to search as all '.fa' and '.fa.gz' suffixed files in dir <s>
            -rZ <f>      : set reversed sequence database size as <f>

            OPTIONS FOR DEBUGGING:
            -hmmonly     : run searches in HMM-only mode; -e must also be used

            OTHER OPTIONS:
            -q <str>     specify queue to submit job to as <str> (EBI \'-q <str>\' JFRC: \'-l <str>=true\')
                         (shortcuts: use <str>='p' for 'production-rh6', <str>='r' for 'research-rh6')
            -ssopt <str> add extra arbitrary string <str> to qsub cmsearch commands, for multiple options use multiple -ssopt <s>
            -nodesc      create a default DESC file, because none exists, also requires one of -t, -e or -cut_ga
            -quiet       be quiet; do not output anything to stdout (rfsearch.log still created)
  	    -dirty       do not remove temporary/intermediate files that are normally removed
  	    -h|-help     print this help, then exit
EOF
}
