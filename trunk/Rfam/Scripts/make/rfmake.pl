#!/usr/bin/env perl 

# rfmake.pl - process the results of rfsearch.pl and set model thresholds.
# $Id: rfmake.pl,v 1.71 2013-01-23 10:06:59 en1 Exp $

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
my $ga_thr;                     # GA threshold
my $evalue;                     # E-value threshold to use, set with -e
my $do_align    = 0;            # TRUE to create align file
my $do_subalign = 0;            # TRUE to create SUBALIGN
my $always_local= 0;            # TRUE to always run alignments locally (never on farm)
my $always_farm = 0;            # TRUE to always run alignments on farm (never locally)
my $nproc       = -1;           # number of processors to use for cmalign
my @cmosA       = ();           # extra single - cmalign options (e.g. -g)
my @cmodA       = ();           # extra double - cmalign options (e.g. --cyk)
my $dbchoice = "r79rfamseq";    # TODO: read this from SM in DESC
# tax info related options
my $no_taxinfo  = 0;            # TRUE to NOT create taxinfo file
my $n2print = 5;                # target number of SEED taxonomy prefixes to print (-n2print)
my $l2print = 0;                # print all unique prefixes of length <n> 
my $do_nsort = 0;               # true to sort output by counts, not min E-value
# other options
my $q_opt = "";                 # <str> from -queue <str>
my $do_dirty = 0;               # TRUE to not unlink files
my $do_help = 0;                # TRUE to print help and exit, if -h used

my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;

open($logFH, ">rfmake.log") || die "ERROR unable to open rfmake.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH,   $executable, "build, calibrate, and search a CM against a database", 1);

&GetOptions( "t=s"        => \$ga_thr,
             "e=s"        => \$evalue,
             "a",         => \$do_align, 
             "subalign"   => \$do_subalign,
             "farm"       => \$always_farm,  
             "local"      => \$always_local,
             "nproc=n"    => \$nproc,
             "dbchoice=s" => \$dbchoice, #TODO: dbchoice should be read from DESC->SM
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmodA,
             "notaxinfo"  => \$no_taxinfo,
             "n2print=n"  => \$n2print,
             "l2print=n"  => \$l2print,
             "nsort"      => \$do_nsort,
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
                                    'CM'     => $io->parseCM("CM"),
                                   );
my $msa  = $famObj->SEED;
my $desc = $famObj->DESC;
my $cm   = $famObj->CM;
my $id   = $desc->ID;
my $acc  = $desc->AC;

# setup dbfile 
my $dbconfig       = $config->seqdbConfig($dbchoice);
my $fetchfile      = $dbconfig->{"fetchPath"};
my $Z              = $dbconfig->{"dbSize"};
my $can_do_taxinfo = $dbconfig->{"haveTax"};

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

if(defined $ga_thr)            { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# bit score GA threshold:",            "$ga_thr" . " [-t]")); }
if(defined $evalue)            { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# E-value-based GA threshold:",        "$evalue" . " [-e]")); }
if($do_align)                  { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# full alignment:",                    "yes" . " [-a]")); }
if($do_subalign)               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# 'representative' alignment:",        "yes" . " [-subalign]")); }
if($always_farm)               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# force farm/cluster alignment:",      "yes" . " [-farm]")); }
if($always_local)              { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# force local CPU alignment:",         "yes" . " [-local]")); }
if($nproc != -1)               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# number of CPUs for cmalign:",        $nproc . " [-nproc]")); }
$str = ""; foreach $opt (@cmosA) { $str .= $opt . " "; }
if(scalar(@cmosA) > 0)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# single dash cmalign options:",       $str . "[-cmos]")); }
$str = ""; foreach $opt (@cmodA) { $str .= $opt . " "; }
if(scalar(@cmodA) > 0)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# double dash cmalign options:",       $str . "[-cmod]")); }
if($no_taxinfo)                { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# skip creation of 'taxinfo' file:",   "yes [-notaxinfo]")); }
elsif(! $can_do_taxinfo)       { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# skip creation of 'taxinfo' file:",   "yes [no tax info for db]")); }
if($do_dirty)                  { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# do not unlink intermediate files:",  "yes [-dirty]")); }
if($q_opt ne "")               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# submit to queue:",                   "$q_opt [-queue]")); }
Bio::Rfam::Utils::printToFileAndStdout($logFH, "#\n");

# create hash of potential output files
my %outfileH = ();
my @outfile_orderA = ("SCORES", "outlist", "species", "taxinfo", "align", "alignout", "subalign", "subalignout"); 
$outfileH{"SCORES"}      = "tabular list of all hits above GA threshold";
$outfileH{"outlist"}     = "sorted list of all hits from TBLOUT";
$outfileH{"species"}     = "same as outlist, but with additional taxonomic information";
$outfileH{"taxinfo"}     = "summary of taxonomic groups in seed/full/other sets";
$outfileH{"align"}       = "alignment of all hits above GA threshold";
$outfileH{"alignout"}    = "tabular cmalign output for 'align'";
$outfileH{"subalign"}    = "alignment of sampling of hits above GA threshold";
$outfileH{"subalignout"} = "tabular cmalign output for 'subalign'";

# remove any of these files that currently exist, they're no invalid, since we're now rerunning the search
my $outfile;
foreach $outfile (@outfile_orderA) {
  if (-e $outfile) { 
    unlink $outfile; 
  } 
}

# extra processing of command-line options 
# enforce -a or --subalign selected if used align-specific options used
if ((! $do_align) && (! $do_subalign)) { 
  if ($always_farm)       { die "ERROR -farm  requires -a or -subalign"; }
  if ($always_local)      { die "ERROR -local requires -a or -subalign"; }
  if (scalar(@cmosA) > 1) { die "ERROR -cmos requires -a or -subalign"; }
  if (scalar(@cmodA) > 1) { die "ERROR -cmod requires -a or -subalign"; }
  if ($nproc != -1)       { die "ERROR -nproc requires -a or -subalign"; }
}


####################################################################
# set thresholds, and write new tblout dependent files, incl. SCORES
####################################################################
if ((defined $ga_thr) && (defined $evalue)) { 
  die "ERROR -t and -e combination is invalid, choose 1"; 
} elsif (defined $evalue) { 
  # TODO, read SM in desc, and pick appropriate E-value line based on that
  my $bitsc = int((Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $evalue, $Z)) + 0.5); # round up to nearest int bit score above exact bit score
  $ga_thr = sprintf("%.2f", $bitsc);
  Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# setting threshold as:", "$ga_thr bits [converted -e E-value]"));
} elsif (defined $ga_thr) { 
  $ga_thr = sprintf("%.2f", $ga_thr);
  Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# setting threshold as:", "$ga_thr bits [-t]"));
} else { 
  $ga_thr = $famObj->DESC->CUTGA; 
  Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# setting threshold as:", "$ga_thr bits [from DESC (neither -t nor -e used)]"));
}    
if (! defined $ga_thr) {
  die "ERROR: problem setting threshold\n";
}
$evalue = Bio::Rfam::Infernal::cm_bitsc2evalue($cm, $ga_thr, $Z);

# write TBLOUT's set of dependent files 
# (we do this no matter what, to be safe)
my $rfamdb = $config->rfamlive;
$io->writeTbloutDependentFiles($famObj, $rfamdb, $famObj->SEED, $ga_thr, $config->RPlotScriptPath);

# set the thresholds based on outlist
my $orig_ga_thr = $famObj->DESC->CUTGA;
my $orig_nc_thr = $famObj->DESC->CUTNC;
my $orig_tc_thr = $famObj->DESC->CUTTC;
setThresholds($famObj, $ga_thr, "outlist");

####################
# create SCORES file
####################
$io->makeAndWriteScores($famObj, "outlist");

################################################
# create taxinfo file, if possible and necessary
################################################
if((! $no_taxinfo) && ($can_do_taxinfo)) { 
  $io->writeTaxinfoFromOutlistAndSpecies($ga_thr, $evalue, $desc->ID, $desc->AC, $desc->DE, $n2print, $l2print, $do_nsort);
}

################
# optional steps 
################
# create full alignment, if necessary
if ($do_align) { 
  Bio::Rfam::Utils::log_output_progress_column_headings($logFH, 1);
    
  # fetch sequences
  my $fetch_start_time = time();  
  my $fetch_sqfile = Bio::Easel::SqFile->new({
    fileLocation => $fetchfile,
  });
  Bio::Rfam::Utils::log_output_progress_local($logFH, "esl-sfetch", time() - $fetch_start_time, 1, 0, sprintf("[fetching %d seqs]", $famObj->SCORES->numRegions), 1);
  $fetch_sqfile->fetch_subseqs($famObj->SCORES->regions, 60, "$$.fa"); 
  $fetch_sqfile->close_sqfile();
  Bio::Rfam::Utils::log_output_progress_local($logFH, "esl-sfetch", time() - $fetch_start_time, 0, 1, "", 1);

  # use cmalign to do the alignment
  my $options = "-o align ";
  $options .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  # Run cmalign locally or on farm (autodetermined based on job size) 
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $options, "CM", "$$.fa", "alignout", "a.$$.err", $famObj->SCORES->numRegions, $famObj->SCORES->nres, $always_local, $always_farm, $q_opt, $nproc, $logFH);

  # remove temporary fasta file, if nec
  if (! $do_dirty) {
    unlink "$$.fa";
  }
} # end of if($do_align)
# TODO: create sub alignment if nec

#####################################################
# write DESC file, with (probably) updated thresholds
#####################################################
$io->writeDESC($famObj->DESC);

##############################################
# finished all work, print output file summary
##############################################
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, 1);
my $description = sprintf("%s%s%s", 
    ($famObj->DESC->CUTTC == $orig_tc_thr)     ? " TC" : "", 
    ($famObj->DESC->CUTGA == $orig_ga_thr)     ? " GA" : "", 
    ($famObj->DESC->CUTNC == $orig_nc_thr)     ? " NC" : "");
if($description ne "") { $description = "desc file (updated:$description)"; }
else                   { $description = "desc file (unchanged)"; }
Bio::Rfam::Utils::log_output_file_summary($logFH, "DESC", $description, 1);

# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
foreach $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, 1);
  }
}
$description = sprintf("log file (*this* output, printed to stdout)");
Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfmake.log", $description, 1);

my $outstr = "#\n";
printf $outstr; print $logFH $outstr;

$outstr = sprintf("# Total time elapsed: %s\n", Bio::Rfam::Utils::format_time_string(time() - $start_time));
printf $outstr; print $logFH $outstr;

$outstr = "# [ok]\n";
printf $outstr; print $logFH $outstr;

close($logFH);
exit 0;

###############
# SUBROUTINES #
###############
sub setThresholds { 
  my ($famObj, $ga, $outlist) = @_;

  my ($tc, $nc, $bits, $line);
  $nc = "undefined";
  $tc = "undefined";

  open(OUTLIST, "$outlist") or die "FATAL: failed to open $outlist\n[$!]";

  while ($line = <OUTLIST>) {
    if ($line !~ m/^\#/) { 
      # first token is bit score
      chomp $line;
      $bits = $line;
      $bits =~ s/^\s+//;  # remove leading whitespace
      $bits =~ s/\s+.*$//;
	    
      if ($ga <= $bits && ($tc eq "undefined" || ($bits < $tc))) {
        $tc = $bits;
      }
      if ($ga  > $bits && ($nc eq "undefined" || ($bits > $nc))) {
        $nc = $bits;
      }
    }
  }

  if ($tc eq "undefined") { 
    die "ERROR, unable to set TC threshold, GA set too high (no hits above GA).\nRerun rfmake.pl with lower bit-score threshold";
  }    
  if ($nc eq "undefined") { 
    die "ERROR, unable to set NC threshold, GA set too low (no hits below GA).\nRerun rfmake.pl with higher bit-score threshold";
  }

  $famObj->DESC->CUTGA($ga);
  $famObj->DESC->CUTTC($tc);
  $famObj->DESC->CUTNC($nc);

  return;
}
#########################################################


sub help {
  print STDERR <<EOF;
    
rfmake.pl - Process the results of rfsearch.pl. 
            Create SCORES file given the GA threshold.
	    By default, the GA threshold in DESC is used.
	    There are two ways to redefine the GA threshold: 
	      1) use -t <f> to set it as <f> bits
              2) use -e <f> to set it as <n> bits, where <n> is 
                 minimum integer bit score with E-value <= <f>.

Usage:      rfmake.pl [options]

Options:    -t <f>  set threshold as <f> bits
            -e <f>  set threshold as minimum integer bit score w/E-value <= <f>
	    
	    OPTIONS RELATED TO CREATING ALIGNMENTS (by default none are created):
	    -a          create 'align' alignment with all hits above threshold, with cmalign (requires -t or -e)
	    -subalign   create 'subalign' alignment, with sampling of representative hits
 	    -local      always run cmalign locally     [default: autodetermined based on predicted time]
 	    -farm       always run cmalign on the farm [default: autodetermined based on predicted time]
            -nproc      specify number of CPUs for cmalign to use as <n>
	    -cmos <str> add extra arbitrary option to cmalign with '-<str>'. (Infernal 1.1, only option is '-g')
            -cmod <str> add extra arbitrary options to cmalign with '--<str>'. For multiple options use multiple
	                -cmod lines. Eg. '-cmod cyk -cmod sub' will run cmalign with --cyk and --sub.

	    OPTIONS RELATED TO OUTPUT 'taxinfo' FILE:
	    -notaxinfo    do not create taxinfo file
            -n2print <n>  target number of SEED taxonomy prefixes to print [default: 5]
            -l2print <n>  print all unique prefixes of length <n>, regardless of number
            -nsort        sort output by counts, not minimum E-value

	    OTHER:
	    -dirty       leave temporary files, don't clean up
            -queue <str> specify queue to submit job to as <str> (EBI \'-q <str>\' JFRC: \'-l <str>=true\')
  	    -h|-help     print this help, then exit

EOF
}
