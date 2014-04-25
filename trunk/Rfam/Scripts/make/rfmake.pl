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

# set default values that command line options may change
my $dbchoice = "rfamseq";       # TODO: read this from SM in DESC
my $ga_bitsc;                   # GA threshold
my $ga_evalue;                  # E-value threshold to use, set with -e
my $df_nper     = 30;           # with -r, default number of seqs to include in each group for representative alignment
my $df_emax     = 10;           # with -r, default maximum E-value to include in "OTHER" group
my $df_bitmin   = "";           # minimum bitscore to include in "OTHER" group
my $df_seed     = 181;          # RNG seed, set with -seed <n>, only relevant if -r used
# options related to creating alignments
my $do_align    = 0;            # TRUE to create align file
my $do_repalign = 0;            # TRUE to create REPALIGN
my $always_local= 0;            # TRUE to always run alignments locally (never on farm)
my $always_farm = 0;            # TRUE to always run alignments on farm (never locally)
my $nproc       = -1;           # number of processors to use for cmalign
my $do_pp       = 0;            # TRUE to annotate alignments with posterior probabilities
my $nper        = $df_nper;     # with -r, number of seqs to include in each group for representative alignment
my $emax        = $df_emax;     # with -r, maximum E-value to include in "OTHER" group
my $bitmin      = $df_bitmin;   # with -r, minimum bitscore to include in "OTHER" group
my $seed        = $df_seed;     # RNG seed, set with -seed <n>, only relevant if -r used
my @cmosA       = ();           # extra single - cmalign options (e.g. -g)
my @cmodA       = ();           # extra double - cmalign options (e.g. --cyk)
# taxinfo related options
my $no_taxinfo  = 0;            # TRUE to NOT create taxinfo file
my $df_n2print = 5;             # target number of SEED taxonomy prefixes to print (-n2print)
my $n2print = $df_n2print;      # target number of SEED taxonomy prefixes to print (-n2print)
my $l2print = 0;                # print all unique prefixes of length <n> 
my $do_nsort = 0;               # true to sort output by counts, not min E-value
# comparison related options
my $oldcompdir = "";            # location of Rfam 11 directory with files for comparison to current ones
my $curcompdir = "";            # location of other rfsearch/rfmake directory with files for comparison to current ones
my $do_forcecomp = 0;           # '1' to allow overwrite of 'comparison' files
# debugging options
my $do_hmmonly = 0;             # TRUE to run cmsearch in hmmonly mode
# other options
my $q_opt = "";                 # <str> from -queue <str>
my $do_dirty = 0;               # TRUE to not unlink files
my $do_stdout = 1;              # TRUE to output to STDOUT
my $do_quiet  = 0;              # TRUE to not output anything to STDOUT
my $do_forcethr = 0;            # TRUE to force GA threshold, even if not hits exist above/below GA
my $do_help = 0;                # TRUE to print help and exit, if -h used

my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;
my $exec_description = "investigate and set family score thresholds.";

my $options_okay = 
    &GetOptions( "t=s"          => \$ga_bitsc,
                 "e=s"          => \$ga_evalue,
                 "a",           => \$do_align, 
                 "r"            => \$do_repalign,
                 "farm"         => \$always_farm,  
                 "local"        => \$always_local,
                 "nproc=n"      => \$nproc,
                 "prob"         => \$do_pp,
                 "nper=n",      => \$nper,
                 "seed=n",      => \$seed,
                 "emax=s",      => \$emax,
                 "bitmin=s",    => \$bitmin,
                 "dbchoice=s"   => \$dbchoice, #TODO: dbchoice should be read from DESC->SM
                 "cmos=s@"      => \@cmosA,
                 "cmod=s@"      => \@cmodA,
                 "notaxinfo"    => \$no_taxinfo,
                 "n2print=n"    => \$n2print,
                 "l2print=n"    => \$l2print,
                 "nsort"        => \$do_nsort,
                 "oldcompare=s" => \$oldcompdir,
                 "curcompare=s" => \$curcompdir,
                 "forcecomp"    => \$do_forcecomp,
                 "hmmonly"      => \$do_hmmonly,
                 "dirty"        => \$do_dirty,
                 "quiet",       => \$do_quiet,
                 "forcethr"     => \$do_forcethr,
                 "queue=s"      => \$q_opt, 
                 "h|help"       => \$do_help );

if(! $options_okay) { 
  &help($exec_description); 
  die "ERROR, unrecognized option;"; 
}

if ( $do_help ) {
  &help($exec_description);
  exit(1);
}

$do_stdout = ($do_quiet) ? 0 : 1;
open($logFH, ">rfmake.log") || die "ERROR unable to open rfmake.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, $exec_description, $do_stdout);

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
                                    'DESC'   => ($do_hmmonly) ? $io->parseDESCallowHmmonly("DESC") : $io->parseDESC("DESC"),
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

# ============================================
# 
# output preamble: user, date, location, etc.
# first, determine maximum column width for pretty formatting
my @opt_lhsA = ();
my @opt_rhsA = ();
my $str;
my $opt;

if(defined $ga_bitsc)          { push(@opt_lhsA, "# bit score GA threshold:");               push(@opt_rhsA, "$ga_bitsc [-t]"); }
if(defined $ga_evalue)         { push(@opt_lhsA, "# E-value-based GA threshold:");           push(@opt_rhsA, "$ga_evalue [-e]"); }
if($do_align)                  { push(@opt_lhsA, "# full alignment:");                       push(@opt_rhsA, "yes [-a]"); }
if($do_repalign)               { push(@opt_lhsA, "# 'representative' alignment:");           push(@opt_rhsA, "yes [-r]"); }
if($always_farm)               { push(@opt_lhsA, "# force farm/cluster alignment:");         push(@opt_rhsA, "yes [-farm]"); }
if($always_local)              { push(@opt_lhsA, "# force local CPU alignment:");            push(@opt_rhsA, "yes [-local]"); }
if($nproc != -1)               { push(@opt_lhsA, "# number of CPUs for cmalign:");           push(@opt_rhsA, $nproc . " [-nproc]"); }
if($do_pp)                     { push(@opt_lhsA, "# include post probs in alignments:");     push(@opt_rhsA, "yes [-prob]"); }
if($nper != $df_nper)          { push(@opt_lhsA, "# number of seqs per group:");             push(@opt_rhsA, $nper . " [-nper]"); }
if($seed != $df_seed)          { push(@opt_lhsA, "# RNG seed set to:");                      push(@opt_rhsA, $seed . " [-seed]"); }
if($emax != $df_emax)          { push(@opt_lhsA, "# max E-value in \"OTHER\" group:");       push(@opt_rhsA, $emax . " [-emax]"); }
if($bitmin ne $df_bitmin)      { push(@opt_lhsA, "# min bit score in \"OTHER\" group:");     push(@opt_rhsA, $bitmin . " [-bitmin]"); }
$str = ""; foreach $opt (@cmosA) { $str .= $opt . " "; }
if(scalar(@cmosA) > 0)         { push(@opt_lhsA, "# single dash cmalign options:");          push(@opt_rhsA, $str . " [-cmos]"); }
$str = ""; foreach $opt (@cmodA) { $str .= $opt . " "; }
if(scalar(@cmodA) > 0)         { push(@opt_lhsA, "# double dash cmalign options:");          push(@opt_rhsA, $str . " [-cmod]"); }
if($no_taxinfo)                { push(@opt_lhsA, "# skip creation of 'taxinfo' file:");      push(@opt_rhsA, "yes [-notaxinfo]"); }
elsif(! $can_do_taxinfo)       { push(@opt_lhsA, "# skip creation of 'taxinfo' file:");      push(@opt_rhsA, "yes [no tax info for db]"); }
if($n2print != $df_n2print)    { push(@opt_lhsA, "# target \# of SEED groups for taxinfo:"); push(@opt_rhsA, $n2print . " [-n2print]"); }
if($l2print != 0)              { push(@opt_lhsA, "# taxinfo unique prefix token length:");   push(@opt_rhsA, $l2print . " [-l2print]"); }
if($do_nsort)                  { push(@opt_lhsA, "# taxinfo sort by count:");                push(@opt_rhsA, "yes [-nsort]"); }
if($oldcompdir ne "")          { push(@opt_lhsA, "# comparing to Rfam 11.0 results in:");    push(@opt_rhsA, $oldcompdir . " [-oldcompare]"); }
if($curcompdir ne "")          { push(@opt_lhsA, "# comparing to other results in:");        push(@opt_rhsA, $oldcompdir . " [-curcompare]"); }
if($do_forcecomp)              { push(@opt_lhsA, "# forcing comparison:");                   push(@opt_rhsA, "yes [-forcecomp]"); }
if($do_dirty)                  { push(@opt_lhsA, "# do not unlink intermediate files:");     push(@opt_rhsA, "yes [-dirty]"); }
if($do_forcethr)               { push(@opt_lhsA, "# forcing GA threshold:");                 push(@opt_rhsA, "yes [-forcethr]"); }
if($q_opt ne "")               { push(@opt_lhsA, "# submit to queue:");                      push(@opt_rhsA, "$q_opt [-queue]"); }
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

# make sure we have the all-important TBLOUT file
if(! -s 'TBLOUT') { die "ERROR: TBLOUT does not exist, did you run rfsearch.pl?"; }

# extra processing of command-line options 
my $do_curcomp = ($curcompdir eq "") ? 0 : 1;
my $do_oldcomp = ($oldcompdir eq "") ? 0 : 1;
# enforce -a or --repalign selected if align-specific options used
if ((! $do_align) && (! $do_repalign)) { 
  if ($always_farm)       { die "ERROR -farm  requires -a or -r"; }
  if ($always_local)      { die "ERROR -local requires -a or -r"; }
  if (scalar(@cmosA) > 1) { die "ERROR -cmos requires -a or -r"; }
  if (scalar(@cmodA) > 1) { die "ERROR -cmod requires -a or -r"; }
  if ($nproc != -1)       { die "ERROR -nproc requires -a or -r"; }
  if ($do_pp)             { die "ERROR -prob requires -a or -r"; }
}
if(! $do_repalign) { 
  if ($nper   != $df_nper)   { die "ERROR -nper requires -r"; }
  if ($seed   != $df_seed)   { die "ERROR -seed requires -r"; }
}
if($do_curcomp && $do_oldcomp) { die "ERROR, -oldcompare and -curcompare are incompatible, pick one"; }
if((! $do_forcecomp) && ($do_curcomp || $do_oldcomp)) { 
  foreach my $compfile ("comparison", "lostoutlist", "lostspecies", "newoutlist", "newspecies") {
    if(-e $compfile) { die "ERROR, comparison file \"$compfile\" already exists, rename or delete it, or use -forcecomp option to overwrite it"; }
  }
}

# create hash of potential output files
my %outfileH = ();
my @outfile_orderA = ("SCORES", "outlist", "revoutlist", "species", "revspecies", "outlist.pdf", "species.pdf", "taxinfo", "align", "alignout", "repalign", "repalignout", "comparison", "lostoutlist", "newoutlist", "lostspecies", "newspecies"); 
$outfileH{"SCORES"}      = "tabular list of all hits above GA threshold";
$outfileH{"outlist"}     = "sorted list of all hits from TBLOUT";
$outfileH{"revoutlist"}  = "sorted list of all hits from REVTBLOUT";
$outfileH{"species"}     = "same as outlist, but with additional taxonomic information";
$outfileH{"revspecies"}  = "same as revoutlist, but with additional taxonomic information";
$outfileH{"outlist.pdf"} = "bit score histograms of all hits";
$outfileH{"species.pdf"} = "bit score histogram hits, colored by taxonomy";
$outfileH{"taxinfo"}     = "summary of taxonomic groups in seed/full/other sets";
$outfileH{"align"}       = "alignment of all hits above GA threshold";
$outfileH{"alignout"}    = "tabular cmalign output for 'align'";
$outfileH{"repalign"}    = "alignment of sampling of hits above GA threshold";
$outfileH{"repalignout"} = "tabular cmalign output for 'repalign'";
if($do_curcomp) { 
  $outfileH{"comparison"}  = "comparison of current search results and another rfsearch dir";
  $outfileH{"lostoutlist"} = "subset of hits (>GA) from other search lost by current search";
  $outfileH{"newoutlist"}  = "subset of hits (>GA) from current search \'outlist\' not in other search \'outlist\'";
  $outfileH{"lostspecies"} = "same as lostoutlist but \'species\' lines instead of outlist lines";
  $outfileH{"newspecies"}  = "same as newoutlist  but \'species\' lines instead of outlist lines";
}
else { 
  $outfileH{"comparison"}  = "comparison of old (Rfam 11.0) and current search results";
  $outfileH{"lostoutlist"} = "subset of hits (>GA) from Rfam 11.0 \'out.list\' lost by current search";
  $outfileH{"newoutlist"}  = "subset of hits (>GA) from current search \'outlist\' not in Rfam 11.0";
  $outfileH{"lostspecies"} = "same as lostoutlist but \'species\' lines instead of out.list lines";
  $outfileH{"newspecies"}  = "same as newoutlist  but \'species\' lines instead of out.list lines";
}
# remove any of these files that currently exist, they're no invalid, since we're now rerunning the search
my $outfile;
foreach $outfile (@outfile_orderA) {
  if (-e $outfile) { 
    unlink $outfile; 
  } 
}

####################################################################
# set thresholds, and write new tblout dependent files, incl. SCORES
####################################################################
if ((defined $ga_bitsc) && (defined $ga_evalue)) { 
  die "ERROR -t and -e combination is invalid, choose 1"; 
} elsif (defined $ga_evalue) { 
  my $bitsc = int((Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $ga_evalue, $Z, $desc->SM)) + 0.5); # round up to nearest int bit score above exact bit score
  $ga_bitsc = sprintf("%.2f", $bitsc);
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# setting threshold as: ", "$ga_bitsc bits [converted -e E-value]"), $do_stdout);
} elsif (defined $ga_bitsc) { 
  $ga_bitsc = sprintf("%.2f", $ga_bitsc);
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# setting threshold as: ", "$ga_bitsc bits [-t]"), $do_stdout);
} else { 
  $ga_bitsc = $famObj->DESC->CUTGA; 
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# setting threshold as: ", "$ga_bitsc bits [from DESC (neither -t nor -e used)]"), $do_stdout);
}    
if (! defined $ga_bitsc) {
  die "ERROR: problem setting threshold\n";
}
$ga_evalue = Bio::Rfam::Infernal::cm_bitsc2evalue($cm, $ga_bitsc, $Z, $desc->SM);

# write TBLOUT's set of dependent files 
# (we do this no matter what, to be safe)
my $rfamdb = $config->rfamlive;
my $require_tax = 0;
if(defined $dbconfig) { $require_tax = 1; } # we require tax info if we're doing standard search against a db in the config
$io->writeTbloutDependentFiles($famObj, $rfamdb, $famObj->SEED, $ga_bitsc, $config->RPlotScriptPath, $require_tax, $logFH);

# set the thresholds based on outlist, also determine if any SEED seqs are below GA or missed altogether
my $orig_ga_bitsc = $famObj->DESC->CUTGA;
my $orig_nc_bitsc = $famObj->DESC->CUTNC;
my $orig_tc_bitsc = $famObj->DESC->CUTTC;
set_nc_and_tc($famObj, $ga_bitsc, "outlist", $do_forcethr, $logFH);

####################
# create SCORES file
####################
$io->makeAndWriteScores($famObj, "outlist");

###################################
# Prep for making additional files:
###################################
# parse the outlist and species into data structures we'll use
# for creating 'taxinfo', 'repalign', and 'comparison' files.
my %infoHH;    # 2D hash, key 1: name/start-end (nse), key 2: "rank", "bitsc", "evalue", "sspecies" or "taxstr"
my @nameOA;    # array, all nse, in order, ranked by score/E-value
my %groupOHA;  # hash of arrays, nse in score rank order, by group
my $do_taxinfo   = ((! $no_taxinfo) && ($can_do_taxinfo)) ? 1 : 0;
my $fetch_sqfile = undef;

if($bitmin ne $df_bitmin) { 
  $emax = Bio::Rfam::Infernal::cm_bitsc2evalue($cm, $bitmin, $Z, $desc->SM);
}
if($do_taxinfo || $do_oldcomp || $do_curcomp) { 
  $io->parseOutlistAndSpecies("outlist", "species", $emax, $ga_bitsc, "", 0, \%infoHH, \@nameOA, \%groupOHA);
}  
if($do_align || $do_repalign || $do_oldcomp) { 
  # open sequence file for fetching seqs
  $fetch_sqfile = Bio::Easel::SqFile->new({
    fileLocation => $fetchfile,
  });
}

################################################
# create taxinfo file, if possible and necessary
################################################
if($do_taxinfo) { 
  my $tiFH;
  open($tiFH, ">taxinfo") || die "ERROR unable to open taxinfo for writing";
  write_taxinfo_file_preamble($tiFH, \%groupOHA, $desc, $ga_bitsc, $ga_evalue);
  $io->taxinfoForHits($tiFH, \%infoHH, \%groupOHA, ["SEED", "FULL", "OTHER"], 1, $n2print, $l2print, $do_nsort, undef, 0);
  write_taxinfo_file_end($tiFH);
  close($tiFH);
}

##################
# OPTIONAL STEPS #
#####################################
# create full alignment, if necessary
#####################################
if ($do_align) { 
  Bio::Rfam::Utils::log_output_progress_column_headings($logFH, sprintf("creating full alignment [enabled with -a, %d sequences]:", $famObj->SCORES->numRegions), $do_stdout);

  # fetch sequences
  my $fetch_start_time = time();  
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 1, 0, sprintf("[fetching %d seqs]", $famObj->SCORES->numRegions), $do_stdout);
  $fetch_sqfile->fetch_subseqs($famObj->SCORES->regions, 60, "$$.fa"); 
  $fetch_sqfile->close_sqfile();
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 0, 1, "", $do_stdout);

  # align with cmalign
  my $options = "-o align --outformat pfam ";
  if(! $do_pp) { $options .= "--noprob " }
  $options .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $options, "CM", "$$.fa", "alignout", "a.$$.err", $famObj->SCORES->numRegions, $famObj->SCORES->nres, $always_local, $always_farm, $q_opt, $nproc, $logFH, $do_stdout);
  if (! $do_dirty) { unlink "$$.fa"; }

} # end of if($do_align)

###############################################
# create representative alignment, if necessary
###############################################
if ($do_repalign) { 
  Bio::Rfam::Utils::log_output_progress_column_headings($logFH, "creating representative alignment [enabled with -r]:", $do_stdout);

  # define each sequence into a group, filter groups down to size 
  # of $nper (default:30) and return fasta string of all remaining seqs; 
  # this is our 'representative set'.
  my ($all_seqs, $all_nseq, $all_nres) = &get_representative_subset($io, $ga_bitsc, $fetch_sqfile, $nper, $emax, $seed, \@cmosA, \@cmodA, $do_dirty, $do_stdout);

  # print representative seqs to file
  open(OUT, ">$$.all.fa") || die "ERROR unable to open $$.all.fa for writing"; 
  print OUT $all_seqs;
  close(OUT);

  # align representative seqs
  my $options = "-o repalign --outformat pfam ";
  if(! $do_pp) { $options .= "--noprob " }
  $options .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $options, "CM", "$$.all.fa", "repalignout", "a.$$.all.err", $all_nseq, $all_nres, $always_local, $always_farm, $q_opt, $nproc, $logFH, $do_stdout);
  if(! $do_dirty) { 
    unlink "$$.all.fa"; 
    unlink "a.$$.all.err";
  }

}  # end of if($do_repalign)

################################################################################################
# compare with other search results, either Rfam11 (if $do_oldcomp) or current (if $do_curcomp)
################################################################################################
my ($old_ga_bitsc, $old_ga_evalue, $avg_bitdiff);
my $ncompared = 100;
if($do_oldcomp) { 
  ($old_ga_bitsc, $old_ga_evalue, $avg_bitdiff, $ncompared) = preliminaries_for_comparison_with_old(\%infoHH, \%groupOHA, $oldcompdir, $cm, $Z, $desc, $fetch_sqfile, $logFH, $do_stdout, $do_dirty);
}
if($do_curcomp) { 
  my $other_desc = $io->parseDESCallowHmmonly($curcompdir . "/DESC");
  $old_ga_bitsc = $other_desc->CUTGA;
}
if($do_oldcomp || $do_curcomp) { 
  my $div_line = "#=========================================================";
  
  my $comparison = "comparison";
  open(COMP, ">" . $comparison) || die "ERROR unable to open $comparison for writing";
  
  print  COMP ($div_line . "\n");
  printf COMP ("# $comparison: created by 'rfmake.pl' with \'%s\' cmd-line option\n", ($do_curcomp) ? "-curcomp" : "-oldcomp"); 
  print  COMP ($div_line . "\n");
  printf COMP ("# %s   %s   %s\n", $desc->AC, $desc->ID, $desc->DE);
  printf COMP ("# current directory: %s\n", getcwd);
  printf COMP ("# directory with old results (relative path): %s\n", ($do_curcomp) ? $curcompdir : $oldcompdir);
  if($do_oldcomp) { 
    printf COMP ("# old results are in old Rfam 11.0 format\n");
  }
  printf COMP ("# new GA bit-score:    $ga_bitsc\n");
  printf COMP ("# new GA E-value:      %6.1g\n", $ga_evalue);
  printf COMP ("# old GA bit-score:    $old_ga_bitsc\n");
  if($do_oldcomp) { 
    printf COMP ("# old GA E-value:      %6.1g\n", $old_ga_evalue);
    printf COMP ("# suggested new GA:    %.2f\n", int($old_ga_bitsc + $avg_bitdiff + 0.5));
    printf COMP ("# calc'ed as old GA ($old_ga_bitsc) plus avg bitsc difference b/t %d old and new hits (%.5f) rounded to nearest integer\n", $ncompared, $avg_bitdiff);
  }
  printf COMP ("#\n");

  if($old_ga_bitsc ne $ga_bitsc) { 
    printf COMP ("# NOTE: GA used to define FULL differs b/t old and new ($old_ga_bitsc != $ga_bitsc)\n");
  }
  close(COMP);
  my $old_outlist = ($do_curcomp) ? $curcompdir . "/outlist" : $oldcompdir . "/out.list";
  my $old_species = ($do_curcomp) ? $curcompdir . "/species" : $oldcompdir . "/species";
  $io->writeHitComparison(\%infoHH, \%groupOHA, $old_outlist, $old_species, "outlist", "species", $do_oldcomp, 1);
}

#####################################################
# write DESC file, with (probably) updated thresholds
#####################################################
$io->writeDESC($famObj->DESC);

##############################################
# finished all work, print output file summary
##############################################
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, $do_stdout);
if(-e "DESC.$$") { 
  Bio::Rfam::Utils::log_output_file_summary($logFH, "DESC.$$", "copy of old DESC file from before this rfmake", $do_stdout);
}
my $description = sprintf("%s%s%s", 
    ($famObj->DESC->CUTTC == $orig_tc_bitsc)   ? " TC" : "", 
    ($famObj->DESC->CUTGA == $orig_ga_bitsc)   ? " GA" : "", 
    ($famObj->DESC->CUTNC == $orig_nc_bitsc)     ? " NC" : "");
if($description ne "") { $description = "family description file (updated:$description)"; }
else                   { $description = "family description file (unchanged)"; }
Bio::Rfam::Utils::log_output_file_summary($logFH, "DESC", $description, $do_stdout);

# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
foreach $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, $do_stdout);
  }
}
$description = sprintf("log file (*this* output)");
Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfmake.log", $description, $do_stdout);

Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("#\n"), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# Total time elapsed: %s\n", Bio::Rfam::Utils::format_time_string(time() - $start_time)), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# [ok]\n"), $do_stdout);

if(defined $fetch_sqfile) {
  $fetch_sqfile->close_sqfile();
}

close($logFH);
exit 0;

###############
# SUBROUTINES #
###############


#########################################################
# set_nc_and_tc: given a GA bit score cutoff and an outlist, determines
# the NC and TC thresholds. If $do_forcethr is '1' then we
# allow the case where 0 hits exist above or below GA,
# else we die in error.

sub set_nc_and_tc { 
  my ($famObj, $ga, $outlist, $do_forcethr, $logFH) = @_;

  my ($tc, $nc, $bits, $line);
  $nc = "undefined";
  $tc = "undefined";

  # no need to validate outlist format, we just created it

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
    if($do_forcethr) { 
      $tc = $ga + 0.5; 
      Bio::Rfam::Utils::printToFileAndStderr($logFH, sprintf ("! WARNING: no hits above GA exist, but -force enabled so TC set as %s bits (GA + 0.5)\n", $tc));
    }
    else { 
      die "ERROR, unable to set TC threshold, GA set too high (no hits above GA).\nRerun rfmake.pl with lower bit-score threshold";
    }
  }    
  if ($nc eq "undefined") { 
    if($do_forcethr) { 
      $nc = $ga - 0.5; 
      Bio::Rfam::Utils::printToFileAndStderr($logFH, sprintf ("! WARNING: no hits below GA exist, but -force enabled so NC set as %s bits (GA - 0.5)\n", $nc));
    }
    else { 
      die "ERROR, unable to set NC threshold, GA set too low (no hits below GA).\nRerun rfmake.pl with higher bit-score threshold";
    }
  }

  $famObj->DESC->CUTGA($ga);
  $famObj->DESC->CUTTC($tc);
  $famObj->DESC->CUTNC($nc);

  return;
}

#########################################################
# get_representative_subset:
#  For each of the three sequence groups ("SEED", "FULL" and "OTHER")
#  filter the sequences in the group to get $nper sequences based on
#  sequence identity in an alignment created by cmalign.
#  Concatenate the resulting 'representative' sequences and return them.

sub get_representative_subset { 
  my($io, $ga_bitsc, $fetch_sqfile, $nper, $emax, $seed, $cmosAR, $cmodAR, $do_dirty, $do_stdout) = @_;

  # preliminaries
  my @groupOA  = ("SEED", "FULL", "OTHER"); # "SEED", "FULL" and "OTHER", order of groups
  my $max_nseq = 2000;
  my $group;
  my $rng = Bio::Easel::Random->new({ seed => $seed});
  my @unlinkA = (); # list of files to unlink before we return

  # parse outlist and species to get info we need for annotating eventual representative alignment
  my %infoHH   = ();      # 2D hash: information read from outlist and species for each hit
  my @nameOA   = ();      # array: rank order of all hits, irrelevant
  my %groupOHA = ();      # hash or arrays: rank order of hits in each group, key is group name
  my @subsetA = ();
  $io->parseOutlistAndSpecies("outlist", "species", $emax, $ga_bitsc, "", 0, \%infoHH, \@nameOA, \%groupOHA);

  # for each group, pick a representative subset of $nper sequences based on pairwise identity
  my $all_seqs = ""; # this will be all representative seqs, concatenated into one string
  my $all_nseq = 0;  # number of representative seqs
  my $all_nres = 0;  # total # residues in all representative seqs
  foreach $group (@groupOA) { 
    if(exists $groupOHA{$group}) { 

      my $fafile  = "$$.$group.fa";
      my $stkfile = "$$.$group.stk";
      my $cmafile = "$$.$group.cmalign";
      my $errfile = "ra.$$.$group.err";

      # fetch the sequences, if there's more than $max_nseq
      # sequences we'll randomly sample only $max_nseq, this
      # is so the pairwise sequence comparison step doesn't 
      # take forever
      my ($concat_seqstring, $nseq, $nres) = &get_random_sequence_subset($rng, $fetch_sqfile, \@{$groupOHA{$group}}, $nper, $logFH, $do_stdout);

      open(OUT, ">" . $fafile) || die "ERROR unable to open $fafile for writing";
      print OUT $concat_seqstring;
      close(OUT);
                                                                                               
      # align sequences
      my $options = "-o $stkfile --noprob ";
      $options .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
      # run cmalign locally or on farm (autodetermined based on job size) 
      Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $options, "CM", $fafile, $cmafile, $errfile, $nseq, $nres, $always_local, $always_farm, $q_opt, $nproc, $logFH, $do_stdout);

      # open and read the MSA
      $msa = Bio::Easel::MSA->new({
        fileLocation => "$$.$group.stk",
      });
      my @usemeA = (); 
      
      # check for case where <= $nper total seqs exist, if so, just include all of them
      if($nseq <= $nper) { 
        Bio::Rfam::Utils::setArray(\@usemeA, 1, $nseq);
      }
      else { 
        # TODO: replace this block with $msa->filter_msa_subset_target_nseq() 
        # (you can get rid of filter_group too, MSA::filter_msa_subset replaces that)
        # and make sure we get the same answer.
        # 
        # binary search for max fractional id ($f_cur) that results in $nper sequences
        # we'll filter the alignment such that no two seqs are more than $f_cur similar to each other
        # (or as close as we can get to $nper by minimal change of 0.01)
        # initializations
        my $f_min = 0.2;
        my $f_opt = 0.2;
        my $f_prv = 1.0;
        my $f_cur = $f_min;
        my ($i, $n);
        my $diff = abs($f_prv - $f_cur);
        while($diff > 0.00999) { # while abs($f_prv - $f_cur) > 0.00999
          @usemeA = ();
          # filter based on percent identity
          &filter_group($msa, $f_cur, $group, \@usemeA);
          $n = Bio::Rfam::Utils::sumArray(\@usemeA, $nseq);
          # printf STDERR ("$group: %.4f %4d seqs\n", $f_cur, $n);
          
          $f_prv = $f_cur;
          # adjust $f_cur for next round based on how many seqs we have
          if($n > $nper) { # too many seqs, lower $f_cur
            $f_cur -= ($diff / 2.); 
          }
          else { # too few seqs, raise $f_cur
            if($f_cur > $f_opt) { $f_opt = $f_cur; }
            $f_cur += ($diff / 2.); 
          }
          
          # round to nearest percentage point (0.01)
          $f_cur = (int(($f_cur * 100) + 0.5)) / 100;
          
          if($f_cur < $f_min) { die "ERROR couldn't meet %d sequences, with fractional id > $f_min for group $group\n"; }
          $diff = abs($f_prv - $f_cur);
        }    
        # $f_opt is our optimal fractional id, the max fractional id that gives <= $nper seqs
        @usemeA = ();
        &filter_group($msa, $f_opt, $group, \@usemeA);
      } # end of else entered if we have more than $nper seqs in group       
      # get unaligned sequences and add to $all_seqs

      my $n = Bio::Rfam::Utils::sumArray(\@usemeA, $nseq);
      my $ctr = 1;
      my $i;
      for($i = 0; $i < $nseq; $i++) { 
        if($usemeA[$i]) { 
          my $sqname  = $msa->get_sqname($i);
          my $sqstr   = $msa->get_sqstring_unaligned($i);
          # replace name with more informative one and add taxstr as seq description
          my $newname = sprintf("B%s|E%s|%s|%s", 
                                $infoHH{$sqname}{"bitsc"}, 
                                $infoHH{$sqname}{"evalue"},
                                $infoHH{$sqname}{"sspecies"},
                                $sqname);
          $all_seqs .= sprintf(">%s%02d|%s %s\n%s\n", $group, $ctr++, $newname, $infoHH{$sqname}{"taxstr"}, $sqstr);
          $all_nseq++;
          $all_nres += length($sqstr);
        }
      } # done adding unaligned seqs to $all_seqs
      push(@unlinkA, ($fafile, $cmafile, $stkfile, $errfile));
    } # end of if(exists($groupOHA{$group}))
  }
  $fetch_sqfile->close_sqfile();

  #cleanup
  if(! $do_dirty) { 
    foreach my $file (@unlinkA) { 
      if(-e $file) { unlink $file; }
    }
  }

  return ($all_seqs, $all_nseq, $all_nres);
}

#########################################################
# filter_group:
#  Given an alignment, filter it such that no two sequences
#  are more than $pid_thr fractionally identical. This *is*
#  order dependent: keep earlier sequences, remove later ones.
#
sub filter_group { 
  my ($msa, $pid_thr, $group, $usemeAR) = @_;

  my ($i, $j, $pid);  # counters and a pid (pairwise identity) value
  my $nseq = $msa->nseq;
  
  # initialize @{$usemeAR}
  for($i = 0; $i < $nseq; $i++) { $usemeAR->[$i] = 1; }
  # for each seq we haven't yet removed, remove any sequences more than $pid_thr identical to it
  for($i = 0; $i < $nseq; $i++) { 
    if($usemeAR->[$i]) { # we haven't removed it yet
      for($j = $i+1; $j < $nseq; $j++) { # for every other seq that ... 
        if($usemeAR->[$j]) { # ... we haven't removed yet
          $pid = $msa->pairwise_identity($i, $j); # get fractional identity
          if($pid > $pid_thr) { 
            $usemeAR->[$j] = 0; # remove it
          }
        }
      }
    }
  }
}

#########################################################
# get_random_sequence_subset
#  Choose a set of $n random sequences from an array of
#  sequence names, and fetch them into a single string
#  in FASTA format (all seqs concatenated together).
#  Return that string, plus number of seqs and residues.
#  If there's fewer than $n seqs in the array, choose
#  them all
#
sub get_random_sequence_subset {
  my ($rng, $fetch_sqfile, $AR, $n, $logFH, $do_stdout) = @_;

  my $orig_nseq = scalar(@{$AR});
  my @chosenA = ();
  my $tmpAR = undef;

  if($orig_nseq <= $n) { # fewer than $n seqs in array, choose them all
    $tmpAR = $AR;
  }
  else { 
    $rng->random_subset_from_array($AR, \@chosenA, $n);
    $tmpAR = \@chosenA;
  }

  # fetch sequences
  my $fetch_start_time = time();  
  my $nseq = 0; 
  my $nres = 0;
  my @fetchAA = (); # temp 2D array for fetching subseqs
  foreach my $nse (@{$tmpAR}) { 
    my (undef, $name, $start, $end, $str) = Bio::Rfam::Utils::nse_breakdown($nse);
    $nres += ($str == 1) ? ($end - $start + 1) : ($start - $end + 1); 
    $nseq++;
    push(@fetchAA, [$nse, $start, $end, $name]); 
  }
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 1, 0, sprintf("[fetching %d seqs]", $nseq), $do_stdout);
  my $concat_seqstring = $fetch_sqfile->fetch_subseqs(\@fetchAA, 60); 
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 0, 1, "", $do_stdout);
  
  return ($concat_seqstring, $nseq, $nres);
}


#########################################################
# parse_old_desc
#  Parse an old DESC file from Rfam 11.0 and return 
#  the GA bit score and a flag indicating if glocal
#  search was used or not.
#
sub parse_old_desc { 
  my ($compdir) = $_[0];

  # new DESC files because old files had different (now illegal) formatting
  $compdir =~ s/\/$//;
  my $olddesc = $compdir . "/DESC";
  my $oldga = "";
  my $old_is_glocal = "";
  my $old_buildopts = "";
  open(ODESC, $olddesc) || die "ERROR unable to open $olddesc [-compare]"; 
  while(<ODESC>) { 
    if(/^GA\s+(\d+\.?\d*)/) { 
      $oldga = $1; 
    }
    if(s/^BM\s+//) { 
      if(m/^cmsearch/) { 
        if(/\s+\-g\s+/) { $old_is_glocal = 1; }
        else            { $old_is_glocal = 0; }
      }
      elsif(m/^cmbuild/) { 
        $old_buildopts = $_;
        $old_buildopts =~ s/^cmbuild\s+//; # remove cmbuild
        $old_buildopts =~ s/\-F+//; # remove -F
        $old_buildopts =~ s/CM//; # remove CM
        $old_buildopts =~ s/SEED//; # remove SEED
        $old_buildopts =~ s/\;.+$//; # remove everything after final ';'
      }
    }
  }

  close(ODESC);
  if($oldga eq "")         { die "ERROR unable to get GA from $olddesc [-compare]"; }
  if($old_is_glocal eq "") { die "ERROR unable to determine if cmsearch -g was used from $olddesc [-compare]"; }
  if($old_buildopts eq "") { die "ERROR unable to determine old cmbuild options from $olddesc [-compare]"; }

  return($oldga, $old_is_glocal, $old_buildopts);
}

#########################################################
# compare_cmalign_files
#  Given two cmalign files created by two different CMs
#  on the same sequence file, return the average bit
#  score difference. 
#
sub compare_cmalign_files {
  my ($cma1, $cma2) = @_;
  
  open(IN1, $cma1) || die "ERROR unable to open $cma1"; 
  open(IN2, $cma2) || die "ERROR unable to open $cma1"; 

  my $nseq = 0;
  my $bitdiff = 0.;
  while(my $line1 = <IN1>) { 
    my $line2 = <IN2>;
    if($line1 !~ m/^\#/) { 
      # 1  AF045143.1/1-98                     98        2      100     no     75.83       -       0.00       0.03       0.03      0.79
      my @elA1 = split(/\s+/, $line1);
      
      if($line2 =~ m/^\#/) { die "ERROR $cma1 and $cma2 are inconsistent"; }
      my @elA2 = split(/\s+/, $line2);
      # ensure seqnames are identical 
      if($elA1[2] ne $elA2[2]) { die "ERROR $cma1 and $cma2 are inconsistent"; }
      $bitdiff += ($elA1[7] - $elA2[7]);
      # print STDERR ("$elA1[2]: $elA1[7] $elA2[7]\n");
      $nseq++;
    }
  }
  close(IN1);
  close(IN2);
  $bitdiff /= $nseq;
  return $bitdiff;
}

#########################################################
# get_comparison_seqs
#  Select a random subset of $comp_nseq sequences from SEED and FULL
#  that are NOT TRUNCATED, fetch them and output them to a file.
#
sub get_comparison_seqs { 
  my ($infoHHR, $groupOHAR, $fetch_sqfile, $fafile, $comp_nseq, $logFH, $do_stdout) = @_;

  my $all_seqstring = "";
  my $all_nseq = 0;
  my $all_nres = 0;  
  my $rng = Bio::Easel::Random->new({ seed => $seed });

  foreach my $group ("SEED", "FULL") { 
    # only choose from hits that are NOT truncated
    my @tmpA = ();
    foreach my $seqname (@{$groupOHA{$group}}) { 
      if($infoHH{$seqname}{"trunc"} eq "no") { 
        push(@tmpA, $seqname);
      }
    }
    my ($concat_seqstring, $nseq, $nres) = &get_random_sequence_subset($rng, $fetch_sqfile, \@tmpA, $comp_nseq, $logFH, $do_stdout);
    $all_seqstring .= $concat_seqstring;
    $all_nseq += $nseq;
    $all_nres += $nres;
  }
  open(OUT, ">" . $fafile) || die "ERROR unable to open $fafile for writing";
  print OUT $all_seqstring;
  close(OUT);

  return ($all_nseq, $all_nres);
}

#########################################################
# compare_old_and_new_hits
#  Compare hits found in old Rfam 11.0 with new set of hits.
#
sub compare_old_and_new_hits { 
  my ($infoHHR, $groupOHAR, $compdir) = @_;

  my %newHHA; # 1st key: group ("SEED" or "FULL"), 2nd key: seqname (not nse), array of 'start-end';
  my %newctH; # key: group ("SEED" or "FULL"), value number of new hits in group
  my %oldctH; # key: group ("SEED" or "FULL"), value number of old hits in group
  my %newolH; # key: group ("SEED" or "FULL"), value number of new hits that overlap >= 1 old hit
  my %oldolH; # key: group ("SEED" or "FULL"), value number of old hits that overlap >= 1 new hit
  # first recast infoHHR into newHHA
  foreach my $group ("SEED", "FULL") { 
    $newolH{$group} = 0;
    $oldolH{$group} = 0;
    foreach my $nse (@{$groupOHAR->{$group}}) { 
      my (undef, $name, $start, $end, $str) = Bio::Rfam::Utils::nse_breakdown($nse);
      push(@{$newHHA{$group}{$name}}, $start . ":" . $end);
      $newctH{$group}++;
    }
  }

  # now for each hit in out.list, do we have a match in the new outlist?
  open(OLD, $compdir . "/out.list") || die "ERROR unable to open $compdir/out.list";
  while(my $line = <OLD>) { 
    if($line !~ m/^\#/) { 

      #91.32	8.65e-15	ALIGN	ACFV01061888.1	      1855	      1756	1	100	.	Callithrix_jacchus_w	Callithrix jacchus Contig81.61, whole genome shotgun sequence.
      my @elA = split(/\s+/, $line);
      my ($group, $bitsc, $name, $start, $end) = ($elA[2], $elA[0], $elA[3], $elA[4], $elA[5]);

      if($group eq "SEED")     { $group = "SEED"; }
      elsif($group eq "ALIGN") { $group = "FULL"; }
      else                     { next; }

      $oldctH{$group}++;
      # determine if there's an overlap in set of new hits
      # We might get > 1 old hits overlapping with the same
      # new hit but we don't want to double count this, so 
      # we redefine the value $newHHA{$group}{$name}[$i] 
      # by multiplying by -1 once it 
      if(exists $newHHA{$group}{$name}) { 
        for(my $i = 0; $i < scalar(@{$newHHA{$group}{$name}}); $i++) { 
          my $already_hit = 0;
          my $startend = $newHHA{$group}{$name}[$i];
          my ($start2, $end2) = split(":", $startend);
          if($start2 < 0) { # this hit already overlapped a previous hit, we marked it previously
            $already_hit = 1;
            $start2 *= -1; # make it positive again
          }
          my $ol = Bio::Rfam::Utils::overlap_nres_or_full($start, $end, $start2, $end2);
          if($ol != 0) { # note, don't want to do > 0, because full overlaps will be ignored
            if(! $already_hit) { $newolH{$group}++; } # only count if not already marked
            $oldolH{$group}++;
            # update value in newHHA so we know this hit has already overlapped with an old hit
            $newHHA{$group}{$name}[$i] = "-" . $start2 . ":" . $end2;
            print("NEW: $name $start2 - $end2 overlapped OLD: $start - $end\n");
            last;
          }
        }
      }
    }
  }
  close(OLD);

  

  return;
}

######################################
    
=head2 write_taxinfo_file_preamble

    Title    : write_taxinfo_file_preamble
    Incept   : EPN, Mon Aug 19 15:04:50 2013
    Usage    : write_taxinfo_file_preamble($groupOHAR, $ga, $evalue, $desc)
    Function : Write preamble for 'taxinfo' output file.
    Args     : $groupOHAR: ref to hash of arrays, nse in score rank order, by group
             : $desc:      Bio::Rfam::Family::DESC object
             : $ga:        GA bit score threshold
             : $evalue:    E-value for $ga
    Returns  : void
    Dies     : upon file input/output error

=cut

sub write_taxinfo_file_preamble { 
  my ($outFH, $groupOHAR, $desc, $ga, $evalue) = @_;

  my %ngroupH       = (); # key: $group, value: number of sequences in a group

  foreach my $group ("SEED", "FULL", "OTHER") { 
    if(! (exists $groupOHAR->{$group})) { 
      $ngroupH{$group} = 0;
    } 
    else { 
      $ngroupH{$group} = scalar(@{$groupOHAR->{$group}});
    }
  }

  printf $outFH ("# =======================================================================================================\n");
  printf $outFH ("# taxinfo: created by 'rfmake.pl', run 'rfmake.pl -h' for a list of cmd-line options that modify behavior\n");
  printf $outFH ("# =======================================================================================================\n");
  printf $outFH ("# family-id:       %s\n", $desc->ID);
  printf $outFH ("# family-acc:      %s\n", $desc->AC);
  printf $outFH ("# pwd:             %s\n", getcwd);
  printf $outFH ("# GA bit-score:    $ga\n");
  printf $outFH ("# GA E-value:      %6.1g\n", $evalue);
  printf $outFH ("# SEED:            %-5d hits (present in SEED)\n", $ngroupH{"SEED"});
  printf $outFH ("# FULL:            %-5d hits (not in SEED, with bitsc >= $ga,  E-value <= %6.1g)\n", $ngroupH{"FULL"}, $evalue);
  printf $outFH ("# OTHER:           %-5d hits (not in SEED or FULL, with E-value <= $emax)\n", $ngroupH{"OTHER"});
  printf $outFH ("#\n");

  return;
}

=head2 write_taxinfo_file_end

    Title    : write_taxinfo_file_end
    Incept   : EPN, Tue Jan 21 14:15:50 2014
    Usage    : write_taxinfo_file_end($fh)
    Function : Write end of taxinfo file: an explanation of the data
             : in the taxinfo file.
    Args     : $fh: output file handle to write to
    Returns  : void
    Dies     : upon file input/output error

=cut

sub write_taxinfo_file_end { 
  my ($fh) = @_;

  # print info on how to interpret the file
  print $fh ("# Explanation of data above:\n");
  print $fh ("#\n");
  print $fh ("# Listed above are counts of hits in various taxonomic groups for the\n");
  print $fh ("# three categories of hits (SEED, FULL, OTHER, defined below), for the current\n");
  print $fh ("# GA threshold. There are several command-line options that modify this output,\n");
  print $fh ("# use rfmake.pl -h for more information.\n");
  print $fh ("#\n");
  print $fh ("# Column abbreviations:\n");
  print $fh ("#   'mem'  column:  three letter summary of which groups have at least 1 hit in this taxonomic group\n");
  print $fh ("#   'ct'   columns: number of hits per hit category for this taxonomic group ('-' if none)\n");
  print $fh ("#   'minE' columns: minimum E-value of all hits in this category and taxonomic group ('-' if none)\n");
  print $fh ("#\n");
  print $fh ("# Definition of the three hit categories:\n");
  print $fh ("#   [S]EED:  seed sequences\n");
  print $fh ("#   [F]ULL:  sequences above current GA\n");
  print $fh ("#   [O]THER: sequences below GA, with E <= 10\n");
  print $fh ("#\n");

  return;
}

#########################################################

=head2 preliminaries_for_comparison_with_old

    Title    : preliminaries_for_comparison_with_old
    Incept   : EPN, Fri Apr 25 10:18:56 2014
    Usage    : preliminaries_for_comparison_with_old()
    Function : Do some computations and write the beginning of
             : the 'comparison' file for an old Rfam 11 dir.
    Args     : $infoHHR:       ref to 2D info hash, filled by FamilyIO:parseOutlistAndSpecies
             : $groupOHAR:     ref to hash of arrays, filled by FamilyIO:parseOutlistAndSpecies
             : $oldcompdir:    path to dir with old files we are comparing to
             : $cm:            the CM object
             : $Z:             database size 
             : $desc:          the DESC object
             : $fetch_sqfile:  to fetch seqs from to cmalign
             : $logFH:         log file handle for output
             : $do_stdout:     '1' to echo log statements to stdout
             : $do_dirty:      '1' to leave intermediate files on disk
    Returns  : $old_ga_bitsc:  GA bit score from old DESC
             : $old_ga_evalue: GA E-value from old DESC
             : $avg_bitdiff:   average bit score difference for $ndiff seqs analyzed with cmalign
             : $all_nseq:      number of sequences compared to get avg_bitdiff
=cut

sub preliminaries_for_comparison_with_old
{
  my ($infoHHR, $groupOHAR, $oldcompdir, $cm, $Z, $desc, $fetch_sqfile, $logFH, $do_stdout, $do_dirty) = @_;

  Bio::Rfam::Utils::log_output_progress_column_headings($logFH, "estimating change in bit scores relative to Rfam 11.0 and Infernal 1.0:", 1);
    
  # process old DESC file, we don't do this the same way we process current DESC
  my ($old_ga_bitsc, $old_is_glocal, $old_buildopts) = &parse_old_desc($oldcompdir);
  my $old_ga_evalue = Bio::Rfam::Infernal::cm_bitsc2evalue($cm, $old_ga_bitsc, $Z, $desc->SM);

  # build CM with '--v1p0' option
  Bio::Rfam::Infernal::cmbuild_wrapper($config, $old_buildopts . " --v1p0 ", "CM.1p0", "SEED", "b.$$.out");
  unlink "b.$$.out";

  # pick sequences to align with new and old CM
  my $fafile = "c.$$.fa";
  my $comp_nseq = 100;
  my ($all_nseq, $all_nres) = &get_comparison_seqs($infoHHR, $groupOHAR, $fetch_sqfile, $fafile, $comp_nseq, $logFH, $do_stdout);

  # align sequences to both old and new CM
  my $stkfile =  "$$.ca.stk";
  my $cmafile  = "$$.ca.cmalign";
  my $errfile  = "$$.ca.err";
  my $ostkfile = "$$.oca.stk";
  my $ocmafile = "$$.oca.cmalign";
  my $oerrfile = "$$.oca.err";
  my $options  = "-o $stkfile --noprob";
  my $ooptions = "-o $ostkfile --noprob";
  if($old_is_glocal) { $ooptions .= " -g"; }
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "ca.$$",  $options,  "CM",     $fafile,  $cmafile,  $errfile, $all_nseq, $all_nres, $always_local, $always_farm, $q_opt, $nproc, $logFH, $do_stdout);
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "oca.$$", $ooptions, "CM.1p0", $fafile, $ocmafile, $oerrfile, $all_nseq, $all_nres, $always_local, $always_farm, $q_opt, $nproc, $logFH, $do_stdout);

  # parse cmalign output files to get avg bit score differences
  my $avg_bitdiff = &compare_cmalign_files($cmafile, $ocmafile);
  if(! $do_dirty) { 
    unlink ($fafile, $stkfile, $cmafile, $errfile, $ostkfile, $ocmafile, $oerrfile);
  }

  return ($old_ga_bitsc, $old_ga_evalue, $avg_bitdiff, $all_nseq);
}

#########################################################

sub help {
  my ($exec_description) = (@_);
  print STDERR <<EOF;
    
rfmake.pl - $exec_description
            Process the results of rfsearch.pl. 
            Create SCORES file given the GA threshold.
	    By default, the GA threshold in DESC is used.
	    There are two ways to redefine the GA threshold: 
	      1) use -t <f> to set it as <f> bits
              2) use -e <f> to set it as <n> bits, where <n> is 
                 minimum integer bit score with E-value <= <f>.

Usage:      rfmake.pl [options]

Options:    -t <f> : set threshold as <f> bits
            -e <f> : set threshold as minimum integer bit score w/E-value <= <f>
	    
	    OPTIONS RELATED TO CREATING ALIGNMENTS (by default none are created):
	    -a          : create 'align' (full) alignment with all hits above GA threshold
	    -r          : create 'repalign' alignment, with sampling of representative hits
 	    -local      : always run cmalign locally     [default: autodetermined based on predicted time]
 	    -farm       : always run cmalign on the farm [default: autodetermined based on predicted time]
            -nproc      : specify number of CPUs for cmalign to use as <n>
            -prob       : annotate alignments with posterior probabilities [default: do not]
            -nper <n>   : with -r, set number of seqs per group (SEED, FULL, OTHER) to <n> [default: 30]
            -seed <n>   : with -r, set RNG seed to <n>, '0' for one-time arbitrary seed [default: 181]
            -emax <f>   : with -r, set maximum E-value   for inclusion in "OTHER" group to <f> [default: 10]
            -minbit <f> : with -r, set minimum bit score for inclusion in "OTHER" group to <f> [default: E-value of 10]
	    -cmos <str> : add extra arbitrary option to cmalign with '-<str>'. (Infernal 1.1, only option is '-g')
            -cmod <str> : add extra arbitrary options to cmalign with '--<str>'. For multiple options use multiple
	                  -cmod lines. Eg. '-cmod cyk -cmod sub' will run cmalign with --cyk and --sub.

	    OPTIONS RELATED TO OUTPUT 'taxinfo' FILE:
	    -notaxinfo   : do not create taxinfo file
            -n2print <n> : target number of SEED taxonomy prefixes to print [default: 5]
            -l2print <n> : print all unique prefixes of length <n>, regardless of number
            -nsort       : sort output by counts, not minimum E-value

            OPTIONS FOR DEBUGGING:
            -hmmonly : rfsearch.pl was run with -hmmonly

	    OPTIONS RELATED TO OUTPUT 'comparison' FILE:
	    -oldcompare <s>   : create comparison file by comparing with old dir <s>
	    -curcompare <s>   : create comparison file by comparing with current (not old) dir <s>
            -forcecompare <s> : prefix all comparison files with <s>

	    OTHER:
	    -dirty       leave temporary files, do not clean up
            -quiet       be quiet; do not output anything to stdout (rfmake.log still created)
            -forcethr    force threshold; even if no hits exist above and/or below GA
            -queue <str> specify queue to submit job to as <str> (EBI \'-q <str>\' JFRC: \'-l <str>=true\')
  	    -h|-help     print this help, then exit

EOF
}


