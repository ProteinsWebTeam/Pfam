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
my $dbchoice = "r79rfamseq";    # TODO: read this from SM in DESC
my $ga_thr;                     # GA threshold
my $evalue;                     # E-value threshold to use, set with -e
my $df_nper     = 30;           # with -r, default number of seqs to include in each group for representative alignment
my $df_emax     = 10;           # with -r, default maximum E-value to include in "OTHER" group
my $df_bitmin   = "";           # with -r, default minimum bitscore to include in "OTHER" group
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
# tax info related options
my $no_taxinfo  = 0;            # TRUE to NOT create taxinfo file
my $n2print = 5;                # target number of SEED taxonomy prefixes to print (-n2print)
my $l2print = 0;                # print all unique prefixes of length <n> 
my $do_nsort = 0;               # true to sort output by counts, not min E-value
# other options
my $q_opt = "";                 # <str> from -q <str>
my $do_dirty = 0;               # TRUE to not unlink files
my $do_help = 0;                # TRUE to print help and exit, if -h used

my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;

open($logFH, ">rfmake.log") || die "ERROR unable to open rfmake.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH,   $executable, "investigate and set family score thresholds");

&GetOptions( "t=s"        => \$ga_thr,
             "e=s"        => \$evalue,
             "a",         => \$do_align, 
             "r"          => \$do_repalign,
             "q=s"        => \$q_opt, 
             "local"      => \$always_local,
             "farm"       => \$always_farm,  
             "nproc=n"    => \$nproc,
             "prob"       => \$do_pp,
             "nper=n",    => \$nper,
             "seed=n",    => \$seed,
             "emax=s",    => \$emax,
             "bitmin=s",  => \$bitmin,
             "dbchoice=s" => \$dbchoice, #TODO: dbchoice should be read from DESC->SM
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmodA,
             "notaxinfo"  => \$no_taxinfo,
             "n2print=n"  => \$n2print,
             "l2print=n"  => \$l2print,
             "nsort"      => \$do_nsort,
             "dirty"      => \$do_dirty,
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
my $cm   = $famObj->CM;
my $id   = $famObj->DESC->ID;
my $acc  = $famObj->DESC->AC;

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
if($do_repalign)               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# 'representative' alignment:",        "yes" . " [-r]")); }
if($always_farm)               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# force farm/cluster alignment:",      "yes" . " [-farm]")); }
if($always_local)              { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# force local CPU alignment:",         "yes" . " [-local]")); }
if($nproc != -1)               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# number of CPUs for cmalign:",        $nproc . " [-nproc]")); }
if($do_pp)                     { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# include post probs in alignments:",  "yes",   " [-prob]")); }
if($nper != $df_nper)          { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# number of seqs per group:",          $nper,   " [-nper]")); }
if($seed != $df_seed)          { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# RNG seed set to:",                   $seed,   " [-seed]")); }
if($emax != $df_emax)          { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# max E-value in \"OTHER\" group:",    $emax,   " [-emax]")); }
if($bitmin ne $df_bitmin)      { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# min bit score in \"OTHER\" group:",  $bitmin, " [-bitmin]")); }
$str = ""; foreach $opt (@cmosA) { $str .= $opt . " "; }
if(scalar(@cmosA) > 0)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# single dash cmalign options:",       $str . "[-cmos]")); }
$str = ""; foreach $opt (@cmodA) { $str .= $opt . " "; }
if(scalar(@cmodA) > 0)         { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# double dash cmalign options:",       $str . "[-cmod]")); }
if($no_taxinfo)                { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# skip creation of 'taxinfo' file:",   "yes [-notaxinfo]")); }
elsif(! $can_do_taxinfo)       { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# skip creation of 'taxinfo' file:",   "yes [no tax info for db]")); }
if($do_dirty)                  { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# do not unlink intermediate files:",  "yes [-dirty]")); }
if($q_opt ne "")               { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# submit to queue:",                   "$q_opt [-q]")); }
Bio::Rfam::Utils::printToFileAndStdout($logFH, "#\n");

# create hash of potential output files
my %outfileH = ();
my @outfile_orderA = ("SCORES", "outlist", "species", "taxinfo", "align", "alignout", "repalign", "repalignout"); 
$outfileH{"SCORES"}      = "tabular list of all hits above GA threshold";
$outfileH{"outlist"}     = "sorted list of all hits from TBLOUT";
$outfileH{"species"}     = "same as outlist, but with additional taxonomic information";
$outfileH{"taxinfo"}     = "summary of taxonomic groups in seed/full/other sets";
$outfileH{"align"}       = "alignment of all hits above GA threshold";
$outfileH{"alignout"}    = "tabular cmalign output for 'align'";
$outfileH{"repalign"}    = "alignment of sampling of hits above GA threshold";
$outfileH{"repalignout"} = "tabular cmalign output for 'repalign'";

# remove any of these files that currently exist, they're no invalid, since we're now rerunning the search
my $outfile;
foreach $outfile (@outfile_orderA) {
  if (-e $outfile) { 
    unlink $outfile; 
  } 
}

# extra processing of command-line options 
# enforce -a or --repalign selected if used align-specific options used
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
  if ($emax   != $df_emax)   { die "ERROR -emax requires -r"; }
  if ($bitmin ne $df_bitmin) { die "ERROR -bitmin requires -r"; }
}

####################################################################
# set thresholds, and write new tblout dependent files, incl. SCORES
####################################################################
if ((defined $ga_thr) && (defined $evalue)) { 
  die "ERROR -t and -e combination is invalid, choose 1"; 
} elsif (defined $evalue) { 
  my $bitsc = int((Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $evalue, $Z, $famObj->DESC->SM)) + 0.5); # round up to nearest int bit score above exact bit score
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
$evalue = Bio::Rfam::Infernal::cm_bitsc2evalue($cm, $ga_thr, $Z, $famObj->DESC->SM);

# write TBLOUT's set of dependent files 
# (we do this no matter what, to be safe)
my $rfamdb = $config->rfamlive;
$io->writeTbloutDependentFiles($famObj, $rfamdb, $famObj->SEED, $ga_thr, $config->RPlotScriptPath);

# set the thresholds based on outlist
my $orig_ga_thr = $famObj->DESC->CUTGA;
my $orig_nc_thr = $famObj->DESC->CUTNC;
my $orig_tc_thr = $famObj->DESC->CUTTC;
set_nc_and_tc($famObj, $ga_thr, "outlist");

####################
# create SCORES file
####################
$io->makeAndWriteScores($famObj, "outlist");

################################################
# create taxinfo file, if possible and necessary
################################################
if((! $no_taxinfo) && ($can_do_taxinfo)) { 
  $io->writeTaxinfoFromOutlistAndSpecies($ga_thr, $evalue, $famObj->DESC->ID, $famObj->DESC->AC, $famObj->DESC->DE, $n2print, $l2print, $do_nsort);
}

##################
# OPTIONAL STEPS #
#####################################
# create full alignment, if necessary
#####################################
if ($do_align) { 
  Bio::Rfam::Utils::log_output_progress_column_headings($logFH, sprintf("creating full alignment [enabled with -a, %d sequences]:", $famObj->SCORES->numRegions), 1);
    
  # fetch sequences
  my $fetch_start_time = time();  
  my $fetch_sqfile = Bio::Easel::SqFile->new({
    fileLocation => $fetchfile,
  });
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 1, 0, sprintf("[fetching %d seqs]", $famObj->SCORES->numRegions), 1);
  $fetch_sqfile->fetch_subseqs($famObj->SCORES->regions, 60, "$$.fa"); 
  $fetch_sqfile->close_sqfile();
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 0, 1, "", 1);

  # align with cmalign
  my $options = "-o align --outformat pfam ";
  if(! $do_pp) { $options .= "--noprob " }
  $options .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $options, "CM", "$$.fa", "alignout", "a.$$.err", $famObj->SCORES->numRegions, $famObj->SCORES->nres, $always_local, $always_farm, $q_opt, $nproc, $logFH);
  if (! $do_dirty) { unlink "$$.fa"; }

} # end of if($do_align)

###############################################
# create representative alignment, if necessary
###############################################
if ($do_repalign) { 
  Bio::Rfam::Utils::log_output_progress_column_headings($logFH, sprintf("creating representative alignment [enabled with -r]:", $famObj->SCORES->numRegions), 1);

  # define each sequence into a group, filter groups down to size 
  # of $nper (default:30) and return fasta string of all remaining seqs; 
  # this is our 'representative set'.
  if($bitmin ne $df_bitmin) { 
    $emax = Bio::Rfam::Infernal::cm_bitsc2evalue($cm, $bitmin, $Z, $famObj->DESC->SM);
  }
  my ($all_seqs, $all_nseq, $all_nres) = &get_representative_subset($io, $ga_thr, $fetchfile, $nper, $emax, $seed, \@cmosA, \@cmodA, $do_dirty);

  # print representative seqs to file
  open(OUT, ">$$.all.fa") || die "ERROR unable to open $$.all.fa for writing"; 
  print OUT $all_seqs;
  close(OUT);

  # align representative seqs
  my $options = "-o repalign --outformat pfam ";
  if(! $do_pp) { $options .= "--noprob " }
  $options .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $options, "CM", "$$.all.fa", "repalignout", "a.$$.all.err", $all_nseq, $all_nres, $always_local, $always_farm, $q_opt, $nproc, $logFH);
  if(! $do_dirty) { 
    unlink "$$.all.fa"; 
    unlink "a.$$.all.err";
  }

}  # end of if($do_repalign)

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
$description = sprintf("log file (*this* output)");
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

# set_nc_and_tc: given a GA bit score cutoff and an outlist, determines
# the NC and TC thresholds.

sub set_nc_and_tc { 
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
# get_representative_subset:
#  For each of the three sequence groups ("SEED", "FULL" and "OTHER")
#  filter the sequences in the group to get $nper sequences based on
#  sequence identity in an alignment created by cmalign.
#  Concatenate the resulting 'representative' sequences and return them.

sub get_representative_subset { 
  my($io, $ga_thr, $fetchfile, $nper, $emax, $seed, $cmosAR, $cmodAR, $do_dirty) = @_;

  # preliminaries
  my @groupOA  = ("S", "F", "O"); # "SEED", "FULL" and "OTHER", order of groups
  my $max_nseq = 2000;
  my $group;
  my $rng = Bio::Easel::Random->new({ seed => $seed }); 
  my $fetch_sqfile = Bio::Easel::SqFile->new({
    fileLocation => $fetchfile,
  });
  my @unlinkA = (); # list of files to unlink before we return

  # parse outlist and species to get info we need for annotating eventual representative alignment
  my %infoHH   = ();      # 2D hash: information read from outlist and species for each hit
  my @nameOA   = ();      # array: rank order of all hits, irrelevant
  my %groupOHA = ();      # hash or arrays: rank order of hits in each group, key is group name
  my @subsetA = ();
  $io->parseOutlistAndSpecies("outlist", "species", $emax, $ga_thr, \%infoHH, \@nameOA, \%groupOHA);

  # for each group, pick a representative subset of $nper sequences based on pairwise identity
  my $all_seqs = ""; # this will be all representative seqs, concatenated into one string
  my $all_nseq = 0;  # number of representative seqs
  my $all_nres = 0;  # total # residues in all representative seqs
  foreach $group (@groupOA) { 
    if(exists $groupOHA{$group}) { 
      # HERE HERE HERE HERE: 
      # 1. WRITE A FUNCTION TO RANDOMLY CHOOSE SEQS TO A CONCATNEATED STRING
      # 2. 

      # first, if there's more than $max_nseq sequences, randomly select $max_nseq, 
      # we do this so the all versus all doesn't take forever
      if(scalar(@{$groupOHA{$group}}) > $max_nseq) { 
        @subsetA = ();
        $rng->random_subset_from_array(\@{$groupOHA{$group}}, \@subsetA, $max_nseq);
        # overwrite full array with subset array
        @{$groupOHA{$group}} = ();
        @{$groupOHA{$group}} = @subsetA;
      }
      my %fgroupHA = (); # holds name of group members that survive the filtering

      # fetch sequences
      my $fetch_start_time = time();  
      my $nseq = 0; 
      my $nres = 0;
      my @fetchAA = (); # temp 2D array for fetching subseqs
      foreach my $nse (@{$groupOHA{$group}}) { 
        my (undef, $name, $start, $end, $str) = Bio::Rfam::Utils::nse_breakdown($nse);
        $nres += ($str == 1) ? ($end - $start + 1) : ($start - $end + 1); 
        $nseq++;
        push(@fetchAA, [$nse, $start, $end, $name]); 
      }
      Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 1, 0, sprintf("[fetching %d seqs]", $nseq), 1);
      $fetch_sqfile->fetch_subseqs(\@fetchAA, 60, "$$.$group.fa"); 
      Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 0, 1, "", 1);

      # align sequences
      my $options = "-o $$.$group.stk --noprob ";
      $options .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
      # run cmalign locally or on farm (autodetermined based on job size) 
      Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $options, "CM", "$$.$group.fa", "$$.$group.cmalign", "a.$$.$group.err", $nseq, $nres, $always_local, $always_farm, $q_opt, $nproc, $logFH);

      # check for case where <= $nper total seqs exist, if so, just include all of them
      if($nseq <= $nper) { 
        @{$fgroupHA{$group}} = @{$groupOHA{$group}};
      }
      else { 
        # open and read the MSA
        my $msa = Bio::Easel::MSA->new({
          fileLocation => "$$.$group.stk",
        });
        my $nseq = $msa->nseq;
        
        # binary search for max fractional id ($f_cur) that results in $nper sequences
        # we'll filter the alignment such that no two seqs are more than $f_cur similar to each other
        # (or as close as we can get to $nper by minimal change of 0.01)
        # initializations
        my $f_min = 0.2;
        my $f_opt = 0.2;
        my $f_prv = 1.0;
        my $f_cur = $f_min;
        my @usemeA = ();
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
        
        # get unaligned sequences and add to $all_seqs
        $n = Bio::Rfam::Utils::sumArray(\@usemeA, $nseq);
        my $ctr = 1;
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
        push(@unlinkA, "$$.$group.stk");
        push(@unlinkA, "$$.$group.fa");
        push(@unlinkA, "$$.$group.cmalign");
        push(@unlinkA, "a.$$.$group.err");
      } # end of else entered if we have more than $nper seqs in group
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
	    -a          create 'align' (full) alignment with all hits above GA threshold
	    -r          create 'repalign' alignment, with sampling of representative hits
            -q <str>    specify queue to submit job to as <str> (EBI \'-q <str>\' JFRC: \'-l <str>=true\')
                        (shortcuts: use <str>='p' for 'production-rh6', <str>='r' for 'research-rh6')
 	    -local      always run cmalign locally     [default: autodetermined based on predicted time]
 	    -farm       always run cmalign on the farm [default: autodetermined based on predicted time]
            -nproc      specify number of CPUs for cmalign to use as <n>
            -prob       annotate alignments with posterior probabilities [default: do not]
            -nper <n>   with -r, set number of seqs per group (SEED, FULL, OTHER) to <n> [default: 30]
            -seed <n>   with -r, set RNG seed to <n>, '0' for one-time arbitrary seed [default: 181]
            -emax <f>   with -r, set maximum E-value   for inclusion in "OTHER" group to <f> [default: 10]
            -bitmin <f> with -r, set minimum bit score for inclusion in "OTHER" group to <f> [default: E-value of 10]
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
  	    -h|-help     print this help, then exit

EOF
}
