#!/usr/bin/env perl 

# rfseed.pl - Add sequences to a SEED.
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
# other options
my $do_stdout = 1;              # TRUE to output to STDOUT
my $do_quiet  = 0;              # TRUE to not output anything to STDOUT
my $do_name   = 0;              # TRUE if input file only includes name/start-end names
my $do_local  = 0;              # TRUE to align locally w.r.t. the CM
my $do_prob   = 0;              # TRUE to include PPs in output alignment
my $do_help   = 0;              # TRUE to print help and exit, if -h used

my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;

&GetOptions( "l"          => \$do_local,
             "p"          => \$do_prob,
             "n"          => \$do_name,
             "quiet",     => \$do_quiet,
             "h|help"     => \$do_help );

$do_stdout = ($do_quiet) ? 0 : 1;
open($logFH, ">rfseed.log") || die "ERROR unable to open rfseed.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, "add sequences to SEED", $do_stdout);

# read in command line variables
my $infile = "";
if(scalar(@ARGV) != 1) { 
  $do_help = 1; 
}
else { 
  $infile = $ARGV[0];
}

if ( $do_help ) {
  &help();
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

# setup dbfile 
my $dbconfig  = $config->seqdbConfig($dbchoice);
my $fetchfile = $dbconfig->{"fetchPath"};

# by default we list user, date, pwd, family, etc.
# and information for any command line flags set by
# the user. This block should stay consistent with 
# the GetOptions() call above, and with the help()
# subroutine.
my $cwidth = 40;
my $str;
my $opt;
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# user:", $user),                 $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# date:", $date),                 $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# pwd:", getcwd),                 $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# location:", $config->location), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# family-id:", $desc->ID),        $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# family-acc:", $desc->AC),       $do_stdout);

if($do_local)  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# align new sequences locally w.r.t. CM: ",   "yes [-l]"), $do_stdout); }
if($do_prob)   { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# include post probs in new seed: ",          "yes [-p]"), $do_stdout); }
if($do_name)   { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# input file is only sequence names",         "yes [-n]"), $do_stdout); }
if($do_quiet)  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# quiet mode: ",                              "on  [-quiet]"), $do_stdout); }
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "#\n", $do_stdout);

# make sure we have a CM file, and that it's newer than the SEED
if(! -s 'CM') { die "ERROR: CM does not exist, did you run rfsearch.pl?"; }
if(Bio::Rfam::Utils::youngerThan("SEED", "CM")) { die "ERROR SEED is younger than CM, did you run rfsearch.pl (possibly with -onlybuild)?"; }
                                                  
# create hash of potential output files
my %outfileH = ();
my @outfile_orderA = ("SEED.$$", "CM.$$", "seedalignout");
$outfileH{"SEED.$$"}      = "old seed alignment, copy of 'SEED' from before this script was run";
$outfileH{"CM.$$"}        = "old CM file, copy of 'CM' from before this script was run";
$outfileH{"seedalignout"} = "tabular cmalign output for 'SEED' (cmalign --mapali 'SEED.$$'...)";

# remove any of these files that currently exist, they're no invalid, since we're now rerunning the search
my $outfile;
foreach $outfile (@outfile_orderA) {
  if (-e $outfile) { 
    unlink $outfile; 
  } 
}

Bio::Rfam::Utils::log_output_progress_column_headings($logFH, sprintf("per-stage progress:"), $do_stdout);

# parse infile to get list of new seqs to fetch
my @fetchAA = (); 
my ($nseq, $nres);
if($do_name) { # -n used on command line
  ($nseq, $nres) = parse_namelist($infile, $oseedmsa, \@fetchAA);
}
else { # -n not used, default 'outlist' subset file format
  ($nseq, $nres) = parse_outlist($infile, $oseedmsa, \@fetchAA);
}

# copy SEED and CM sideways, do this after we parse the outlist, which checks for overlaps between new seqs and seed seqs
if (-e "SEED") { copy("SEED", "SEED.$$"); }
if (-e "CM")   { copy("CM",   "CM.$$"); }

# fetch sequences
fetch_new_seed_seqs($fetchfile, \@fetchAA, "$$.fa", $logFH, $do_stdout);

# align new sequences to CM, using --mapali and --mapstr with original SEED
my $align_opts = "-o SEED --mapali SEED.$$ --mapstr";
SEEDif(! $do_prob)  { $align_opts .= " --noprob"; }
if(! $do_local) { $align_opts .= " -g"; }
Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $align_opts, "CM", "$$.fa", "seedalignout", "a.$$.err", $nseq, $nres, 1, 0, "", -1, $logFH, $do_stdout);
unlink "$$.fa";

# pause 1 second before building CM, so we can tell CM file was created after SEED file
sleep(1);

# build new CM, using BM from DESC
my $build_opts = $desc->BM;
$build_opts =~ s/^cmbuild\s+//; # remove leading 'cmbuild'
$build_opts =~ s/CM\s+SEED$//;   # remove trailing 'CM SEED';
my $build_start_time = time();
Bio::Rfam::Infernal::cmbuild_wrapper($config, $build_opts, "CM", "SEED", "b.$$.out");
unlink "b.$$.out";
Bio::Rfam::Utils::log_output_progress_local($logFH, "cmbuild", time() - $build_start_time, 0, 1, "", $do_stdout);

##############################################
# finished all work, print output file summary
##############################################
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, $do_stdout);
# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
if(-e "SEED") { 
  Bio::Rfam::Utils::log_output_file_summary($logFH, "SEED", sprintf("new seed alignment; %d seqs added with cmalign [NOTE: inserts in old seqs may have shifted!]", $nseq), $do_stdout);
}  
foreach $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, $do_stdout);
 }
}
my $description = sprintf("log file (*this* output)");
Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfseed.log", $description, $do_stdout);

Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("#\n"), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# Total time elapsed: %s\n", Bio::Rfam::Utils::format_time_string(time() - $start_time)), $do_stdout);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# [ok]\n"), $do_stdout);

close($logFH);
exit 0;

###############
# SUBROUTINES #
###############
#########################################################
# parse_outlist
#
sub parse_outlist {
  my ($outlist, $oseedmsa, $fetchAAR) = @_;

  open(IN, $outlist) || die "ERROR unable to open $outlist"; 

  my @new_nseA = ();    # temporary array of nse we've added, which we'll use for overlap checking
  my $nseq = 0;
  my $nres = 0;

  while(my $line = <IN>) { 
    # example outlist line:
    #  87.5  4.8e-18      FULL  AACZ03038953.1        -    25367    25465    +       1   101     no  Pan_troglodytes_(chimpa..[9598]    Pan troglodytes chromosome 5 Contig52.80, whole genome shotgun sequence.
    #  27.7  3.2e+02      FULL  AAWR02038290.1     53057    53080       1    24     no  Equus_caballus_(horse)[9796]        Equus caballus cont2.38289, whole genome shotgun sequence.                                                    
    if($line !~ m/^\#/) { 
      $line =~ s/^\s+//; # remove leading whitespace
      my @out_elA = split(/\s\s+/, $line); # note: we separate by double spaces
      my ($name, $start, $end) = ($out_elA[3], $out_elA[5], $out_elA[6]);
      $nres += ($start <= $end) ? ($end - $start + 1) : ($start - $end + 1);
      $nseq++;

      my $nse = "$name/$start-$end";
      my ($validated, undef, undef, undef, undef) = Bio::Rfam::Utils::nse_breakdown($nse);
      if(! $validated) { die "ERROR something wrong with outlist line, can't break it down to name/start-end format ($line)"; }
      
      # make sure this sequence does not overlap with any existing seed sequence
      # or any other sequence we're trying to add, these will die with an error if
      # there's a problem
      check_overlap_with_seed($oseedmsa, $nse);
      check_overlap_with_new_seqs(\@new_nseA, $nse);

      push(@fetchAA, [$nse, $start, $end, $name]); 
      push(@new_nseA, $nse);
      # print ("added $name/$start-$end\n");
    }
  }
  close(IN);

  return ($nseq, $nres);
}

#########################################################
# parse_namelist
#
sub parse_namelist {
  my ($namelist, $oseedmsa, $fetchAAR) = @_;

  open(IN, $namelist) || die "ERROR unable to open $namelist"; 

  my @new_nseA = ();    # temporary array of nse we've added, which we'll use for overlap checking
  my $line_ctr = 0;
  my $nseq = 0;
  my $nres = 0;

  while(my $line = <IN>) { 
    $line_ctr++;
    if($line =~ m/\w/ && $line !~ m/^\#/) { 
      chomp $line;
      $line =~ s/^\s+//; # remove leading whitespace
      $line =~ s/^\s+.*$//; # remove trailing whitespace, and everything after
      my $nse = $line;
      my ($validated, $name, $start, $end, undef) = Bio::Rfam::Utils::nse_breakdown($nse);
      if(! $validated) { die "ERROR first token on line $line_ctr is not in name/start-end format ($nse)"; }
      $nres += ($start <= $end) ? ($end - $start + 1) : ($start - $end + 1);
      $nseq++;

      # make sure this sequence does not overlap with any existing seed sequence
      # or any other sequence we're trying to add, these will die with an error if
      # there's a problem
      check_overlap_with_seed($oseedmsa, $nse);
      check_overlap_with_new_seqs(\@new_nseA, $nse);

      push(@fetchAA, [$nse, $start, $end, $name]); 
      push(@new_nseA, $nse);
      # print ("added $name/$start-$end\n");
    }
  }
  close(IN);

  return ($nseq, $nres);
}

#########################################################
# fetch_new_seed_seqs
#
sub fetch_new_seed_seqs { 
  my ($fetchfile, $fetchAAR, $seqfile, $logFH, $do_stdout) = @_;

  my $fetch_sqfile = Bio::Easel::SqFile->new({
    fileLocation => $fetchfile,
  });

  # fetch sequences
  my $fetch_start_time = time();  
  
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 1, 0, sprintf("[fetching %d seqs]", scalar(@{$fetchAAR}), $do_stdout));
  my $concat_seqstring = $fetch_sqfile->fetch_subseqs($fetchAAR, 60, $seqfile); 
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 0, 1, "", $do_stdout);

  $fetch_sqfile->close_sqfile();

  return;
}

#########################################################
# check_overlap_with_seed: check if a name/start-end ($nse)
# overlaps with any sequence already in the seed, die
# if there is an overlap
#
sub check_overlap_with_seed {
  my ($oseedmsa, $nse) = @_;

  my ($overlap_name, $overlap_fraction) = $oseedmsa->nse_overlap($nse);
  if($overlap_name ne "") { die "ERROR new sequence to add $nse overlaps with sequence already in SEED ($overlap_name)"; }

  return;
}

#########################################################
# check_overlap_with_new_seqs: check if a name/start-end ($nse)
# overlaps with any new sequence we've just added to the 
# seed (in new_nseAR), die if there is an overlap
#
sub check_overlap_with_new_seqs { 
  my ($new_nseAR, $nse) = @_;

  foreach my $overlap_name (@{$new_nseAR}) { 
    my $overlap_fraction = Bio::Rfam::Utils::overlap_fraction_two_nse($nse, $overlap_name);
    if($overlap_fraction > 0.) { die "ERROR two sequences to add overlap with each other: $overlap_name and $nse"; }
  }

  return;
}

#########################################################

sub help {
  print STDERR <<EOF;
  
  rfseed.pl - Add sequences to a SEED.

Usage:      rfseed.pl [options] <file with subset of outlist lines for sequences to add to SEED>

Options:    -l        align locally w.r.t the CM [default: globally]
            -p        include posterior probabilities in new SEED [default: don't]
            -n        <file> includes only sequence names in name/start-end format, one per line
            -quiet    be quiet; do not output anything to stdout (rfseed.log still created)
            -h|-help  print this help, then exit

EOF
}
