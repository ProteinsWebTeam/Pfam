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
my $do_local = 0;               # TRUE to align locally w.r.t. the CM
my $do_help = 0;                # TRUE to print help and exit, if -h used

my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;

open($logFH, ">rfseed.log") || die "ERROR unable to open rfseed.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, "add sequences to SEED", 1);

&GetOptions( "l"          => \$do_local,
             "h|help"     => \$do_help );

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
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# user:", $user));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# date:", $date));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# pwd:", getcwd));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# location:", $config->location));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# family-id:", $desc->ID));
Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# family-acc:", $desc->AC));

if($do_local)       { Bio::Rfam::Utils::printToFileAndStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# align new sequences locally w.r.t. CM:",  "yes [-l]")); }
Bio::Rfam::Utils::printToFileAndStdout($logFH, "#\n");

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

Bio::Rfam::Utils::log_output_progress_column_headings($logFH, sprintf("per-stage progress:"), 1);

# parse infile to get list of new seqs to fetch
my @fetchAA = (); 
my ($nseq, $nres) = parse_outlist($infile, $oseedmsa, \@fetchAA);

# copy SEED and CM sideways, do this after we parse the outlist, which checks for overlaps between new seqs and seed seqs
if (-e "SEED") { copy("SEED", "SEED.$$"); }
if (-e "CM")   { copy("CM",   "CM.$$"); }

# fetch sequences
fetch_new_seed_seqs($fetchfile, \@fetchAA, "$$.fa", $logFH);

# align new sequences to CM, using --mapali with original SEED
my $align_opts = "-o SEED --mapali SEED.$$";
if(! $do_local) { $align_opts .= " -g"; }
Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $align_opts, "CM", "$$.fa", "seedalignout", "a.$$.err", $nseq, $nres, 1, 0, "", -1, $logFH);
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
Bio::Rfam::Utils::log_output_progress_local($logFH, "cmbuild", time() - $build_start_time, 0, 1, "", 1);

##############################################
# finished all work, print output file summary
##############################################
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, 1);
# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
foreach $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, 1);
 }
}
my $description = sprintf("log file (*this* output)");
Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfseed.log", $description, 1);

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
#########################################################
# parse_outlist
#
sub parse_outlist {
  my ($outlist, $oseedmsa, $fetchAAR) = @_;

  open(IN, $outlist) || die "ERROR unable to open $outlist"; 

  my @tmp_nseA = ();    # temporary array of nse we've added, which we'll use for overlap checking
  my $overlap_name;     # name/start-end for overlap check
  my $overlap_fraction; # fraction of overlap

  while(my $line = <IN>) { 
    # example outlist line:
    #  27.7  3.2e+02      FULL  AAWR02038290.1     53057    53080       1    24     no  Equus_caballus_(horse)[9796]        Equus caballus cont2.38289, whole genome shotgun sequence.                                                    
    if($line !~ m/^\#/) { 
      $line =~ s/^\s+//; # remove leading whitespace
      my @out_elA = split(/\s\s+/, $line); # note: we separate by double spaces
      my ($name, $start, $end) = ($out_elA[3], $out_elA[4], $out_elA[5]);
      $nres += ($start <= $end) ? ($end - $start + 1) : ($start - $end + 1);
      $nseq++;

      my $nse = "$name/$start-$end";
      # make sure this sequence does not overlap with any existing seed sequence
      # or any other sequence we're trying to add
      ($overlap_name, $overlap_fraction) = $oseedmsa->nse_overlap($nse);
      if($overlap_name ne "") { die "ERROR new sequence to add $nse overlaps with sequence already in SEED ($overlap_name)"; }
      
      foreach $overlap_name (@tmp_nseA) { 
        $overlap_fraction = Bio::Rfam::Utils::overlap_fraction_two_nse($nse, $overlap_name);
        if($overlap_fraction > 0.) { die "ERROR two sequences to add overlap with each other: $overlap_name and $nse"; }
      }

      push(@fetchAA, [$nse, $start, $end, $name]); 
      push(@tmp_nseA, $nse);
      # print ("added $name/$start-$end\n");
    }
  }
  close(IN);

  return ($nseq, $nres);
}

#########################################################
#########################################################
# fetch_new_seed_seqs
#
sub fetch_new_seed_seqs { 
  my ($fetchfile, $fetchAAR, $seqfile, $logFH) = @_;

  my $fetch_sqfile = Bio::Easel::SqFile->new({
    fileLocation => $fetchfile,
  });

  # fetch sequences
  my $fetch_start_time = time();  
  
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 1, 0, sprintf("[fetching %d seqs]", scalar(@{$fetchAAR}), 1));
  my $concat_seqstring = $fetch_sqfile->fetch_subseqs($fetchAAR, 60, $seqfile); 
  Bio::Rfam::Utils::log_output_progress_local($logFH, "seqfetch", time() - $fetch_start_time, 0, 1, "", 1);

  $fetch_sqfile->close_sqfile();

  return;
}

#########################################################

sub help {
  print STDERR <<EOF;
  
  rfseed.pl - Add sequences to a SEED.

Usage:      rfseed.pl <file with subset of outlist lines for sequences to add to SEED> [options]

Options:    -l        align locally w.r.t the CM [default: globally]
            -h|-help  print this help, then exit

EOF
}
