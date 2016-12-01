#!/usr/bin/env perl 

# rfreplace.pl - Replace SEED sequences not in RFAMSEQ with identical or 
#                near identical matches in RFAMSEQ.
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

##################################################################
# EPN, Tue Apr 15 11:28:04 2014
# Notes on how to overhaul this script, when possible.
#
# From Janelia: /groups/eddy/home/nawrockie/notebook/14_0401_lsu_rfam/00LOG
# on April 15
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# What it should do is the following:
# - take rfmake.log (or maybe 'warnings') file as input
#  for each sequence that has a warning listed in rfmake.log:
#  interact with user to replace that sequence
#
#  case 1: user can delete it, replace with overlap, or leave it as is
#          and user can specify all other case 1s be handled as this
#          one was (save preference)
#  case 2: user can delete it or leave it alone and save preference
#  case 3: same options as case 1
#  case 4: we'll have to do the full drill of looking for highly
#          similar seqs in the hit list (much like what rfreplace.pl
#          currently does).
#          
#  for any sequence that does NOT have a warning, leave it in the
#  SEED, AS IT IS CURRENTLY ALIGNED! I know how to do this with
#  changes in inserts, but if we don't even want to change inserts
#  we'll have to map the consensus positions.... like I do in
#  rfsearch.pl to output a RF annotated SEED.. This will require some thought
#  IMPT: we can assume SEED has RF annotation (it should from rfsearch.pl)
#  
#  If there's no case 4s then we don't need to do the full drill of
#  looking for highly similar sequences in the hit list, which is good.
#
#  If there's no warnings at all, we can just exit: no replacement necessary.
#
# I also have notes on new features I need in rfreplace in
# ~/notebook/14_0223_lm/00LOG, on Feb 23.  Fortuantely, the idea above
# of using the rfmake.log file as required input gets around the nasty
# issue of finding highly similar seqs to all SEED seqs as a first pass
# from the 0223 log, because rfmake essentially already did that, and
# output the results to rfmake.log and warnings.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##################################################################


###################################################################
# Preliminaries:
# - set default values that command line options may change
# - process command line options
# - set input/output file names, and ensure input files exist
# - process DESC file

my $start_time = time();
my $executable = $0;
my $dbchoice   = "rfamseq";         # We'll always use this database.

# set default values that command line options may change
# REPLACEMENT mode options
my $df_id1_thr    = 0.90;           # default value for $id1_thr (see next line) changeable to <f> with -1 <f>
my $id1_thr       = $df_id1_thr;    # minimum fractional identity for a replacement candidate for a SEED seq
my $df_id2_thr    = 0.96;           # default value for $id2_thr (see next line) changeable to <f> with -2 <f>
my $id2_thr       = $df_id2_thr;    # maximum fractional identity allowed for two replacement candidates to same SEED seq
my $df_maxcands   = 10;             # default value for $maxcands, changeable to <n> with -x <n>
my $maxcands      = $df_maxcands;   # maximum number of candidates we'll collect for any single SEED seq
my $df_bitdiff    = 5;              # default value for $bitdiff (see next line), changeable to <n> with -b <n>
my $bitdiff       = $df_bitdiff;    # we'll align all outlist hits down to a score of lowest scoring SEED seq minus $df_bitdiff bits
# other options
my $inseed  = "SEED";               # name for input seed alignment,  redefined if -inseed
my $outseed = "SEED";               # name for output seed alignment, redefined if -outseed
my $do_local  = 0;                  # TRUE to align locally w.r.t. the CM
my $do_prob   = 0;                  # TRUE to include PPs in output alignment
my $do_help   = 0;                  # TRUE to print help and exit, if -h used
my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;
my $exec_description = "replace non-Rfamseq seqs in a SEED with Rfamseq seqs.";

my $options_okay = &GetOptions( "1=s"       => \$id1_thr,
                                "2=s"       => \$id2_thr,
                                "x=s"        => \$maxcands,
                                "b=s"        => \$bitdiff,
                                "p"          => \$do_prob,
                                "alocal"     => \$do_local,
                                "inseed=s"   => \$inseed,
                                "outseed=s"  => \$outseed,
                                "h|help"     => \$do_help);
if(! $options_okay) { 
  &help($exec_description); 
  die "ERROR, unrecognized option; "; 
}
if ( $do_help ) {
  &help($exec_description);
  exit(1);
}

# copy rfseed.log sideways if it exists
if (-e "rfreplace.log") { copy("rfreplace.log", "rfreplace.log.$$"); }

open($logFH, ">rfreplace.log") || die "ERROR unable to open rfreplace.log for writing";
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, $exec_description, 1);

# read in command line variables
my $infile = "";
if(scalar(@ARGV) != 0) { 
  $do_help = 1; 
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
                                               fileLocation => "$inseed",
                                               aliType      => 'seed'
                                              },
                                    'TBLOUT' => { 
                                                 fileLocation => "TBLOUT",
                                                },
                                    'DESC'   => $io->parseDESC("DESC"),
                                    'CM'     => $io->parseCM("CM"),
                                   );
my $seedmsa = $famObj->SEED;
my $desc    = $famObj->DESC;
my $cm      = $famObj->CM;
my $id      = $desc->ID;
my $acc     = $desc->AC;

# setup dbfile 
my $dbconfig  = $config->seqdbConfig($dbchoice);
my $Z         = $dbconfig->{"dbSize"};
my $fetchfile = $dbconfig->{"fetchPath"};
my $outlist = "outlist";
my $species = "species";
if(! -s $outlist) { die "ERROR $outlist does not exist, did you run rfsearch.pl and rfmake.pl?"; }
if(! -s $species) { die "ERROR $species does not exist, did you run rfsearch.pl and rfmake.pl?"; }
# make sure we have a CM file, and that it's newer than the SEED
if(! -s 'CM') { die "ERROR: CM does not exist, did you run rfsearch.pl?"; }
if(Bio::Rfam::Utils::youngerThan("SEED", "CM")) { die "ERROR SEED is younger than CM, did you run rfsearch.pl?"; }
# make sure we have an outlist and species file, and that they're newer than the CM
if(! -s $outlist) { die "ERROR: $outlist does not exist, did you run rfsearch.pl and rfmake.pl?"; }
if(! -s $species) { die "ERROR: $species does not exist, did you run rfsearch.pl and rfmake.pl?"; }
if(Bio::Rfam::Utils::youngerThan("CM", "$outlist")) { die "ERROR CM is younger than $outlist, did you run rfsearch.pl and rfmake.pl?"; }
if(Bio::Rfam::Utils::youngerThan("CM", "$species")) { die "ERROR CM is younger than $species, did you run rfsearch.pl and rfmake.pl?"; }
if($inseed ne "SEED" && $inseed eq $outseed) { 
  die "ERROR, with -inseed <f1> and -outseed <f2>, <f1> can't equal <f2>";
}

# output preamble: user, date, location, etc.
# first, determine maximum column width for pretty formatting
my @opt_lhsA = ();
my @opt_rhsA = ();
if($id1_thr    ne $df_id1_thr)    { push(@opt_lhsA, "# min fractional identity for a replacement candidate: "); push(@opt_rhsA, "$id1_thr [-1]"); }
if($id2_thr    ne $df_id2_thr)    { push(@opt_lhsA, "# max fractional id b/t two replacement candidates: ");    push(@opt_rhsA, "$id2_thr [-2]"); }
if($maxcands   ne $df_maxcands)   { push(@opt_lhsA, "# max num of replacement candidates per SEED seq: ");      push(@opt_rhsA, "$maxcands [-x]"); }
if($bitdiff    ne $df_bitdiff)    { push(@opt_lhsA, "# bit score difference from lowest scoring SEED: ");       push(@opt_rhsA, "$bitdiff [-b]"); }
if($do_prob)                      { push(@opt_lhsA, "# add posterior probs to output seed: ");                  push(@opt_rhsA, "yes [-p]"); }
if($do_local)                     { push(@opt_lhsA, "# align new sequences locally w.r.t. CM: ");               push(@opt_rhsA, "yes [-alocal]"); }
if($inseed ne "SEED")             { push(@opt_lhsA, "# input seed alignment in file: ");                        push(@opt_rhsA, "$inseed [-inseed]"); }
if($outseed ne "SEED")            { push(@opt_lhsA, "# output seed alignment to file: ");                       push(@opt_rhsA, "$outseed [-outseed]"); }
my $nopt = scalar(@opt_lhsA);
my $cwidth = ($nopt > 0) ? Bio::Rfam::Utils::maxLenStringInArray(\@opt_lhsA, $nopt) : 0;
if($cwidth < 14) { $cwidth = 14; } ; # max length of lhs string in log_output_preamble
$cwidth++; # one extra space

# now we have column width output preamble
my $do_stdout = 1;
Bio::Rfam::Utils::log_output_preamble($logFH, $cwidth, $user, $config, $desc, $do_stdout);
# and report options enabled by the user
for(my $z = 0; $z < $nopt; $z++) { 
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("%-*s%s\n", $cwidth, $opt_lhsA[$z], $opt_rhsA[$z]), $do_stdout);
}
Bio::Rfam::Utils::log_output_divider($logFH, $do_stdout);

# create hash of potential output files
my %outfileH = ();
my @outfile_orderA = ("$outseed.$$", "seedalignout", "rfreplace.log.$$");
$outfileH{"$outseed.$$"}      = "old seed alignment, copy of '$inseed' from before this script was run";
$outfileH{"seedalignout"}     = "tabular cmalign output for 'SEED'";
$outfileH{"rfreplace.log.$$"} = "old rfreplace.log file, copy of 'rfreplace.log' from before this script was run";

my $searchopts = get_searchopts($desc);

#############################################
Bio::Rfam::Utils::log_output_progress_column_headings($logFH, sprintf("per-stage progress:"), 1);
    
# Search all SEED seqs to get their scores and make sure they're detectable by cmsearch
my ($seed_seqstring, $seed_min_bitsc) = cmsearch_seed($seedmsa, $searchopts, $logFH);
my $min_bitsc = $seed_min_bitsc - $bitdiff;

# Align all hits above $min_bitsc (lowest scoring SEED sequence minus $bitdiff bits)
my $fafile  = "$$.fa";
my $stkfile = "$$.stk";
my $msa = cmalign_seed_and_hits($seed_seqstring, $fetchfile, $outlist, $min_bitsc, $do_local, $fafile, $stkfile);

my $nseed = $seedmsa->nseq;
my $nseq  = $msa->nseq;
my $nhit  = $nseq-$nseed;  
my $first_hit = $nseed+1; # index of first hit (non-SEED) seq in $msa
my @ridxA = (); # this will be a list of all seqs (their index in msa) that the user has 
                # chosen as existing SEED seq replacements or the original SEED seqs IFF
                # they are from RFAMSEQ and are so named (name/start-end), or -2 if 
                # no replacement candidates can be found
Bio::Rfam::Utils::setArray(\@ridxA, -1, $nseed); # set to -1 for all values

# Determine if cmalign changed the names of the sequences in the MSA
# because there were duplicate names. This should NOT happen because 
# in cmalign_seed_and_hits we added the prefix 'seed-' to SEED seqs 
# and 'hit-' to hits from outlist. Since there should be no 
# duplicates in outlist and there should be no duplicates in SEED
# then we should be good. We checked that there were nto duplicate
# names in the SEED in cmsearch_seed()
my $prefix_added = $msa->check_if_prefix_added_to_sqnames;
if($prefix_added) { 
  die "ERROR cmalign added numerical prefixes to sequences. This indicates there are duplicate sequence names in the SEED. That is not allowed. Fix and rerun."; }

# Preliminary step before we ask user to make modifications to the SEED,
# go through and make sure that each sequence has at least one hit 
# >= $id1 fractionally identical. Also check to see if any sequences are
# already from RFAMSEQ, we'll skip these and leave them as they are in SEED.
preliminary_replacement_check($msa, $nseed, $id1_thr, \@ridxA, $logFH);

# Parse the outlist and species to get tax info on each hit, 
# which we'll present to the user to aide their decision on
# which replacement candidate seq to pick.
my %infoHH;    # 2D hash, key 1: name/start-end (nse), key 2: "rank", "bitsc", "evalue", "sspecies" or "taxstr"
$io->parseOutlistAndSpecies($outlist, $species, 0, 0, $min_bitsc, 0, \%infoHH, undef, undef);
# '0, 0,' are the $emax and $ga_bitsc values, which are irrelevant for our purposes here

# For each seed sequence, collect replacement candidates and
# interact the user to select a replacement.
for(my $i = 0; $i < $nseed; $i++) { 
  my $seed_name = remove_rfreplace_prefix_from_sqname($msa->get_sqname($i), "seed");
  if($ridxA[$i] == -2) { # if == -2, there's not replacement candidates, we'll delete this seq, warn user against it first though
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\n Processing seed sequence %d of %d: %s\n DELETING: no replacement candidates > $id1_thr identical found.\n", ($i+1), $nseed, $seed_name), 1);
  }
  elsif($ridxA[$i] == $i) { # if == $i, then we found an identical match in RFAMSEQ, leave this one alone
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\n Processing seed sequence %d of %d: %s\n SKIPPING: Rfamseq source and name validated, it will remain in the seed.\n", ($i+1), $nseed, $seed_name), 1);
  }
  else { 
    my $ncand = interactive_replacement_selection($config, $i, $fafile, \%infoHH, $msa, $nseed, $id1_thr, $id2_thr, $maxcands, $Z, \@ridxA, $do_local, $logFH);
    if($ncand == 0) { 
      # special case, initial check for a sequence > $id1_thr identity passed, but all sequences
      # that were > $id1_thr were chosen as replacements for earlier sequences, inform the user
      $ridxA[$i] = -2;
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\n Processing seed sequence %d of %d: %s\n DELETING: no replacement candidates > $id1_thr identical remain.\n At least one of the earlier chosen replacements was > $id1_thr identical\n but you're not seeing that here because choosing the same\n replacement more than once is not allowed.\n", ($i+1), $nseed, $seed_name), 1);
    }
  }
}

Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n\n Finished processing seed sequences.\n Computing new SEED alignment...\n\n", 1);
    
# create new SEED, after copying current SEED sideways
my $tmpseed = "$$.tmp.seed";
my @newseedA = ();
my $nnew = 0;
for(my $i = 0; $i < $nseed; $i++) { 
  if   ($ridxA[$i] == -1) { my $z = $i+1; die "ERROR, end of replacement mode, seed seq $z not yet processed"; }
  elsif($ridxA[$i] == -2) { ; } # skip this sequence, we're deleting it (not replacing it) }
  else { 
    my $seed_name = $msa->get_sqname($ridxA[$i]);
    # don't remove prefix of 'seed-' or 'hit-' here
    push(@newseedA, $seed_name);
    $nnew++;
  }
}
redo_cmalign_alignment_of_subset($config, $fafile, "CM", $tmpseed, "seedalignout", \@newseedA, $do_local);

# now rename all seqs by removing seed- or -hit- prefix, and output new alignment
if (-e $outseed) { copy("$outseed", "$outseed.$$"); }
my $tmpmsa = Bio::Easel::MSA->new({
  fileLocation => "$tmpseed",
  forceText => "1"
});
for(my $i = 0; $i < $tmpmsa->nseq; $i++) { 
  $tmpmsa->set_sqname($i, remove_rfreplace_prefix_from_sqname($tmpmsa->get_sqname($i), "either"));
}
$tmpmsa->write_msa($outseed, "stockholm");

# done all work in replacement mode, print output file summary
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, 1);
# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
foreach my $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, 1);
  }
}
if(-e $outseed) { 
  Bio::Rfam::Utils::log_output_file_summary($logFH, $outseed, sprintf("new seed alignment; %d seq(s) replaced; %d seq(s) deleted", $nnew, ($nseed - $nnew)), 1);
}  
if( -e "rfreplace.log") { 
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfreplace.log", "log file (*this* output)", 1);
}

Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("#\n"), 1);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# Total time elapsed: %s\n", Bio::Rfam::Utils::format_time_string(time() - $start_time)), 1);
Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# [ok]\n"), 1);

foreach my $file ("$$.fa", "$$.fa.ssi", "$$.stk") { 
  if(-e $file) { unlink $file; }
}

close($logFH);
exit 0;

###############
# SUBROUTINES #
###############
#########################################################
# parse_outlist
#
sub parse_outlist {
  my ($outlist, $min_bitsc, $fetchAAR) = @_;

  Bio::Rfam::FamilyIO::validate_outlist_format($outlist);

  my $cur_bitsc = $min_bitsc + 1;
  my $nseq = 0;
  my $nres = 0;
  my $line; 

  open(IN, $outlist) || die "ERROR unable to open $outlist"; 
  while($line = <IN>) {
    # example outlist line:
    #  87.5  4.8e-18      FULL  AACZ03038953.1        -    25367    25465    +       1   101     no  Pan_troglodytes_(chimpa..[9598]    Pan troglodytes chromosome 5 Contig52.80, whole genome shotgun sequence.
    if($line !~ m/^\#/) { 
      $line =~ s/^\s+//; # remove leading whitespace
      my @out_elA = split(/\s\s+/, $line); # note: we separate by double spaces
      my ($cur_bitsc, $name, $start, $end) = ($out_elA[0], $out_elA[3], $out_elA[5], $out_elA[6]);
      
      if($cur_bitsc >= $min_bitsc){ 
        my $nse = "$name/$start-$end";
        $nres += Bio::Rfam::Utils::nse_sqlen($nse);
        $nseq++;
        
        my ($validated, undef, undef, undef, undef) = Bio::Rfam::Utils::nse_breakdown($nse);
        if(! $validated) { die "ERROR something wrong with outlist line, can't break it down to name/start-end format ($line)"; }
        
        push(@{$fetchAAR}, [$nse, $start, $end, $name]); 
        #print ("added seq $name/$start-$end\n");
      }
      else { # bit score is below our minimum: stop
        last;
      }
    }
  }
  close(IN);

  return ($nseq, $nres);
}
#########################################################
sub get_searchopts {
  my ($desc) = (@_);
  
  my $searchopts = $desc->{'SM'};
  $searchopts =~ s/\s*CM\s*/ /;     # remove 'CM',  
  $searchopts =~ s/\s*SEQDB\s*/ /;  # remove 'SEQDB'
  $searchopts =~ s/cmsearch//;      # remove 'cmsearch'
  if($searchopts !~ /\-\-cpu\s+\S+/) { die "ERROR, SM from DESC ($searchopts) does not include --cpu <> it should..."; }
  $searchopts =~ s/\-\-cpu\s+\S+/\-\-cpu 0/;  
  $searchopts .= " --rfam --toponly"; # add --rfam and --toponly
  return $searchopts;
}

#########################################################
# cmsearch_seed
# Returns: $seed_seqstring: all seed seqs in FASTA format concatenated into 1 string
#          $min_bitsc:      minimum bit score of all seed sequences
# Dies:    If any seed sequence is not found by cmsearch.
sub cmsearch_seed { 
  my ($seedmsa, $searchopts, $logFH) = @_;

  # first output seed as fasta file
  my $seed_fafile = "s.$$.fa";
  my $nseed = $seedmsa->nseq;
  $seedmsa->write_msa($seed_fafile, "fasta");
  my $seed_seqstring = Bio::Rfam::Utils::fileToString($seed_fafile);
  my $seed_search_start_time = time();

  my $tblout = "s.$$.tbl";
  Bio::Rfam::Utils::log_output_progress_local($logFH, "cmsearch", time() - $seed_search_start_time, 1, 0, "[$nseed SEED seqs]", 1);
  Bio::Rfam::Infernal::cmsearch_wrapper($config, "", "--tblout $tblout " . $searchopts, "CM", $seed_fafile, undef, "s.$$.err", "", "", 1);
  Bio::Rfam::Utils::log_output_progress_local($logFH, "cmsearch", time() - $seed_search_start_time, 0, 1, "", 1);
  unlink($seed_fafile);

  # process search results to get lowest scoring SEED seq
  # die if we don't find a hit to any single SEED seq
  my $seed_min_bitsc = parse_tblout($tblout, $seedmsa, $logFH);
  unlink $tblout;

  return($seed_seqstring, $seed_min_bitsc);
}
########################################
# parse_tblout:
# Returns: minimum bit score of lowest scoring seed sequence hit
# Dies:    if there's 0 hits to any seed sequence 
sub parse_tblout {
  my ($tblout, $seedmsa, $logFH) = @_;

  open(IN, $tblout) || die "ERROR unable to open $tblout"; 

  my $min_bitsc = undef;
  my %foundH = (); # keep track of which SEED sequences we've seen a hit for
  while(my $line = <IN>) { 
    if($line !~ m/^\#/) { 
      # Vault-sample6        -         SEED                 -          cm        1       95        1       95      +    no    1 0.59   0.0   94.5     2e-17 !   -
      chomp $line;
      my @elA = split(/\s+/, $line);
      my ($seqname, $bitsc) = ($elA[0], $elA[14]);
      if(! exists $foundH{$seqname}) { $foundH{$seqname} = 1; }
      $min_bitsc = $bitsc;
    }
  }
  close(IN);

  my $unfound = 0;
  for(my $i = 0; $i < $seedmsa->nseq; $i++) { 
    my $sqname = $seedmsa->get_sqname($i);
    if(! exists $foundH{$sqname}) { 
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "ERROR, cmsearch unable to find SEED sequence $sqname\n", 1);
      $unfound++;
    }      
  }
  if($unfound > 0) { die "$unfound SEED sequences not found by cmsearch"; }

  return $min_bitsc;
}
########################################
# cmalign_seed_and_hits
#
# Args:    $seed_seqstring:  string of all SEED seqs concatenated together in FASTA
#          $fetchfile:       master fasta file to fetch seqs from
#          $outlist:         name of outlist file, usually 'outlist'
#          $min_bitsc:       we'll align all hits above this bit score
#          $do_local:        '1' to align locally w.r.t. the CM
#          $fafile:          the name of the fasta file we create and use as input to cmalign
#          $stkfile:         the name of the output alignment to create
#
# Returns: $msa:             the MSA object we create
sub cmalign_seed_and_hits { 
  my ($seed_seqstring, $fetchfile, $outlist, $min_bitsc, $do_local, $fafile, $stkfile) = @_;

  my @fetchAA = (); 
  my ($nhit, $nhit_res) = parse_outlist($outlist, $min_bitsc, \@fetchAA);
  my $hit_seqstring     = Bio::Rfam::Utils::fetch_from_sqfile_wrapper($fetchfile, \@fetchAA, 1, undef, $logFH, 1, ""); # undef is for default line width, "" means return a string of all seqs

  # Remove descriptions from the sequences. We do this because Easel
  # indexes the sequences in an MSA by including the seqs with
  # descriptions (#=GS annotation) first, then including all other seqs
  # later.  So to make sure we have the proper order (SEED seqs, then
  # hits) in the eventual ESL_MSA data structure which will be created
  # when we read in the cmalign output alignment, we need to remove
  # descriptions from all seqs.
  my $nodesc_seed_seqstring = Bio::Rfam::Utils::remove_descriptions_from_fasta_seq_string($seed_seqstring);
  my $nodesc_hit_seqstring  = Bio::Rfam::Utils::remove_descriptions_from_fasta_seq_string($hit_seqstring);

  # add 'seed-' prefix to beginning of all seed sequence names
  # add 'hit-' prefix to beginning of all hit sequence names
  $nodesc_seed_seqstring =~ s/\>/\>seed-/g;
  $nodesc_hit_seqstring =~ s/\>/\>hit-/g;

  open(OUT, ">" . $fafile) || die "ERROR unable to open $fafile for writing seqs to";
  print OUT $nodesc_seed_seqstring;
  print OUT $nodesc_hit_seqstring;
  close(OUT);

  # align seqs
  my $align_opts = "-o $stkfile --noprob"; # PPs will just increase size of file -- we won't use them
  my $cmalign_file = "$$.cmalign";
  if(! $do_local) { $align_opts .= " -g"; }
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $align_opts, "CM", $fafile, "$$.cmalign", "a.$$.err", $nhit, $nhit_res, 1, 0, "", -1, $logFH, 1);
  foreach my $file ($cmalign_file) { unlink $file; }
  
  my $msa = Bio::Easel::MSA->new({
     fileLocation => "$stkfile",
  });

  return $msa;
}
########################################
# preliminary_replacement_check
#
# Args:    $msa:               alignment of SEED sequences and hits
#          $nseed:             number of seqs in the seed, these will be the first $nseed seqs in $msa
#          $id1_thr:           minimum threshold for a replacement candidate
#          $ridxAR:            ref to array [0..i..nseed-1], the indices of replacement for seq i
#          $logFH:             log output file handle
#          1:         TRUE to echo log messages to stdout
#
# Returns: void
# Dies:    if any seed sequence doesn't have any replacement candidates (>= $id1 fractionally identical)
#          if all seed sequences derive from RFAMSEQ and are properly named
sub preliminary_replacement_check { 
  my ($msa, $nseed, $id1_thr, $ridxAR, $logFH) = @_;

  my $nseq = $msa->nseq;
  my $nfailed_id1 = 0;
  my $nfrom_rfamseq = 0;
  my $first_hit = $nseed;
  my $min_max_id1 = 1.0;
  for(my $i = 0; $i < $nseed; $i++) { 
    my $seed_name = remove_rfreplace_prefix_from_sqname($msa->get_sqname($i), "seed");
    my $passed_id1 = 0;
    my $max_id1 = 0.;
    for(my $j = $first_hit; $j < $nseq; $j++) { 
      my $id1 = $msa->pairwise_identity($i, $j);
      $max_id1 = ($id1 > $max_id1) ? $id1 : $max_id1;
      if($id1 >= $id1_thr) { 
        $passed_id1 = 1;
      }
      if($id1 >= 0.999999) { # identical
        my $hit_name  = remove_rfreplace_prefix_from_sqname($msa->get_sqname($j), "hit");
        if($seed_name eq $hit_name) { 
          Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("! Warning: $seed_name SEED seq \#%d appears to already derive from RFAMSEQ and is properly named.\n", ($i+1)), 1);
          $ridxAR->[$i] = $i; 
          $nfrom_rfamseq++;
        }
      }
    }
    if(! $passed_id1) { 
      if($nfailed_id1 == 0) { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n", 1); }
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("! Warning: didn't find any hits with >= $id1_thr fractional identity (max found: %.2f) to $seed_name.\n", $max_id1), 1); 
      $min_max_id1 = ($max_id1 < $min_max_id1) ? $max_id1 : $min_max_id1;        
      $nfailed_id1++;
      $ridxAR->[$i] = -2; # flag for deleting this hit
    }
  }
  if($nfailed_id1 > 0) { 
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\n\n! Warning: $nfailed_id1 seed seqs didn't have a match >= $id1_thr to any hit.\n! If you proceed these seed seqs will be deleted.\n! Alternatively, you can abort and rerun with -1 <f>\n! <f>=%.2f should cause all seed seqs to have matches.\n\n", $min_max_id1 - 0.01), 1);
    # ask the user if they want to proceed, and process their response
    my $keep_going = 1;
    while($keep_going) { 
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "! Do you want to continue? ['y' or 'n']\n", 1);
      my $choice = <>;
      chomp $choice;
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "$choice", 0); # note we don't echo it back to screen the '0' 
      if($choice =~ m/^\s*([Nn])\s*$/) { 
        quit_early($logFH);
      }
      elsif($choice =~ m/^\s*([Yy])\s*$/) { 
        $keep_going = 0;
      }
    }
  }
  if($nfrom_rfamseq == $nseed) { die "ERROR, all $nseed SEED sequences look like they derive from RFAMSEQ, no need to replace them. Use rfseed.pl -a or -d to add or delete seqs"; }

  return;
}
#########################################################
# interactive_replacement_selection
#  Find replacement candidate for SEED seq idx $i in $msa and 
#  interact with user to determine a replacement.
# Args:    $config:       the config, with path to infernal executables
#          $i:            index in MSA of seed seq we're currently looking at
#          $fafile:       fasta file with all the seqs in $msa
#          $infoHHR:      ref to 2D hash, key 1: name/start-end (nse), key 2: "rank", "bitsc", "evalue", "sspecies" or "taxstr"
#          $msa:          the MSA with seed seqs plus hit seqs
#          $nseed:        number of seed seqs, the first $nseed seqs in the MSA
#          $id1_thr:      from -1 opt, min identity between seed and replacement candidates
#          $id2_thr:      from -2 opt, max identity between any two replacement candidates
#          $maxcands:     max # of candidate replacements to present to user
#          $Z:            total DB size for cmsearch
#          $ridxAR:       ref to array of replacement indices for each seed seq [0..i..nseed-1]
#          $do_local:     TRUE to align locally with respect to the CM if cmalign is used
#          $logFH:        file handle for output log file
#
# Returns: Number of candidate replacement sequences that were listed as an option 
#          for replacement to the user. If this number is 0, caller will know that
#          there were zero sequences > $id1_thr fractionally identical to $i that
#          *WERE NOT ALREADY CHOSEN AS REPLACEMENTS FOR EARLIER SEED SEQS $ip < $i*

sub interactive_replacement_selection { 
  my ($config, $i, $fafile, $infoHHR, $msa, $nseed, $id1_thr, $id2_thr, $maxcands, $Z, $ridxAR, $do_local, $logFH) = @_;

  my $first_hit = $nseed;
  my $ncand = 0;
  my $nseq = $msa->nseq;
  my $seed_name = remove_rfreplace_prefix_from_sqname($msa->get_sqname($i), "seed");

  # gather replacement candidates
  # arrays [0..x..ncand-1] with info on replacement candidate <x>
  my @idxA      = (); # index in $msa
  my @nameA     = (); # sqname of candidate
  my @id1A      = (); # identity to seed seq $i
  my @sspeciesA = (); # 'short species' string
  my @taxstrA   = (); # taxonomic string 
  my @eidA      = (); # identity to closest existing (already confirmed/replaced) seed seq
  my @etaxstrA  = (); # tax string of closest existing (already confirmed/replaced) seed seq
  my $id2_failure = 0; # set to '1' if a sequence is > $id2_thr identical to another replacement candidate
  my $using_as_replacement = 0; # set to '1' if a sequence $ip with MSA index $ip < $i is already using 
                                # this sequence to replace it.
  for(my $j = $first_hit; $j < $nseq; $j++) { 
    # first, make sure we haven't already selected sequence $j as a replacement
    # for an earlier seed sequence
    $using_as_replacement = 0;
    for(my $ip = 0; $ip < $i; $ip++) { 
      if ($ridxAR->[$ip] == $j) { 
        #printf("i is $i, j is $j, we determined that we are already using sequence j to replace seed sequence $ip\n");
        $using_as_replacement = 1;
      }
    }
    if(! $using_as_replacement) { 
      my $id1 = $msa->pairwise_identity($i, $j);
      #printf("j: $j id1: $id1 ($id1_thr)\n");
      if($id1 >= $id1_thr) { 
        # we could have a replacement candidate, first make sure it's not > $id2_thr identical to 
        # any replacement candidates we've found thus far (there are $ncand thus far)
        $id2_failure = 0;
        for(my $k = 0; $k < $ncand; $k++) { 
          if($msa->pairwise_identity($j, $idxA[$k]) > $id2_thr) { 
            #printf("id2 failure %.2f > %.2f (%s %s)\n", $msa->pairwise_identity($j, $idxA[$k]), $id2_thr, $msa->get_sqname($j), $msa->get_sqname($idxA[$k]));
            $id2_failure = 1; 
            last;
          }
        }
        if(! $id2_failure) { # okay, we've got a sufficiently unique replacement candidate
          $ncand++;
          my $sqname = remove_rfreplace_prefix_from_sqname($msa->get_sqname($j), "hit");
          # make sure we have $sqname in infoHHR, we better (since we already dealt with prefix removal if nec)
          if(! exists $infoHHR->{$sqname}) { die "ERROR sequence $sqname aligned but not in outlist..."; }
          # determine the max percent identity between this replacement candidate
          # and all replacement seqs we've chosen thus far
          my $max_eid = "-";
          my $eid_taxstr = "N/A";
          for(my $i2 = 0; $i2 < $nseed; $i2++) { 
            if($ridxAR->[$i2] >= 0) { # if -1 we haven't replaced it yet, if -2 we will delete it
              my $eid = $msa->pairwise_identity($j, $ridxAR->[$i2]);
              if($max_eid eq "-" || $eid > $max_eid) { 
                $max_eid = $eid;
                my $i2_sqname = remove_rfreplace_prefix_from_sqname($msa->get_sqname($ridxAR->[$i2]), "either");
                if(defined $infoHHR->{$i2_sqname}) { 
                  $eid_taxstr = $infoHHR->{$i2_sqname}{"taxstr"};
                }
                else { 
                  $eid_taxstr = "unavailable";
                }
              }
            }
          }

          push(@idxA,      $j);
          push(@nameA,     $sqname);
          push(@id1A,      $id1);
          push(@sspeciesA, $infoHHR->{$sqname}{"sspecies"});
          push(@taxstrA,   $infoHHR->{$sqname}{"taxstr"});
          push(@eidA,      $max_eid);
          push(@etaxstrA,  $eid_taxstr);
          
          if($ncand == $maxcands) { last; }
        } # end of 'if (! $id2_failure)'
      } # end of 'if $id1 >= $id1_thr'
    } # end of 'if (! $using_as_replacment)'
  } # end of 'for ($j = $nseed; $j < $nseq; $j++)' loop
  if($ncand == 0) { 
    # we're done, there's no acceptable candidates, inform caller of this by returning 0
    return 0; 
  }

  # now reorder all the arrays, so that the candidates occur in decreasing order
  # of percent identity to the seed seq they may replace. We do an embarassingly
  # inefficient O(N^2) sort here, but it should be okay since typically we'll only
  # have very few candidates (< 10).
  if($ncand > 1) { # resorting only makes sense if we have more than 1 candidate
    my @already_sortedA = ();
    Bio::Rfam::Utils::setArray(\@already_sortedA, 0, $ncand);
    my @orderA = ();
    for(my $i = 0; $i < $ncand; $i++) { 
      my $max  = 0.0;
      my $i2max = -1;
      for(my $i2 = 0; $i2 < $ncand; $i2++) { 
        if(! $already_sortedA[$i2]) { 
          if($i2max == -1 || $id1A[$i2] > $max) { 
            $i2max = $i2;
            $max   = $id1A[$i2];
          }
        }
      }
      push(@orderA, $i2max);
      $already_sortedA[$i2max] = 1;
    }
    # now reorder all arrays 
    reorder_array(\@idxA,      $ncand, \@orderA);
    reorder_array(\@nameA,     $ncand, \@orderA);
    reorder_array(\@id1A,      $ncand, \@orderA);
    reorder_array(\@sspeciesA, $ncand, \@orderA);
    reorder_array(\@taxstrA,   $ncand, \@orderA);
    reorder_array(\@eidA,      $ncand, \@orderA);
    reorder_array(\@etaxstrA , $ncand, \@orderA);
  }

  # get info we need for formatting
  my $xmatchlen = Bio::Rfam::Utils::maxLenStringInArray(\@nameA);
  my $xspeclen  = Bio::Rfam::Utils::maxLenStringInArray(\@sspeciesA);
  if($xmatchlen < length("sqname")) { $xmatchlen = length("sqname"); }
  my $ndig = 1;
  if   ($ncand > 99) { $ndig = 3; }
  elsif($ncand > 9)  { $ndig = 2; }
  
  # big while loop that prints candidates and asks user how to replace
  my $keep_printing_candidates = 1;
  my $max_line_cnt = 25; # we'll reprint candidate list every $max_line_cnt lines or so
  my $line_ctr = 0;
  my $cand_ctr = 0;
  while($keep_printing_candidates) { 
    $cand_ctr++;
    # print column labels
    sleep(0.5); # a pause: helpful for the user to follow what the script is doing.
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\n %srocessing seed sequence %d of %d: %s\n REPLACING; list of replacement candidates:\n\n", 
                                                             ($cand_ctr > 1) ? "Still p" : "P", 
                                                             ($i+1), 
                                                             $nseed, 
                                                             $seed_name), 
                                             1);
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\t%-*s  %-*s   %%id  species\n", 
                                                             $ndig+2, "idx",
                                                             $xmatchlen, "sqname"), 
                                             1);
    my $dash1 = Bio::Rfam::Utils::monocharacterString("-", $ndig+2);
    my $dash2 = Bio::Rfam::Utils::monocharacterString("-", $xmatchlen);
    my $dash3 = Bio::Rfam::Utils::monocharacterString("-", $xspeclen);
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\t$dash1  $dash2  ----  $dash3\n", 1);
    $line_ctr+=3;
    for(my $i = 0; $i < $ncand; $i++) { 
      # print idx, match name, and %id to seed seq
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\t[%*d]  %-*s  %3d%%  %s\n", 
                                                               $ndig, $i+1, 
                                                               $xmatchlen, $nameA[$i], 
                                                               id2percent($id1A[$i]), 
                                                               $sspeciesA[$i]), 
                                               1);
      $line_ctr++;
    }
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n", 1);

    # print options for getting more info or choosing a replacement:
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\tchoice   effect\n", 1);
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\t------   -----------------------------------------------------------------\n", 1);
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\tr<idx>   choose candidate <idx> as replacement\n", 1);
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\tt<idx>   display tax info on candidate <idx> and closest existing seed seq\n", 1);
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\ta<idx>   display pairwise alnment of <idx> and seed seq being replaced\n", 1);
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\ts<idx>   display cmsearch alignment of candidate <idx>\n", 1);
    if($ncand > 1) { 
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\tT        display tax info on all candidates\n", 1);
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\tA        display multiple alignment of all candidates and seed seq\n", 1);
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\tS        display cmsearch alignments of all candidates\n", 1);
    }
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\td        delete this seed sequence\n", 1);
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\tq        quit early (SEED will stay the same)\n", 1);
    $line_ctr += 7;
     
    my $keep_printing_enter_choice = 1;
    my $choice = undef;
    my $do_delete = 0;
    my $do_replace_ali = 0;
    my ($letter, $cidx);
    # now the loop for prompting the user for a choice, keep doing this until they select a replacement or decide to delete
    while($keep_printing_enter_choice) { 
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n\tEnter choice:\n\t", 1);
      $line_ctr+=2;
      $choice = <>;
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "$choice", 0); # note we don't echo it back to screen the '0' 
      $line_ctr++;
      chomp $choice;
      if($choice =~ m/^\s*([QqDdTAS])\s*$/) { # valid choice, no idx
        $letter = $1;
        if($letter =~ m/[Qq]/) { # quit, with cleanup and without rewriting SEED
          Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\n\tUser chose to quit and leave SEED alone and clean up temporary files.\n"), 1);
          quit_early($logFH);
        }
        if($letter =~ m/[Dd]/) { # delete this seq
          $ridxAR->[$i] = -2; # flag for 'delete this seq from seed'
          Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\n\tUser chose to delete seed sequence $seed_name.\n"), 1);
          $keep_printing_candidates   = 0; # one of two ways keep_printing_candidates gets set to 0
          $keep_printing_enter_choice = 0; 
        }
        elsif($letter eq "T") { 
          $line_ctr += process_choice_taxinfo_all($ncand, \@taxstrA, $logFH); 
        }
        elsif($letter eq "A") { 
          my @tmpA = ($seed_name, @nameA);
          $line_ctr += process_choice_cmalign($config, $msa, $fafile, \@tmpA, $do_local, $logFH); 
        }
        elsif($letter eq "S") { 
          for($cidx = 1; $cidx <= $ncand; $cidx++) { 
            $line_ctr += process_choice_cmsearch($config, $msa, $fafile, $cidx, $nameA[($cidx-1)], $Z, $logFH); 
          }
        }
      } # end of if($choice =~ m/DdTAS/) 
      elsif($choice =~ m/^\s*([tSsAaRr])\s*(\d+)\s*$/) {  # valid choice + idx
        $letter = $1;
        $cidx   = $2;
        if($cidx > 0 && $cidx <= $ncand) { # valid index
          if   ($letter eq "t") { 
            $line_ctr += process_choice_taxinfo_single($cidx, $taxstrA[($cidx-1)], $etaxstrA[($cidx-1)], $eidA[($cidx-1)], $logFH); 
          }
          elsif($letter eq "a") { 
            my @tmpA = ($seed_name, $nameA[($cidx-1)]);
            $line_ctr += process_choice_cmalign($config, $msa, $fafile, \@tmpA, $do_local, $logFH); 
          }
          elsif($letter eq "s") { 
            $line_ctr += process_choice_cmsearch($config, $msa, $fafile, $cidx, $nameA[($cidx-1)], $Z, $logFH); 
          }
          elsif($letter =~ m/[Rr]/) { # replacement choice
            $ridxAR->[$i] = $idxA[($cidx-1)];
            Bio::Rfam::Utils::printToFileAndOrStdout($logFH, 
                                                     sprintf("\n\tUser chose to replace seed sequence $seed_name with " . $nameA[($cidx-1)] . " (%3d%% identical).\n", 
                                                             id2percent($id1A[($cidx-1)])), 
                                                     1);
            $keep_printing_candidates   = 0; # one of two ways keep_printing_candidates gets set to 0
            $keep_printing_enter_choice = 0; 
          }
        }
      }
      elsif($choice =~ m/^\s*([rtas])\s*$/) {  # user forgot to include index
        $letter = $1;
        Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n\tYou forgot to include an index after $letter.\n", 1);
        $line_ctr += 2;
      }
      # printf("line_ctr: $line_ctr (max_line_cnt: $max_line_cnt)\n");
      if($line_ctr > $max_line_cnt) { # should we print the candidate list again?
        $keep_printing_enter_choice = 0; # this flag will force the candidate list to be printed again
      }
    } # end of while($keep_printing_enter_choice
  } # end of while($keep_printing_candidates)

  return $ncand;
}
#########################################################
# process_choice_taxinfo_single
# Output taxonomic information on a single candidate
# and it's closest match in the existing SEED.
# Returns: number of lines printed (3).
sub process_choice_taxinfo_single { 
  my ($cidx, $cand_taxstr, $eid_taxstr, $eid, $logFH) = @_;

  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n", 1);
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH,   sprintf("\tCandidate %-3d               : $cand_taxstr\n", $cidx), 1);
  if($eid_taxstr ne "N/A") { 
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\tClosest seed match [%3d%% id]: $eid_taxstr\n", id2percent($eid)), 1);
  }
  else { 
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\tClosest seed match          : N/A, no seqs in seed yet\n", 1);
  }
  return 3;
}
#########################################################
# process_choice_taxinfo_all
# Output taxonomic information on all candidates
# Returns: number of lines printed ($ncand).
sub process_choice_taxinfo_all { 
  my ($ncand, $taxstrAR, $logFH) = @_;

  my ($i, $a);

  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n", 1);
  if($ncand == 1) { 
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH,   sprintf("\tCandidate %-3d : %s\n", 1, $taxstrAR->[0], 1));
    return 1; 
  }

  # If we have more than one seq (which we should, since printing
  # is only sensible if there's more than one) 
  # we go through a bit of trouble to format the output nicely,
  # first by outputting the common prefix to all candidates 
  # and then by having fixed width columns for all other
  # taxonomic string tokens, such that each prefix level 
  # of taxonomy is lined up, 
  # like this:
  #
  # All 10 candidates share the first 8 taxonomic string tokens:
  # 
  #  Eukaryota;  Metazoa;  Chordata;  Craniata;  Vertebrata;  Euteleostomi;  Mammalia;  Eutheria;
  #
  # Remainder of taxonomic strings for each candidate:
  # 
  # Candidate 1   :  Euarchontoglires;  Scandentia     ;  Tupaiidae     ;  Tupaia.     ;
  # Candidate 2   :  Laurasiatheria  ;  Cetartiodactyla;  Ruminantia    ;  Pecora      ;  Bovidae        ;  Bovinae        ;  Bos.       ;
  # Candidate 3   :  Euarchontoglires;  Primates       ;  Haplorrhini   ;  Catarrhini  ;  Hominidae      ;  Gorilla.       ;
  # Candidate 4   :  Euarchontoglires;  Primates       ;  Haplorrhini   ;  Platyrrhini ;  Cebidae        ;  Callitrichinae ;  Callithrix.;
  # Candidate 5   :  Laurasiatheria  ;  Cetartiodactyla;  Cetacea       ;  Odontoceti  ;  Delphinidae    ;  Tursiops.      ;
  # Candidate 6   :  Euarchontoglires;  Primates       ;  Haplorrhini   ;  Catarrhini  ;  Cercopithecidae;  Cercopithecinae;  Macaca.    ;
  # Candidate 7   :  Laurasiatheria  ;  Cetartiodactyla;  Tylopoda      ;  Camelidae   ;  Vicugna.       ;
  # Candidate 8   :  Euarchontoglires;  Glires         ;  Lagomorpha    ;  Leporidae   ;  Oryctolagus.   ;
  # Candidate 9   :  Euarchontoglires;  Primates       ;  Haplorrhini   ;  Catarrhini  ;  Cercopithecidae;  Cercopithecinae;  Macaca.    ;
  # Candidate 10  :  Laurasiatheria  ;  Chiroptera     ;  Megachiroptera;  Pteropodidae;  Pteropodinae   ;  Pteropus.      ;
  #
  # instead of just printing them, like this:
  #
  # Candidate 1   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Scandentia; Tupaiidae; Tupaia.
  # Candidate 2   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Laurasiatheria; Cetartiodactyla; Ruminantia; Pecora; Bovidae; Bovinae; Bos.
  # Candidate 3   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Hominidae; Gorilla.
  # Candidate 4   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Platyrrhini; Cebidae; Callitrichinae; Callithrix.
  # Candidate 5   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Laurasiatheria; Cetartiodactyla; Cetacea; Odontoceti; Delphinidae; Tursiops.
  # Candidate 6   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Cercopithecidae; Cercopithecinae; Macaca.
  # Candidate 7   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Laurasiatheria; Cetartiodactyla; Tylopoda; Camelidae; Vicugna.
  # Candidate 8   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Glires; Lagomorpha; Leporidae; Oryctolagus.
  # Candidate 9   : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Euarchontoglires; Primates; Haplorrhini; Catarrhini; Cercopithecidae; Cercopithecinae; Macaca.
  # Candidate 10  : Eukaryota; Metazoa; Chordata; Craniata; Vertebrata; Euteleostomi; Mammalia; Eutheria; Laurasiatheria; Chiroptera; Megachiroptera; Pteropodidae; Pteropodinae; Pteropus.
  # 
  # split up tax strings into tokens
  my @tokAA = ();
  my @ntokA = ();
  for($i = 0; $i < $ncand; $i++) { 
    push(@tokAA, [ split(";", $taxstrAR->[$i]) ]); 
    $ntokA[$i] = scalar(@{$tokAA[$i]});
    # printf("ntokA[$i]: $ntokA[$i]\n");
  }
  my $max_ntok = Bio::Rfam::Utils::maxArray(\@ntokA, $ncand);
  my $min_ntok = Bio::Rfam::Utils::maxArray(\@ntokA, $ncand);
  # determine max len token at each prefix level, for easier-to-read output
  my @xwidthA = (); # 0..max_ntok, max length of token in all candidates
  Bio::Rfam::Utils::setArray(\@xwidthA, 0, $max_ntok);
  for($a = 0; $a < $max_ntok; $a++) { 
    for($i = 0; $i < $ncand; $i++) { 
      if($a < $ntokA[$i]) { 
        my $toklen = length($tokAA[$i][$a]);
        if($toklen > $xwidthA[$a]) { 
          $xwidthA[$a] = $toklen;
        }
      }
    }
  }
  # determine first token level where all strings are not identical
  my $last_id = -1;
  my $found_diff = 0;
  my $common_prefix = "";
  for($a = 0; $a < $min_ntok; $a++) { 
    my $tok = $tokAA[0][$a];
    for($i = 1; $i < $ncand; $i++) { 
      if($tokAA[$i][$a] ne $tok) { 
        $found_diff = $a;
        last;
      }
    }
    if($found_diff) { 
      last;
    }
    else { 
      $last_id++;
      $common_prefix .= " " . $tokAA[0][$a] . ";"
    }
  }

  # now output
  if(($last_id+1) < $max_ntok) { 
    if($last_id >= 0) { 
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\tAll %d candidates share the first %d taxonomic string tokens:\n\n\t$common_prefix\n", $ncand, ($last_id+1)), 1);
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n\tRemainder of taxonomic strings for each candidate:\n\n", 1);
    }
    for($i = 0; $i < $ncand; $i++) { 
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\tCandidate %-3d :", ($i+1)), 1);
      for($a = $last_id+1; $a < $max_ntok; $a++) { 
        if($ntokA[$i] > $a) { 
          Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf(" %-*s;", $xwidthA[$a], $tokAA[$i][$a]), 1);
        }
      }
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n", 1);
    }
  }
  elsif(($last_id+1) == $max_ntok) { 
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("\tAll %d candidates have identical taxonomic strings:\n\n\t$common_prefix\n", $ncand, ($last_id+1)), 1);
  }
  return $ncand;
}
#########################################################
# process_choice_cmsearch
# Potentially removes Easel added prefix in seq names and
# then calls function to perform cmsearch.
#
# Returns: number of lines printed
sub process_choice_cmsearch { 
  my ($config, $msa, $fafile, $cidx, $cand_sqname, $Z, $logFH) = @_;

  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n\tcmsearch alignment of candidate $cidx, with non-canonicals marked with 'v':\n", 1);
  return do_and_display_cmsearch_alignment($config, $fafile, $cand_sqname, $Z, "\t", $logFH);
}
#########################################################
# process_choice_cmalign
#
# Align sequences to the CM and add "ID" annotation,
# after possibly removing Easel-added prefixes on 
# the names. Prints alignment to the screen 
# and $logFH, for user to look at.
# 
# Returns: number of lines printed
sub process_choice_cmalign { 
  my ($config, $msa, $fafile, $nameAR, $do_local, $logFH) = @_;

  my $nprinted = 0;
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\n\tAlignment of seed sequence and candidate replacement(s) to CM:\n", 1);

  # do alignment
  my $stkfile = "tmp.$$.stk";
  my $cmafile = "tmp.$$.cmalign";
  redo_cmalign_alignment_of_subset($config, $fafile, "CM", $stkfile, $cmafile, $nameAR, $do_local);

  # read in alignment we just made, we only need to do this 
  # so we can add ID annotation indicating conserved columns
  my $newmsa = Bio::Easel::MSA->new({
     fileLocation => "$stkfile",
  });
  $newmsa->addGC_identity(0); # 0 says indicate 100% identical columns with '*', not residues
  # write out new alignment, unfortunately we have to do this to a file b/c that's how Easel does it
  my $stkfile2 = "tmp2.$$.stk";
  $newmsa->write_msa($stkfile2, "stockholm");

  # output alignment
  open(IN, $stkfile2) || die "ERROR unable to open $stkfile2 for reading";
  while(my $line = <IN>) { 
    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "\t" . $line, 1);
    $nprinted++;
  }
  close(IN);

  # clean up
  foreach my $file ($stkfile, $stkfile2, $cmafile) { 
    unlink $file;
  }

  return $nprinted;
}
#########################################################
# do_and_display_cmsearch_alignment
# Search the CM against a single sequence and output 
# the resulting hit alignment. 
#
# Dies:    if no hit is found, shouldn't happen
# Returns: number of lines printed
sub do_and_display_cmsearch_alignment {
  my ($config, $fafile, $sqname, $Z, $prefix, $logFH) = @_;
  
  # fetch fasta seq to single seq fasta file
  my @nameA = ($sqname);
  my $tmpfile = "single.$$.fa";
  Bio::Rfam::Utils::fetch_from_sqfile_wrapper($fafile, \@nameA, 0, undef, undef, 0, $tmpfile); # undef: default line length, undef, 0 means don't update log file
      
  # cmsearch against that seq, with -Z, --toponly
  my $tblout     = "single.$$.tblout";
  my $searchout  = "single.$$.cmsearch";
  my $searchopts = " --nohmmonly --rfam --toponly --cpu 0 -Z $Z"; 
  my $errout     = "single.$$.err";
  Bio::Rfam::Infernal::cmsearch_wrapper($config, "", "--tblout $tblout " . $searchopts, "CM", $tmpfile, $searchout, $errout, "", "", 1);

  # output cmsearch alignment
  open(SEARCH, $searchout) || die "ERROR unable to open $searchout";
  my $line = "";
  my $nprinted = 0;
  while($line = <SEARCH>) { 
    if($line =~ m/^Hit alignments\:/) { last; }
  }
  while($line = <SEARCH>) { 
    if($line =~ m/Internal CM pipeline/) { last; }
    else { 
      $line =~ s/^(\s+)SEED/$1  CM/;  # replace SEED with CM
      $nprinted++;
      Bio::Rfam::Utils::printToFileAndOrStdout($logFH, $prefix . $line, 1);
    }
  }
  close(SEARCH);

  if($nprinted == 0) { die "ERROR: single sequence search of $sqname failed to find any hits, but larger search of rfamseq found it..."; };

  foreach my $file ($tmpfile, $tblout, $searchout, $errout) { 
    if(-e $file) { unlink $file; }
  }
  return $nprinted;
}  
#########################################################
# redo_cmalign_alignment_of_subset
#
# Purpose: Fetch and align seqs.
#
# Args: 
# $config:      the config, with path to infernal executables
# $fafile:      fasta file to fetch seqs from and align
# $out_stkfile: output stk file from cmalign to create
# $out_cmafile: output cmalign output file from cmalign to create
# $nameAR:      ref to array of names of seqs to align
# $do_local:    '1' to align locally w.r.t. the CM, else align globally
#
# Returns: void
sub redo_cmalign_alignment_of_subset {
  my ($config, $fafile, $cmfile, $out_stkfile, $out_cmafile, $nameAR, $do_local) = @_;

  my $nprinted = 0;  

  # fetch fasta seqs to a single seq fasta file
  my $tmp_fafile = "tmp.$$.fa";
  Bio::Rfam::Utils::fetch_from_sqfile_wrapper($fafile, $nameAR, 0, undef, undef, 0, $tmp_fafile); # undef: default line length, undef, 0 means don't update log file

  # don't use cmalign_wrapper, we'll do this always locally and always with 1 CPU
  my $local_opt = ($do_local) ? "" : "-g";
  Bio::Rfam::Utils::run_local_command($config->infernalPath . "cmalign -o $out_stkfile --cpu 0 --noprob $local_opt $cmfile $tmp_fafile > $out_cmafile"); 

  unlink $tmp_fafile;

  return;
}  
#########################################################
# remove_rfreplace_prefix_from_sqname
#
# Purpose: Remove either 'seed-' or 'hit-' from beginning 
# of a sequence name and return it.
#
# Args: 
# $sqname:      sequence name to remove prefix from
# $type:        'seed' or 'hit' or 'either'
#
# Returns: $sqname with 'seed-' or 'hit-' removed.
sub remove_rfreplace_prefix_from_sqname { 
  my ($sqname, $type) = @_;

  if($type ne "seed" && $type ne "hit" && $type ne "either") { 
    die "ERROR in remove_rfreplace_prefix_from_space(), type is $type, but should be 'seed' or 'hit' or 'either'";
  }
  if($type eq "seed") { 
    if($sqname !~ m/^seed\-/) { die "ERROR trying to remove seed- prefix from $sqname, but it doesn't have one"; }
    $sqname =~ s/^seed-//;
  }
  elsif($type eq "hit") { 
    if($sqname !~ m/^hit\-/) { die "ERROR trying to remove hit- prefix from $sqname, but it doesn't have one"; }
    $sqname =~ s/^hit-//;
  }
  elsif($type eq "either") { 
    if(($sqname !~ m/^hit\-/) && ($sqname !~ m/^seed\-/)) { 
      die "ERROR trying to remove seed- or -hit- prefix from $sqname, but it doesn't have one"; 
    }
    if($sqname =~ m/^hit\-/)  { $sqname =~ s/^hit-//; }
    if($sqname =~ m/^seed\-/) { $sqname =~ s/^seed-//; }
  }

  return $sqname;
}

################################################
sub id2percent {
  return int (($_[0] * 100) + .5);
}
################################################
# reorder_array($AR, $n, $orderAR)
# Reorder the $n values in $AR so they're in the
# order specified in $orderAR. $orderAR should be 
# of size $n, and all of its values must be 
# '0..$n-1', and if you don't want duplicates,
# all values 0..n-1 should occur exactly once.

sub reorder_array {
  my ($AR, $n, $orderAR) = @_;

  my @tmpA = ();
  my $i;
  for($i = 0; $i < $n; $i++) { 
    $tmpA[$i] = $AR->[$orderAR->[$i]];
  }
  for($i = 0; $i < $n; $i++) { 
    $AR->[$i] = $tmpA[$i];
  }

  return;
}
################################################
# quit_early
# User decided to abort. Remove all intermediate
# files and exit. Careful: all intermediate files
# are hard-coded here, so we have to make sure
# we update this subroutine if we make changes to
# the set of intermediate files we expect to
# have lying around.
# We DO NOT get rid of the rfreplace.log file.
# User may want that...
sub quit_early {
  my ($logFH) = @_;

  Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, 1);

  my $description;
  if (-e "rfreplace.log.$$") { 
    $description = "old rfreplace.log file, copy of 'rfreplace.log' from before this script was run";
    Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfreplace.log.$$", $description, 1);
  }

  $description = sprintf("log file (*this* output)");
  Bio::Rfam::Utils::log_output_file_summary($logFH,   "rfreplace.log", $description, 1);

  # now unlink files 
  foreach my $file ("$$.fa", "$$.stk") { 
    #unlink $file;
  }

  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("#\n"), 1);
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# Total time elapsed: %s\n", Bio::Rfam::Utils::format_time_string(time() - $start_time)), 1);
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf("# [ok]\n"), 1);

  close($logFH);

  exit 0;

  return; # not reached
}
################################################

sub help {
  my ($exec_description) = (@_);
  print STDERR <<EOF;
  
  rfreplace.pl - $exec_description

Usage:      rfreplace.pl [options]

Options:    -1 <f>       : set minimum fractional id for definition of a match to <f> [df: $df_id1_thr]
            -2 <f>       : set maximum fractional id b/t candidate matches to same SEED seq to <f> [df: $df_id2_thr]
            -x <n>       : set maximum number of matches to collect to <n> [df: $df_maxcands]
            -b <f>       : set minimum hit score to search for matches to lowest SEED seq score - <n> [df: $df_bitdiff]
            -p           : include posterior probabilities in new SEED [default: do not]
            -alocal      : align locally w.r.t. the CM [default: globally]
            -inseed <f>  : seed alignment is in <f>, not 'SEED'
            -outseed <f> : save new seed alignment as <f>, not 'SEED'
            -h|-help     : print this help, then exit

EOF
}
