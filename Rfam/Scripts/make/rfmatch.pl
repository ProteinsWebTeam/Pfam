#!/usr/bin/env perl 

# rfmatch.pl - compare hits in an outlist to a FASTA flatfile, and 
#              look for nearly identical matches
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
my $dbchoice   = "rfamseq";         # We'll always use this database.

# set default values that command line options may change
my $do_stdout     = 1;              # TRUE to output to STDOUT
my $do_quiet      = 0;              # TRUE to not output anything to STDOUT
my $df_id1_thr    = 0.90;           # default value for $id1_thr (see next line) changeable to <f> with -1 <f>
my $id1_thr       = $df_id1_thr;    # minimum fractional identity for defining a match
# options for selecting cutoff for which hits to look for matches to
my $do_all        = 1;              # TRUE to consider all hits in outlist
my $e_thr         = undef;          # defined with -e
my $t_thr         = undef;          # defined with -t
my $b_thr         = undef;          # defined with -b
my $df_maxhits    = 25000;          # default for $maxhits, see next line
my $maxhits       = $df_maxhits;    # if number of hits exceeds this, we'll die in error
# other options
my $df_in_prefix  = "";             # default value for $in_prefix (see next line) changeable to <s> with -in <s>
my $in_prefix     = $df_in_prefix;  # prefix for input  files, $in_prefix  . "outlist" and $in_prefix . "species" will be read in
my $do_local      = 0;              # TRUE to align locally w.r.t. the CM
my $do_force      = 0;              # TRUE with -f, overwrite files 
my $do_help       = 0;              # TRUE to print help and exit, if -h used

my $logFH;

my $config = Bio::Rfam::Config->new;

&GetOptions( "1=s"        => \$id1_thr,
             "e=s"        => \$e_thr,
             "t=s"        => \$t_thr,
             "b=s"        => \$b_thr,
             "x=s"        => \$maxhits,
             "alocal"     => \$do_local,
             "in=s"       => \$in_prefix,
             "f"          => \$do_force,
             "quiet"      => \$do_quiet,
             "h|help"     => \$do_help);

$do_stdout = ($do_quiet) ? 0 : 1;

# read in command line variables
my $infile     = "";
my $out_prefix = "";
my $exec_description = "find near identical matches to 'outlist' hits in a sequence file";
if(scalar(@ARGV) != 2) { 
  $do_help = 1; 
}
else { 
  ($infile, $out_prefix) = @ARGV;
}
$out_prefix =~ s/\.+$//;

if ( $do_help ) {
  &help($exec_description);
  exit(1);
}

# copy rfmatch.$out_prefix.log sideways if it exists
my $logfile = "rfmatch.$out_prefix.log";
$logfile =~ s/\.\.log/\.log/;
if (-e $logfile) { 
  if(! $do_force) { 
    die "ERROR, $logfile already exists, to force overwrite use -f"; 
  }
}

open($logFH, ">" . $logfile) || die "ERROR unable to open $logfile for writing";
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
my $outlist   = $in_prefix . "outlist";
my $species   = $in_prefix . "species";
if($in_prefix eq "") { 
  if(! -s $outlist) { die "ERROR $outlist does not exist, did you run rfsearch.pl and rfmake.pl?"; }
  if(! -s $species) { die "ERROR $species does not exist, did you run rfsearch.pl and rfmake.pl?"; }
}
else { 
  if(! -s $outlist) { die "ERROR $outlist does not exist"; }
  if(! -s $species) { die "ERROR $species does not exist"; }
}

# determine score threshold
my $minsc = undef; # if this stays undef, we'll use all hits in outlist
if   (defined $e_thr) { $do_all = 0; $minsc = Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $e_thr, $Z, $desc->{'SM'}); }
elsif(defined $t_thr) { $do_all = 0; $minsc = $t_thr; }
elsif(defined $b_thr) { $do_all = 0; $minsc = $desc->CUTGA - $b_thr; }

# check for incompatible option combos:
if($in_prefix eq $out_prefix) { 
  die "ERROR, with -in <s>, <s> cannot be the same as the out prefix $out_prefix";
}

# output preamble: user, date, location, etc.
my $cwidth = 70;
Bio::Rfam::Utils::log_output_preamble($logFH, $cwidth, $user, $config, $desc, $do_stdout);

# This block should stay consistent with 
# the GetOptions() call above, and with the help()
# subroutine.
                                    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# input flat file to search for matches in: ",            "$infile"),            $do_stdout); 
                                    Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# output prefix, for naming output files: ",              "$out_prefix"),        $do_stdout); 
if($id1_thr    ne $df_id1_thr)    { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# min fractional identity for a replacement candidate: ", "$id1_thr [-1]"),      $do_stdout); }
if($do_all)                       { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# search for matches to all hits: ",                      "yes [default]"),      $do_stdout); }
if(defined $e_thr)                { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# search for matches to hits with max E value of: ",      "$e_thr [-e]"),        $do_stdout); }
if(defined $t_thr)                { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# search for matches to hits with min bit score of: ",    "$t_thr [-t]"),        $do_stdout); }
if(defined $b_thr)                { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# consider hits <f> bits less than lowest scoring SEED: ","<f>=$b_thr [-b]"),    $do_stdout); }
if($do_local)                     { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# align new sequences locally w.r.t. CM: ",               "yes [-alocal]"),      $do_stdout); }
if($in_prefix  ne $df_in_prefix)  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# prefix for input 'outlist' and 'species' files: ",      "$in_prefix [-in]"),   $do_stdout); }
if($do_quiet)                     { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# quiet mode: ",                                          "on  [-quiet]"),       $do_stdout); }

Bio::Rfam::Utils::log_output_divider($logFH, $do_stdout, 100);

                                                  
# create hash of potential output files
my %outfileH = ();
my $ooutlist = $out_prefix . ".outlist";
my $ospecies = $out_prefix . ".species";
my @outfile_orderA = ($ooutlist, $ospecies, $logfile);
$outfileH{$ooutlist} = "$outlist plus markup of matches in <infile>";
$outfileH{$ospecies} = "$species plus markup of matches in <infile>";
$outfileH{$logfile}  = "log file (*this* output)";

#############################################
Bio::Rfam::Utils::log_output_progress_column_headings($logFH, sprintf("per-stage progress:"), 1);

# Search all $infile seqs to get there scores and so we know which ones to align
my $search_start_time = time();
my $searchopts = Bio::Rfam::FamilyIO::cmsearchReadySearchopts($desc, "0");
$searchopts .= " --rfam --toponly";
#$searchopts .= " --mid --toponly";
my $tblout    = "s.$$.tbl";
my $searchout = "s.$$.cmsearch";
Bio::Rfam::Utils::log_output_progress_local($logFH, "cmsearch", time() - $search_start_time, 1, 0, "[$infile]", 1);
Bio::Rfam::Infernal::cmsearch_wrapper($config, "", "--tblout $tblout " . $searchopts, "CM", $infile, $searchout, "s.$$.err", "", "", 1);
Bio::Rfam::Utils::log_output_progress_local($logFH, "cmsearch", time() - $search_start_time, 0, 1, "", 1);

# get string of all hits found in $infile and all hits above $minsc, we do this in two steps so we can exit if we have more than $maxhits hits
my @match_nseA = ();
my @hit_nseA   = ();
Bio::Rfam::FamilyIO::nseArrayFromOutlistOrTblout($tblout,  "tblout",  "",     \@match_nseA);
Bio::Rfam::FamilyIO::nseArrayFromOutlistOrTblout($outlist, "outlist", $minsc, \@hit_nseA);
my $nalign = scalar(@match_nseA) + scalar(@hit_nseA);
if($nalign > $maxhits) { 
  die "ERROR, $nalign > $maxhits hits to align exist, increase maximum with -x, or consider lowering threshold with -e, -t or -b."; 
}
my ($nmatch, $nmatch_res, $match_seqstring) = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@match_nseA, $infile,    undef, "", $logFH, $do_stdout); # undef: default line len, "" means return a string of all seqs
my ($nhit,   $nhit_res,   $hit_seqstring)   = Bio::Rfam::Utils::fetchSubseqsGivenNseArray(\@hit_nseA,   $fetchfile, undef, "", $logFH, $do_stdout); # undef: default line len, "" means return a string of all seqsli

# output all seqs to a single file
my $fa_file  = "$$.fa";
open(OUT, ">" . $fa_file) || die "ERROR unable to open $fa_file for writing seqs to";
print OUT $match_seqstring;
print OUT $hit_seqstring;
close(OUT);

# align all seqs
my $cmalign_file = "$$.cmalign";
my $stk_file     = "$$.stk";
my $err_file     = "a.$$.err";
my $align_opts = "-o $stk_file --noprob"; # PPs will just increase size of file -- we won't use them
if(! $do_local) { $align_opts .= " -g"; }
Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $align_opts, "CM", $fa_file, $cmalign_file, "a.$$.err", $nhit + $nmatch, $nhit_res + $nmatch_res, 1, 0, "", -1, $logFH, 1);
foreach my $file ($cmalign_file, $err_file) { 
  if(-s $file) { 
    unlink $file; 
  }
}

# read in MSA we just created
my $msa = Bio::Easel::MSA->new({
   fileLocation => "$stk_file",
});
my $prefix_added = $msa->check_if_prefix_added_to_sqnames;

# look for matches
my $nall = $nmatch + $nhit;
my @matchnameA = ();
my @matchpcntA = ();
for(my $i = $nmatch; $i < $nall; $i++) { 
  my $maxid = 0.;
  my $match = "";
  for(my $j = 0; $j < $nmatch; $j++) { 
    my $pid = $msa->pairwise_identity($i, $j);
    if($pid > $id1_thr && $pid > $maxid) { 
      $maxid = $pid;
      $match = $j;
    }
  }
  if($match ne "") { # we found a match to $i
    my $matchname = $msa->get_sqname($match);
    if($prefix_added) { 
      $matchname = $msa->remove_prefix_from_sqname($matchname);
    }
    push(@matchnameA, $matchname);
    push(@matchpcntA, Bio::Rfam::Utils::percentize($maxid));
  }
  else { 
    push(@matchnameA, "-");
    push(@matchpcntA, "-");
  }
}

# output new outlist and species files
open(INO, $outlist) || die "ERROR unable to open $outlist for reading";
open(INS, $species) || die "ERROR unable to open $species for reading";
my $outoFH;
my $outsFH;
open($outoFH, ">" . $ooutlist) || die "ERROR unable to open $ooutlist for writing";
open($outsFH, ">" . $ospecies) || die "ERROR unable to open $ospecies for writing";
my $line_cnt = 0;
my ($i, $ip, $init_ip) = (0, 0, 0);
my $min_namew = length("# match-seqname:bits:Evalue");
my $namew = $min_namew;
my @outoA = (); # all outlist lines in current block
my @outsA = (); # all species lines in current block
# we read in outlist/species and output matchoutlist/matchspecies in blocks
# we only have to do this so we get width of matchname column correct (we have
# to look at all matchnames in a block before we know the max width).
while($i < $nhit) { 
  my $oline = <INO>;
  my $sline = <INS>;

  # check if we need to output block
  if($oline =~ m/^\#\s*$/ && $line_cnt > 0) { 
    my $ip = output_block($line_cnt, $outoFH, $outsFH, $namew, \@outoA, \@outsA, $init_ip, \@matchnameA, \@matchpcntA);
    if($i != $ip) { die "ERROR, didn't output internal block correctly; bug in code."; }
    # reinitialize for next block
    $init_ip = $ip;      # we want to remember this, for the next block
    $namew = $min_namew; # reset max length name for next block
    $line_cnt = 0;       # reset line_cnt for next block
    @outoA = ();         # clear this for next block
    @outsA = ();         # clear this for next block
  }

  # save $oline and $sline
  push(@outoA, $oline);
  push(@outsA, $sline);
  if($oline !~ m/^\#/) { # check if we have a new max length matchname:
    if($i < $nhit) { 
      my $len = length($matchnameA[$i]);
      $namew = ($len > $namew) ? $len : $namew;
      $i++; # the critical i increment, we'll break out of the enclosing while when $i hits $nhit
    }
  }
  $line_cnt++; # the critical line_cnt increment
}
if($i > $init_ip) { 
  my $ip = output_block($line_cnt, $outoFH, $outsFH, $namew, \@outoA, \@outsA, $init_ip, \@matchnameA, \@matchpcntA);
  if($i != $ip) { die "ERROR, didn't output final block correctly; bug in code."; }
}  
close(INO);
close(INS);
close($outoFH);
close($outsFH);

###############################################
# finished all work, print output file summary
###############################################
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, $do_stdout);
# output brief descriptions of the files we just created, we know that if these files exist that 
# we just created them, because we deleted them at the beginning of the script if they existed
foreach my $outfile (@outfile_orderA) { 
  if(-e $outfile) { 
    Bio::Rfam::Utils::log_output_file_summary($logFH, $outfile, $outfileH{$outfile}, $do_stdout);
 }
}

Bio::Rfam::Utils::log_output_tail($logFH, $start_time, $do_stdout);

close($logFH);
exit 0;

########################################
sub output_block {
  my ($line_cnt, $outoFH, $outsFH, $namew, $outoAR, $outsAR, $init_ip, $matchnameAR, $matchpcntAR) = @_;

  my $ip = $init_ip;

  # first 3 lines should match what we expect at the beginning of a block:
  #
  # bits  evalue   seqLabel  name             overlap  start   end     str  qstart  qend  trunc  species                            description
  #=====  =======  ========  ===============  =======  ======  ======  ===  ======  ====  =====  =================================  ================================================================================================================================================================================================================================================

  my $j = 0;
  if($outoAR->[$j] !~ m/^\#\s*$/) { die "ERROR unexpected beginning to outlist block, line 1..."; }
  if($outsAR->[$j] !~ m/^\#\s*$/) { die "ERROR unexpected beginning to species block, line 1..."; }
  printf $outoFH $outoAR->[$j];
  printf $outsFH $outsAR->[$j];

  $j = 1;
  if($outoAR->[$j] !~ m/^\# bits/) { die "ERROR unexpected beginning to outlist block, line 2..."; }
  if($outsAR->[$j] !~ m/^\# bits/) { die "ERROR unexpected beginning to species block, line 2..."; }
  $outoAR->[$j] =~ s/^\#/ /; # remove leading '#'
  $outsAR->[$j] =~ s/^\#/ /; # remove leading '#'
  printf $outoFH ("%-*s  %3s  $outoAR->[$j]", $namew, "# match-seqname", "%id");
  printf $outsFH ("%-*s  %3s  $outsAR->[$j]", $namew, "# match-seqname", "%id");

  $j = 2;
  if($outoAR->[$j] !~ m/^\#==/) { die "ERROR unexpected beginning to outlist block, line 3..."; }
  if($outsAR->[$j] !~ m/^\#==/) { die "ERROR unexpected beginning to species block, line 3..."; }
  $outoAR->[$j] =~ s/^\#/ /; # remove leading '#'
  $outsAR->[$j] =~ s/^\#/ /; # remove leading '#'
  my $tmpline =  Bio::Rfam::Utils::monocharacterString("=", $namew-1);
  printf $outoFH ("#%-*s  %3s  $outoAR->[$j]", $namew-1,  $tmpline, "===");
  printf $outsFH ("#%-*s  %3s  $outsAR->[$j]", $namew-1,  $tmpline, "===");

  for($j = 3; $j < $line_cnt; $j++) { 
    if($outoAR->[$j] !~ m/^\#/) { 
      printf $outoFH ("%-*s  %3s  $outoAR->[$j]", $namew, $matchnameAR->[$ip], $matchpcntAR->[$ip]);
      printf $outsFH ("%-*s  %3s  $outsAR->[$j]", $namew, $matchnameAR->[$ip], $matchpcntAR->[$ip]);
      $ip++;
    }
    else { # comment line
      printf $outoFH $outoAR->[$j];
      printf $outsFH $outsAR->[$j];
    }
  }
  return $ip;
}

########################################

sub help {
  print STDERR <<EOF;
  
  rfmatch.pl - $exec_description

Usage:      rfmatch.pl [options]
                       <fasta seq file to search for high identity matches to hits in outlist>
                       <prefix for output files, e.g. 'mirbase' => 'mirbase.outlist' will be created>

Options:    -1 <f>       : set minimum fractional id for definition of a match to <f> [df: $df_id1_thr]
            -e <f>       : set maximum hit E-value to search for matches to <f> [df: do all hits]
            -t <f>       : set minimum hit bit score to search for matches to <f> [df: do all hits]
            -b <f>       : set minimum hit score to search for matches to GA - <f> bits [df: do all hits]
            -x <n>       : set maximum number of hits to consider as <n>, die if exceeded [df: $df_maxhits]
            -alocal      : align locally w.r.t. the CM [default: globally]
            -in <s>      : input 'outlist' and 'species' files are prefixed with <s> [df: $df_in_prefix]
            -f           : force; files may be overwritten
            -quiet       : be quiet; do not output anything to stdout (rfmatch.log still created)
            -h|-help     : print this help, then exit

EOF
}
