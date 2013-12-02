#!/usr/bin/env perl 

# rfseed-extend.pl - Extend or trim a seed alignment 5' or 3'.

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
#use Bio::Rfam::Interactive;
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
my $dbchoice   = "rfamseq";  # We'll always use this database.
my $do_stdout  = 1;

my $do_trim = 0; # '1' to trim alignment instead of extend, '0' to extend instead of trim
my $n5 = undef;  # number of residues to extend/trim in 5' (left  hand) direction
my $n3 = undef;  # number of residues to extend/trim in 3' (right hand) direction
# other options
my $do_list  = 0;       # '1' to read in list file with subset of seqs to extend
my $listfile = undef;   # set to a name by GetOptions() if -l used
my $inseed   = "SEED";  # input seed alignment file, 'SEED' by default changeable with -i <s>
my $outseed  = "SEED";  # output seed alignment file, 'SEED' by default changeable with -o <s>
my $do_help  = 0;       # TRUE to print help and exit, if -h used

my @unlinkA = ();      # array of files to unlink before exiting.

my $date = scalar localtime();
my $logFH;

my $config = Bio::Rfam::Config->new;
my $exec_description = "extend or trim a seed alignment 5' and/or 3'";

my $options_okay = &GetOptions( "5=s"        => \$n5,
                                "3=s"        => \$n3,
                                "l=s"        => \$listfile, 
                                "t"          => \$do_trim,
                                "i=s"        => \$inseed,
                                "o=s"        => \$outseed,
                                "h|help"     => \$do_help);
if(! $options_okay) { 
  &help($exec_description); 
  die "ERROR, unrecognized option; "; 
}

# check that at least one of -5 or -3 was used
if(! defined $n5 && ! defined $n3) { die "ERROR, at least one of -5 or -3 must be used."; }
if(defined $n5 && $n5 !~ m/\d+/)   { die "ERROR, with -5 <n>, <n> must be positive integer."; }
if(defined $n3 && $n3 !~ m/\d+/)   { die "ERROR, with -3 <n>, <n> must be positive integer."; }
# handle -l
if(defined $listfile) { $do_list = 1; }
if($do_list && $do_trim) { die "ERROR, -l and -t are incompatible, choose one."; }

# copy rfseed-extend.log sideways if it exists
my $logfile = "rfseed-extend.log";
if (-e $logfile) { copy($logfile, $logfile . ".$$"); }

open($logFH, ">" . $logfile) || die "ERROR unable to open $logfile for writing";
my $dwidth = 100;
Bio::Rfam::Utils::log_output_rfam_banner($logFH, $executable, $exec_description, 1, $dwidth);

# read in command line variables
if(scalar(@ARGV) != 0) { $do_help = 1; }
if ($do_help) { &help(); exit(1); }

# get user
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
                                    'DESC'   => $io->parseDESC("DESC"),
                                   );
my $oseedmsa = $famObj->SEED;
my $desc     = $famObj->DESC;
my $id       = $desc->ID;
my $acc      = $desc->AC;
# setup dbfile 
my $dbconfig   = $config->seqdbConfig($dbchoice);
my $Z          = $dbconfig->{"dbSize"};
my $fetchfile  = $dbconfig->{"fetchPath"};

if(! defined $n5) { $n5 = 0; }
if(! defined $n3) { $n3 = 0; }
my $allgap5 = Bio::Rfam::Utils::monocharacterString(".", $n5);
my $allgap3 = Bio::Rfam::Utils::monocharacterString(".", $n3);

# by default we list user, date, pwd, family, etc.
# and information for any command line flags set by
# the user. This block should stay consistent with 
# the GetOptions() call above, and with the help()
# subroutine.
my $cwidth = 70;
Bio::Rfam::Utils::log_output_preamble($logFH, $cwidth, $user, $config, $desc, 1);

if($do_trim)          { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# trimming seed alignment, rather than extending: ",            "yes [-t]"),      1); }
else                  { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# extending seed alignment, rather than trimming: ",            "yes [default]"), 1); }
if($inseed ne "SEED") { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# input alignment in file: ",                                   "$inseed [-i]"),  1); }
if(defined $n5)       { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# number of residues/columns to extend/trim in 5' direction: ", "$n5 [-5]"),      1); }
if(defined $n3)       { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# number of residues/columns to extend/trim in 3' direction: ", "$n3 [-3]"),      1); }
if($do_list)          { Bio::Rfam::Utils::printToFileAndOrStdout($logFH, sprintf ("%-*s%s\n", $cwidth, "# read subset of sequence names to extend from: ",              "$listfile [-l]"),1); }
Bio::Rfam::Utils::log_output_divider($logFH, 1, $dwidth);

my $oalen = $oseedmsa->alen;
my $nseedmsa = undef; # will become new MSA
if(defined $n5 && $n5 >= $oalen) { die "ERROR, with -5 <n>, <n> cannot exceed alignment length ($oalen)"; }
if(defined $n3 && $n3 >= $oalen) { die "ERROR, with -3 <n>, <n> cannot exceed alignment length ($oalen)"; }
my $echoed_rf = 0; # set to '1' if seed had GC RF annotation, in which case we'll warn user at exit

# create skiopmeH hash that we'll use if we're extending, skipmeH{$sqname} = 1 means do not 'extend', else do extend
my %skipmeH = (); # if (skipmeH{$name} == 1), then do not extend sequence $name, else do
my $sqname;
my $i;
if($do_list) { # read in the list file and store a hash of 
  # first set skipemeH to '1' for all seqs, then go back and delete those listed in list file
  for($i = 0; $i < $oseedmsa->nseq; $i++) { 
    $skipmeH{$oseedmsa->get_sqname($i)} = 1;
  }
  open(LIST, $listfile) || die "ERROR unable to open $listfile";
  while($sqname = <LIST>) { 
    chomp $sqname;
    $sqname =~ s/\s+$//; # remove trailing whitespace if any
    if($sqname =~ m/^\w/) { 
      if($sqname =~ m/\s+/) { die "ERROR, in $listfile, found line with whitespace, each line should be a sequence name, no whitespace:\n$sqname"; }
      delete $skipmeH{$sqname};
    }
  }
}

if($do_trim) { # trim mode, the easy case, just remove the requested columns 
  # we'll keep columns spos..epos where [0..spos..epos..alen-1]
  my $spos = 0;
  my $epos = $oalen-1;
  if(defined $n5) { $spos += $n5; }
  if(defined $n3) { $epos -= $n3; }
  if($spos > $epos) { die "ERROR, spos > epos ($spos > $epos)..."; }
  # create usemeA array
  my @usemeA = ();
  my $apos;
  for($apos = 0; $apos <  $spos;  $apos++) { $usemeA[$apos] = 0; }
  for(         ; $apos <= $epos;  $apos++) { $usemeA[$apos] = 1; }
  for(         ; $apos <  $oalen; $apos++) { $usemeA[$apos] = 0; }
  # copy MSA then remove unwanted columns 
  $nseedmsa = $oseedmsa->clone_msa();
  $nseedmsa->column_subset(\@usemeA);
}
else { # not trim mode, adding columns
  # we want to not modify the original seed, so we take extreme measures
  # and roll our own method for creating the bastardized alignment:
  # 1) output original seed in pfam format
  # 2) read in pfam seed, when we reach a sequence line, prepend additional 
  #    (extended) 5' sequence if any, and append 3' additional (extended) 
  #    sequence if any to the string, and keep track of new alignment
  #    as a string we'll convert to an MSA at the end.
  # 3) for per column and per residue sequence annotation, extend 5' and 3'
  #    by using all gaps.
  my $fetch_sqfile = Bio::Easel::SqFile->new({
    fileLocation => $fetchfile,
  });

  # output the SEED in pfam format so we can read it in and regurgitate most of it
  my $tmpseed = "$$.stk";
  my $newseed_str = "";
  my $sqidx = 0;    # index of current seq in $oseedmsa Bio::Easel::MSA object
  $oseedmsa->write_msa($tmpseed, "pfam");
  push(@unlinkA, $tmpseed);
  open(INSEED, $tmpseed) || die "ERROR unable to open $tmpseed for reading";
  my $line = <INSEED>;
  my $linectr = 1;
  while($line =~ m/^\#/ || $line !~ m/\w/) { 
    $newseed_str .= $line;
    if(! ($line = <INSEED>)) { die "ERROR, ran out of lines early when reading $tmpseed."; };
    $linectr++;
  }
  # at this point we're at the first sequence line of the pfam formatted $tmpseed file
  while($line) { 
    if($line =~ /\/\/\n/) { 
      # the final line ('//')
      $newseed_str .= $line; 
    }
    elsif($line =~ /(^\#=GR)\s+(\S+)\s+(\S+)\s+(\S+)$/) { 
      # per residue annotation
      $newseed_str .= $1 . " " . $2 . " " . $3 . " " . $allgap5 . $4 . $allgap3 . "\n";
    }
    elsif($line =~ /(^\#=GC)\s+(\S+)\s+(\S+)$/) { 
      # per column annotation
      if($2 eq "RF") { $echoed_rf = 1; }
      $newseed_str .= $1 . " " . $2 . " " . $allgap5 . $3 . $allgap3 . "\n";
    }    
    elsif($line =~ /^(\S+)\s+(\S+)\n$/) { 
      # sequence line
      my ($oname, $oseq) = ($1, $2);
      # validate sequence
      my $nse =$oseedmsa->get_sqname($sqidx++);
      my ($validated, $name, $ostart, $oend, $strand) = Bio::Rfam::Utils::nse_breakdown($nse);
      if(! $validated)    { die "ERROR, seed sequence $sqidx ($nse) not in expected name/start-end format"; }
      if($oname ne $nse) { die "ERROR, inconsistent name $oname ne $nse (coding bug)"; }
      my ($fetch_start, $fetch_end, $nres);
      my $n5subseq = "";
      my $n3subseq = "";
      my $do_revcomp = 0;
      if($n5 > 0) { 
        if($strand == 1) { # top strand 
          if   (exists $skipmeH{$nse}) { $nres = 0;   }         # skipping this seq, don't fetch any extra res
          elsif($n5 < $ostart)         { $nres = $n5; }         # can fetch $n5 residues without running out
          else                         { $nres = $ostart - 1; } # can only fetch $ostart-1 residues before running out of sequence (nres be 0 if $ostart==1)
          $fetch_start = $ostart - $nres;
          $fetch_end   = $ostart - 1;
          $do_revcomp  = 0;
        }
        else { # bottom strand
          my $sqlen = $fetch_sqfile->fetch_seq_length_given_name($name);
          if   (exists $skipmeH{$nse})         { $nres = 0;   }              # skipping this seq, don't fetch any extra res
          elsif(($ostart + $n5 - 1) <= $sqlen) { $nres = $n5; }              # can fetch $n5 residues without running out
          else                                 { $nres = $sqlen - $ostart; } # can only fetch $sqlen - $ostart residues before running out of sequence (nres be 0 if $ostart==$sqlen)
          $fetch_start = $ostart + $nres;
          $fetch_end   = $ostart + 1;
          $do_revcomp  = 1;
        }
        $n5subseq = get_subseq($name, $fetch_sqfile, $fetch_start, $fetch_end, $nres, $n5, 1, $do_revcomp);
      }
      if($n3 > 0) { 
        if($strand == 1) { # top strand 
          # this block mirrors bottom strand 5' block, but with $n3 and $oend replacing $n5 and $ostart
          my $sqlen = $fetch_sqfile->fetch_seq_length_given_name($name);
          if($skipmeH{$nse})                 { $nres = 0; }              # skipping this seq, don't fetch any extra res
          elsif(($oend + $n3 - 1) <= $sqlen) { $nres = $n3; }            # can fetch $n3 residues without running out
          else                               { $nres = $sqlen - $oend; } # can only fetch $sqlen - $oend residues before running out of sequence (nres be 0 if $oend==$sqlen)
          $fetch_start = $oend + 1;
          $fetch_end   = $oend + $nres;
          $do_revcomp  = 0;
        }
        else { # bottom strand
          # this block mirrors top strand 5' block, but with $n3 and $oend replacing $n5 and $ostart
          if   (exists $skipmeH{$nse}) { $nres = 0; }         # skipping this seq, don't fetch any extra res
          elsif($n3 < $oend)           { $nres = $n3; }       # can fetch $n3 residues without running out
          else                         { $nres = $oend - 1; } # can only fetch $oend-1 residues before running out of sequence (nres be 0 if $oend==1)
          $fetch_start = $oend - 1;
          $fetch_end   = $oend - $nres;
          $do_revcomp  = 1;
        }
        $n3subseq = get_subseq($name, $fetch_sqfile, $fetch_start, $fetch_end, $nres, $n3, 0, $do_revcomp);
      }
      $newseed_str .= $oname . " " . $n5subseq . $oseq . $n3subseq . "\n";
    } # end of sequence line block
    else { # unrecognized line, exit
      die "ERROR did not recognize line $linectr: $line"; 
    }
    $line = <INSEED>;
    $linectr++;
  } # end of while($line)
  # now $newseed_str is the full MSA, create a Bio::Easel::MSA object from it
  unlink $tmpseed;
  # printf("$newseed_str");
  $nseedmsa = Bio::Easel::MSA::create_from_string($newseed_str, "stockholm", "rna", 1);
}

# create new SEED
# copy $inseed first if nec
if($inseed eq $outseed) { 
  if (-e $inseed) { copy($inseed, $inseed . ".$$"); }
}
# update names in new SEED
update_names($nseedmsa, $oseedmsa, $do_trim, $n5, $n3, \%skipmeH);
if($do_trim) { $oseedmsa->write_msa($outseed, "stockholm"); }
else         { $nseedmsa->write_msa($outseed, "stockholm"); }

# done all work, print output file summary
my $fwidth = 25;
$dwidth = 81;
Bio::Rfam::Utils::log_output_file_summary_column_headings($logFH, 1, $fwidth, $dwidth);
# output brief descriptions of the files we just created, first update the
# arrays we defined earlier with new files that we've now created.
if(-e "$outseed.$$") {                               
  Bio::Rfam::Utils::log_output_file_summary($logFH, "$outseed.$$", "old seed alignment, copy of '$inseed' from before this script was run", $do_stdout, $fwidth, $dwidth);
}
my $seed_desc = "";
if($do_trim) { 
  if   ($n5 > 0 && $n3 > 0) { $seed_desc = "new seed alignment, same as '$inseed' but with $n5 column(s) trimmed 5' and $n3 column(s) trimmed 3'"; }
  elsif($n5 > 0)            { $seed_desc = "new seed alignment, same as '$inseed' but with $n5 column(s) trimmed 5'"; }
  elsif($n3 > 0)            { $seed_desc = "new seed alignment, same as '$inseed' but with $n3 column(s) trimmed 3'"; }
}
else { 
  if   ($n5 > 0 && $n3 > 0) { $seed_desc = "new seed alignment, same as '$inseed' but extended $n5 column(s) 5' and $n3 column(s) 3'"; }
  elsif($n5 > 0)            { $seed_desc = "new seed alignment, same as '$inseed' but extended $n5 column(s) 5'"; }
  elsif($n3 > 0)            { $seed_desc = "new seed alignment, same as '$inseed' but extended $n3 column(s) 3'"; }
}
Bio::Rfam::Utils::log_output_file_summary($logFH, "$outseed", $seed_desc, $do_stdout, $fwidth, $dwidth);
if(-e "$logfile.$$") { 
  Bio::Rfam::Utils::log_output_file_summary($logFH, "$logfile.$$", "old log file, copy of '$logfile' from before this script was run", $do_stdout, $fwidth, $dwidth);  
}
Bio::Rfam::Utils::log_output_file_summary($logFH, $logfile, "log file (*this* output)", $do_stdout, $fwidth, $dwidth);
if($echoed_rf) { 
  Bio::Rfam::Utils::printToFileAndOrStdout($logFH, "#\n# ! WARNING, GC RF annotation in $outseed was extended by adding gap columns, you might want to manually edit this.\n", $do_stdout, $fwidth, $dwidth);
}
Bio::Rfam::Utils::log_output_tail($logFH, $start_time, 1);

close($logFH);
exit 0;


###############
# SUBROUTINES #
###############
sub get_subseq { 
  my ($name, $fetch_sqfile, $fetch_start, $fetch_end, $nres, $ntot, $gap_before, $do_revcomp) = @_;

  my $ngap = $ntot - $nres;
  my $gapstr = "";
  my $seqstr = "";

  if($ngap > 0) { $gapstr .= Bio::Rfam::Utils::monocharacterString(".", $ngap); }

  if($nres > 0) { 
    printf("in get_subseq() fetching $fetch_start..$fetch_end\n");
    $seqstr = $fetch_sqfile->fetch_subseq_to_fasta_string($name, $fetch_start, $fetch_end, -1, $do_revcomp);     
    # remove sequence name, description and newlines
    if($seqstr !~ s/^\>\S+\s*.*\n//) { die "ERROR, unable to process beginning of fetched sequence: $seqstr"; } 
    if($seqstr !~ s/\n$//)           { die "ERROR, unable to process end of fetched sequence: $seqstr"; } 
  }
  
  my $ret_str = ($gap_before) ? ($gapstr . $seqstr) : ($seqstr . $gapstr);

  return $ret_str;
}

#####################################

sub update_names { 
  my ($nseedmsa, $oseedmsa, $do_trim, $n5, $n3, $skipmeHR) = @_;

  my($i, $apos, $nstart, $nend);
  my $nalen = $nseedmsa->alen;
  for($i = 0; $i < $nseedmsa->nseq; $i++) { 
    my $onse =$oseedmsa->get_sqname($i);
    if(! $skipmeHR->{$onse}) { 
      my ($validated, $oname, $ostart, $oend, $strand) = Bio::Rfam::Utils::nse_breakdown($onse);
      my $nstart = $ostart;
      my $nend   = $oend;
      # we'll only enter this for loop if $n5 > 0
      for($apos = 1; $apos <= $n5; $apos++) { 
        if($nseedmsa->is_residue($i, $apos)) { 
          if($do_trim) { 
            if($strand eq "1") { $nstart++; }
            else               { $nstart--; }
          }
          else { 
            if($strand eq "1") { $nstart--; }
            else               { $nstart++; }
          }
        }
      }
      # we'll only enter this for loop if $n3 > 0
      for($apos = $nalen-$n3+1; $apos <= $nalen; $apos++) { 
        if($nseedmsa->is_residue($i, $apos)) { 
          if($do_trim) { 
            if($strand eq "1") { $nend--; }
            else               { $nend++; }
          }
          else { 
            if($strand eq "1") { $nend++; }
            else               { $nend--; }
          }
        }
      }
      my $nnse = $oname . "/" . $nstart . "-" . $nend;
      # printf("old s-e: $ostart-$oend     new s-e: $nstart-$nend\n");
      $nseedmsa->set_sqname($i, $nnse);
    } # end of 'if(! $skipmeHR->{$onse})'
  }

  return;
}
    
################################################
sub help {
  print STDERR <<EOF;
  
  rfseed-extend.pl - $exec_description

Usage:      rfseed-extend.pl [options]

Options:    -5 <n>  : extend each sequence <n> residues in 5 prime direction (left  hand side)
            -3 <n>  : extend each sequence <n> residues in 3 prime direction (right hand side)
            -l <f>  : only extend sequences listed in file <f> (one seq name per line) [df: extend all]
            -t      : trim alignment instead of extending it [df: extend]
            -i <s>  : input  alignment is file <s> [df: SEED]
            -o <s>  : output alignment to file <s> [df: SEED]
            -h|-help: print this help, then exit

EOF
}
