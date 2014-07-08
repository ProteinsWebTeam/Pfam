#!/usr/bin/env perl
# 
# esl-alidepair.pl: remove some consensus basepairs from an alignment based 
#                   on posterior probabilities of aligned residues in the 
#                   paired positions.
#
# EPN, Mon Jul  7 09:03:48 2014
# 
# This script uses BioEasel's MSA module and creates a new alignment
# with >= 0 of the consensus basepairs from the original alignment
# removed. Basepairs with average posterior probabilities of less
# than 0.9 by default (changeable to <x> with -a <x>) are removed.

use strict;
use Getopt::Long;
use Bio::Easel::MSA;

my $in_alifile  = "";   # name of input MSA file
my $outfile     = "";   # name of output alignment file
my $min_avgpp   = 0.9;  # minimum average posterior probability for keeping a consensus basepair
my $use_weights = 0;    # TRUE to use sequence weights

my $usage;
$usage  = "esl-alidepair.pl [OPTIONS] <alignment file to work on> <name of output alignment file>\n";
$usage .= "\tOPTIONS:\n";
$usage .= "\t\t-a <f> : change minimum average posterior probability to keep to <f> [df: $min_avgpp]\n";
$usage .= "\t\t-w     : use sequence weights in the alignment file to weight counts [df: do not]\n";

&GetOptions( "a=s" => \$min_avgpp,
             "w"   => \$use_weights);

if(scalar(@ARGV) != 2) { die $usage; }
($in_alifile, $outfile) = @ARGV;

# validate input args
if(! -e $in_alifile) { die "ERROR $in_alifile does not exist"; }

# open file 
my $msa = Bio::Easel::MSA->new({ fileLocation => $in_alifile });

# check if we have sequence weights (if we need them)
if($use_weights) { 
  if(! $msa->has_sqwgts) { die "ERROR, with -w the alignment must have sequence weights, but $in_alifile does not"; }
}

# get SS_cons and convert to a CT array.
my @ctA = $msa->get_ss_cons_ct();

my @ngapA  = ();
my $nseq = $msa->nseq;
my $alen = $msa->alen;
my ($apos, $lpos, $rpos, $pp_lpos, $pp_rpos);

my @avgppA = ();
for($apos = 0; $apos < $alen; $apos++) { 
  $avgppA[$apos] = 0.; 
  $ngapA[$apos] = 0.;
}

my $tot_nseq = 0.; # this will probably be equal to $nseq, but maybe not if $use_weights is TRUE and we have funky weights

for(my $i = 0; $i < $nseq; $i++) { 
  my $ppstr = $msa->get_ppstring_aligned($i);
  my @ppA = split("", $ppstr);
  my $seqwt = $use_weights ? $msa->get_sqwgt($i) : 1.0;
  $tot_nseq += $seqwt;
  for($apos = 0; $apos < $alen; $apos++) { 
    $lpos = $apos+1;
    $rpos = $ctA[$lpos];
    if($rpos > $lpos) { # lpos and rpos make a basepair ($lpos < $rpos)
      $pp_lpos = $ppA[$apos];
      $pp_rpos = $ppA[$rpos-1];
      if($pp_lpos ne ".") { $avgppA[$apos] += (pp_to_fraction($pp_lpos) * $seqwt); }
      else                { $ngapA[$apos] += $seqwt; }
      if($pp_rpos ne ".") { $avgppA[$apos] += (pp_to_fraction($pp_rpos) * $seqwt); }
      else                { $ngapA[$apos] += $seqwt; }
    }
  }
}

# normalize to get averages, and remove those under the min avg pp
printf("#%4s  %5s  %5s  %9s  %9s  %7s\n", "lpos", "rpos", "avgpp", "nnongap", "ngap", "remove?");
printf("#%4s  %5s  %5s  %9s  %9s  %7s\n", "-----", "-----", "-----", "---------", "---------", "-------");
my $remove_str;
my @new_ssconsA = split("", $msa->get_ss_cons());
for($apos = 0; $apos < $alen; $apos++) { 
  $lpos = $apos+1;
  $rpos = $ctA[$lpos];
  if($rpos > $lpos) { # lpos and rpos make a basepair ($lpos < $rpos)
    $avgppA[$apos] /= (($tot_nseq * 2.) - $ngapA[$apos]);
    if($avgppA[$apos] < $min_avgpp) { 
      $remove_str = "yes";
      $new_ssconsA[$apos] = ".";
      $new_ssconsA[$rpos-1] = ".";
    }
    else { 
      $remove_str = "no";
    }
    printf("%5d  %5d  %5.3f  %9.1f  %9.1f  %7s\n", $lpos, $rpos, $avgppA[$apos], $tot_nseq - ($ngapA[$apos] / 2.), $ngapA[$apos] / 2., $remove_str);
  }
}
my $new_sscons = "";
for($apos = 0; $apos < $alen; $apos++) { $new_sscons .= $new_ssconsA[$apos]; }
$msa->set_ss_cons_wuss($new_sscons);

$msa->write_msa($outfile);

exit 0;

sub pp_to_fraction {
  my ($pp) = @_;
  if($pp eq '*') { return 0.975; }
  if($pp eq '9') { return 0.9; }
  if($pp eq '8') { return 0.8; }
  if($pp eq '7') { return 0.7; }
  if($pp eq '6') { return 0.6; }
  if($pp eq '5') { return 0.5; }
  if($pp eq '4') { return 0.4; }
  if($pp eq '3') { return 0.3; }
  if($pp eq '2') { return 0.2; }
  if($pp eq '1') { return 0.1; }
  if($pp eq '0') { return 0.025; }
  die "ERROR unexpected value $pp in pp_to_fraction"; 
}
