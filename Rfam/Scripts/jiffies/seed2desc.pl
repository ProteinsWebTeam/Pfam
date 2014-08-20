#!/usr/bin/env perl 
#
# seed2desc.pl - create a DESC file formatted file from a SEED
#                alignment's GF annotation as quickly possible
#                with no error checking.
# 
# usage: perl seed2desc.pl SEED > DESC
use strict;
use Getopt::Long;

my $usage  = "usage:  perl seed2desc.pl [OPTIONS] <input seed file>\n\n";
$usage .= "\tOPTIONS:\n";
$usage .= "\t\t-t <x>: set bit score threshold to <x> in SM line\n";
$usage .= "\t\t-e <x>: set E-value   threshold to <x> in SM line\n";
$usage .= "\t\t-acc_opt <string>: Set the accession, eg RF01234\n\n";


my $acc = undef;                # option to specify the accession
my $e_opt = undef;              # cmsearch E-value to use, defined by GetOptions, if -e
my $t_opt = undef;              # cmsearch bit score cutoff to use, defined by GetOptions, if -t

my $options_okay = &GetOptions("e=s"       => \$e_opt,
                               "t=s"       => \$t_opt,
                               "acc_opt=s" => \$acc);

my $exit_early = 0;
if(! $options_okay) { 
  printf("ERROR, unrecognized option;\n\n"); 
  $exit_early = 1;
}

if($exit_early) { 
  printf $usage;
  exit 0;
}
                               
if (defined ($acc)){
  printf("%-5s%s\n", "AC", $acc);
}

while(my $line = <>) { 
  # example:
  #=GF NC 28.5
  #chomp $line;
  if($line =~ m/^\#=GF\s+(\S+)\s+(.+)$/) {
    my ($tag, $value) = ($1, $2);
    # deal with special cases:
    if($tag eq "SQ") { 
      ; # don't print this line
    }
    # break down old BM line that used to include both cmbuild and cmcalibrate line into two lines, BM and CB
    if($tag eq "BM") { 
      if($value =~ m/cmbuild.+\;\s*cmcalibrate/){ 
        #=GF BM cmbuild  -F CM SEED; cmcalibrate --mpi -s 1 CM
        my($value1, $value2) = split(";", $value);
        $value1 =~ s/\;$//;
        printf("%-5s%s\n", $tag, $value1);
        $value2 =~ s/^\s+//;
        # get rid of old '-s <d>' from cmcalibrate command
        $value2 =~ s/\-s\s+\d+//;
        $value2 =~ s/\s\s/ /g;
        printf("%-5s%s\n", "CB", $value2);
      }
      if($value =~ m/cmsearch/) { 
        #=GF BM cmsearch  -Z 274931 -E 1000000  --toponly  CM SEQDB
        $tag = "SM";
        # replace with valid cmsearch command, using user defined threshold
        if(defined $e_opt) { 
          $value = "cmsearch --cpu 4 --verbose --nohmmonly -E $e_opt -Z 549862.597050 CM SEQDB";
        }
        if(defined $t_opt) { 
          $value = "cmsearch --cpu 4 --verbose --nohmmonly -T $t_opt -Z 549862.597050 CM SEQDB";
        }
        printf("%-5s%s\n", $tag, $value);
      }
    }
    else { # echo whatever else was in the SEED
      printf("%-5s%s\n", $tag, $value);
    }
  }
}
