#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Bio::Pfam::LiftoverAlignment;
use Bio::Pfam::Config;

my $evalue_threshold = 1e-10; #Default evalue
my $scores_file = "scores";

my ($alignment, $hmm_file, $scores, $local, $help);

&GetOptions("align=s" => \$alignment,
  "hmm=s"    => \$hmm_file,
  "scores"   => \$scores,
  "evalue=f" => \$evalue_threshold,
   "local"   => \$local,
   "help"    => \$help);

#Check input options
unless($alignment and -s $alignment) {
  warn "Error: Need to supply alignment on the command line.\n";
  help();
}
if($evalue_threshold < 0 or $evalue_threshold > 0.01) {
  warn "Error: E-value should be between 0 and 0.01\n";
  exit;
}
if($scores) {
  unless(-s $scores_file) {
    warn "Error: The scores file ($scores_file) does not exist in the current working directory";
    exit;
  }
  $local=1;
}
if($hmm_file and !-s $hmm_file) {
  warn "Error: The HMM file ($hmm_file) does not exist\n";
}


my $config = Bio::Pfam::Config->new;
my $db = $config->pfamseqLoc."/pfamseq";
my $db_size = $config->dbsize;
my $mini_db = "mini_db.fa";

if($local) {
  #Make HMM if not supplied
  unless($hmm_file) {
    $hmm_file = "$alignment.hmm";
    Bio::Pfam::LiftoverAlignment::make_hmm($alignment, $hmm_file);
  }

  #Make mini database from scores file or via a hmmsearch against pfamseq
  if($scores) {
    Bio::Pfam::LiftoverAlignment::make_database_from_scores($scores_file, $evalue_threshold, $db, $mini_db);
  }
  else {
    my $db_size = $config->dbsize;
    Bio::Pfam::LiftoverAlignment::make_database_from_hmmsearch($hmm_file, $evalue_threshold, $mini_db, $db, $db_size);
  }

  #Run phmmer on each seq in aligment, and align the top hit using hmmalign
  Bio::Pfam::LiftoverAlignment::transform_alignment($alignment, $hmm_file, $mini_db);
}
else {
  my $queue = $config->{farm}->{lsf}->{queue};
  my $memory_mb = 4000;

  my $job_name = "Liftover_$alignment";
  system("bsub -q $queue -M 4000 -R \"rusage[mem=$memory_mb]\" -o liftover.log -J$job_name $0 -align $alignment -local -evalue $evalue_threshold");
}

sub help {
print<<EOF;

This script takes an input alignment and transforms it to the current pfamseq 
database. The script requires an input alignment, and optionally a scores file, 
a HMM file and an E-value threshold. The steps in the script are as follow:

1. Create a HMM if not supplied
2. Create a mini database (using the E-value threshold as cut-off) from either:
    - the scores file (if supplied)
    - running a hmmsearch with the HMM against pfamseq (if scores file not supplied)
3. Run phmmer on each sequence in the input aligment against the mini database
   to find best hit for each sequence
4. Align the best hit for each sequence to the HMM using hmmalign (output file is
   called <alignment>.phmmer>)

The script also creates a file called change.log which shows the replacement
sequence accession for each sequence in the input alignment.

Usage: $0 -align <alignment>

Additional options:

  -scores                       Use scores file in current directory
  -hmm    <hmm_file>            To specify a HMM file
  -evalue <E-value threshold>   To specify an E-value threshold (default 1e-10)
  -local                        Runs the scipt locally
  -help                         Prints this help

EOF
    exit(1);
}
