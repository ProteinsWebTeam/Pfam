#!/usr/bin/env perl
# 
# esl-ssplit.pl: split up an input sequence file into smaller files.
# EPN, Fri Jan 17 14:38:43 2014
# 
# This script uses BioEasel's SqFile module and will create a .ssi
# index file of the input fasta file and then delete it. It would be
# possible to modify the script to not require an .ssi file (and
# instead do 2 passes through the input file) but it would require
# adding code to the SqFile module for a probably negligible
# improvement in speed.

use strict;
use Getopt::Long;
use Bio::Easel::SqFile;

my $in_sqfile    = "";    # name of input file to split up, 1st cmd line arg
my $do_nseq      = 1;     # true by default, output files should have a specified number of seqs each
my $nseq_per     = 0;     # number of seqs for each output file
my $do_nfiles    = 0;     # set to 1 if -n, output a specified number of files 
my $nfiles       = 0;     # number of output files, irrelevant unless -n is used
my $do_nres      = 0;     # set to 1 if -r, output files so they have roughly same # of residues
my $do_verbose   = 0;     # set to 1 if -v, output some extra info to stdout
my $do_dirty     = 0;     # 'dirty' mode, don't clean up (e.g. .ssi file).
my $outfile_root = undef; # root for name of output file, default is $in_sqfile, changed if -oroot used
my $outfile_dir  = "";    # dir for output files, pwd unless -odir is used   

&GetOptions( "oroot=s" => \$outfile_root, 
             "odir=s"  => \$outfile_dir,
             "n"       => \$do_nfiles, 
             "r"       => \$do_nres,
             "v"       => \$do_verbose, 
             "d"       => \$do_dirty);

my $usage;
$usage  = "esl-ssplit.pl [OPTIONS] <seqfile to split> <# seqs for each new file (or # new files if -n)>\n";
$usage .= "\tOPTIONS:\n";
$usage .= "\t\t-n        : 2nd cmd line arg specifies number of output files, not sequences per output file\n";
$usage .= "\t\t-r        : requires -n, split sequences so roughly same number of residues are in each output file\n";
$usage .= "\t\t-v        : be verbose with output to stdout, default is to output nothing to stdout\n";
$usage .= "\t\t-d        : dirty mode: leave temporary files on disk (e.g. .ssi index file)\n";
$usage .= "\t\t-oroot <s>: name output files <s> with integer suffix, default is to use input seq file name\n";
$usage .= "\t\t-odir  <s>: output files go into dir <s>, default is pwd\n";

if(scalar(@ARGV) != 2) { die $usage; }
($in_sqfile, $nseq_per) = @ARGV;

# validate input args
if(! -e $in_sqfile) { die "ERROR $in_sqfile does not exist"; }
if($nseq_per <= 0)  { die "ERROR \# seqs for each new file must be positive int (got $nseq_per)"; }

# add '/' to $outfile_dir if nec
if($outfile_dir ne "" && $outfile_dir !~ m/\/$/) { $outfile_dir .= "/"; }
 
# make sure -n was used if -r used
if($do_nres && (! $do_nfiles)) { die "ERROR -r only works in combination with -n"; }

# set output root if not set with -oroot
if(! defined $outfile_root) { $outfile_root = $in_sqfile; }

# determine if we should remove .ssi file we're about to create at end of script, we do remove unless in dirty mode or .ssi file already exists.
my $cleanup_ssi = 1;
if($do_dirty || (-e $in_sqfile . ".ssi")) { $cleanup_ssi = 0; }

# initialize
my $fctr = 1;
my $sctr = 0;
my $cur_file = $outfile_dir . $outfile_root. "." . $fctr;
if($do_nfiles) { 
  $nfiles = $nseq_per; 
  $nseq_per = 0; 
  if($nfiles <= 1) { die "ERROR with -n, number of files must be > 1"; }
}

# open file 
my $sqfile = Bio::Easel::SqFile->new({ fileLocation => $in_sqfile });

# determine number of sequences or residues to output to each file, if nec
my $tot_nseq = $sqfile->nseq_ssi(); # this will create the .ssi index if necessary
my $tot_nres = 0; # we only need to know this if -r set at cmdline ($do_nres will be TRUE)
my $nres_per = 0; # we only need to know this if -r set at cmdline ($do_nres will be TRUE)
if($do_nfiles) { 
  $nseq_per = int($tot_nseq / $nfiles); 
  if($tot_nseq % $nfiles != 0) { $nseq_per++; }
}
if($do_nres) { 
  $tot_nres = $sqfile->nres_ssi();
  $nres_per = int($tot_nres / $nfiles);
  if($tot_nres % $nfiles != 0) { $nres_per++; }
}

# do the work, fetch and output sequences to new files
my $nseq_remaining = $tot_nseq;
my $cur_nseq = 0;
my $cur_nres = 0;
if(! $do_nres) { # simple case: fetch and output $nseq_per seqs at a time
  while($nseq_remaining > 0) { 
    my $cur_file = $outfile_dir . $outfile_root. "." . $fctr;
    $fctr++;
    $cur_nseq = ($nseq_remaining < $nseq_per) ? $nseq_remaining : $nseq_per;
    $sqfile->fetch_consecutive_seqs($cur_nseq, "", -1, $cur_file);
    $nseq_remaining -= $cur_nseq;
    if($do_verbose) { printf("$cur_file finished (%d seqs)\n", $cur_nseq); }
  }
}
else { # complex case: $do_nres is TRUE, we need to keep track of sequence lengths output
  while($nseq_remaining > 0) { 
    $cur_nres = 0;
    my $cur_file = $outfile_dir . $outfile_root. "." . $fctr;
    $fctr++;
    open(OUT, ">" . $cur_file) || die "ERROR unable to open $cur_file for writing"; 
    $cur_nseq = 0;
    while(($cur_nres < $nres_per) && ($nseq_remaining > 0)) { 
      my $seqstring = $sqfile->fetch_consecutive_seqs(1, "", -1, undef);
      $sctr++;
      $cur_nseq++;
      $nseq_remaining--;
      # $seqstring is in this format: "><seqname><description of any length>\n<actual sequence>\n"
      # with exactly two newlines, we want to know the length of actual sequence
      chomp $seqstring;
      if($seqstring =~ m/\n/g) { 
        $cur_nres += length($seqstring) - pos($seqstring);
        print OUT $seqstring . "\n"; # appending \n is nec b/c we chomped it above
      }
      else { die "ERROR error reading sequence number $sctr\n"; }
    }
    close(OUT);
    if($do_verbose) { printf("$cur_file finished (%d seqs, %d residues)\n", $cur_nseq, $cur_nres); }
  }
}

$sqfile->close_sqfile;
if($cleanup_ssi) { 
  if(-e "$in_sqfile.ssi") { unlink "$in_sqfile.ssi"; }
}

exit 0;
