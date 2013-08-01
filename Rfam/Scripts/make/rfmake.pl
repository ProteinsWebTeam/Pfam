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

use Bio::Easel::SqFile;

###################################################################
# PRELIMINARIES:
# - set default values that command line options may change
# - process command line options
# - set input/output file names, and ensure input files exist
# - process DESC file

# set default values that command line options may change
my $dbchoice = "r79rfamseq";
my $do_align    = 0;            # TRUE to create align file
my $do_subalign = 0;            # TRUE to create SUBALIGN
my $do_help     = 0;            # TRUE to print help and exit, if -h used
my $no_taxinfo  = 0;            # TRUE to NOT create taxinfo file
my $no_compare  = 0;            # TRUE to NOT create comparison file
my $dirty       = 0;            # TRUE to leave files on file system
my $thr;                        # threshold, only defined if -t used
my $farm        = 0;            # TRUE to use farm for alignment jobs
my @cmosA       = ();           # extra single - cmalign options (e.g. -g)
my @cmodA       = ();           # extra double - cmalign options (e.g. --cyk)
my $evalue;                     # E-value threshold to use, set with -e
my $verbose     = 0;            # TRUE to be verbose with output

&GetOptions( "t=s"        => \$thr,
	     "e=s"        => \$evalue,
	     "a",         => \$do_align,
	     "subalign"   => \$do_subalign,
	     "farm"       => \$farm,
	     "dbchoice=s" => \$dbchoice, #TODO: dbchoice should be read from DESC->SM
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmodA,
	     "notaxinfo"  => \$no_taxinfo,
	     "nocompare"  => \$no_compare,
	     "dirty"      => \$dirty,
	     "verbose"    => \$verbose,
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
my $config = Bio::Rfam::Config->new;
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

# extra processing of command-line options 

# setup dbfile 
my $dbconfig  = $config->seqdbConfig($dbchoice);
my $fetchfile = $dbconfig->{"fetchPath"};
my $Z         = $dbconfig->{"dbSize"};

# enforce -a or --subalign selected if used align-specific options used
if ((! $do_align) && (! $do_subalign)) { 
  if ($farm) {
    die "ERROR --farm requires -a or --subalign";
  }
  if (scalar(@cmosA) > 1) {
    die "ERROR --cmosA requires -a or --subalign";
  }
  if (scalar(@cmodA) > 1) {
    die "ERROR --cmodA requires -a or --subalign";
  }
}

# set threshold
my $changed_thr = 0;
if ((defined $thr) && (defined $evalue)) { 
  die "ERROR -t and -e combination is invalid, choose 1"; 
} elsif (defined $evalue) { 
  # TODO, read SM in desc, and pick appropriate E-value line based on that
  my $cm    = $famObj->CM;
  my $bitsc = int((Bio::Rfam::Infernal::cm_evalue2bitsc($cm, $evalue, $Z)) + 0.5); # round up to nearest int bit score above exact bit score
  $thr = sprintf("%.2f", $thr);
  $changed_thr = 1;
  print STDERR  "Using GA threshold of $thr bits, converted from E-value of $evalue from command line\n";
} elsif (defined $thr) { 
  $thr = sprintf("%.2f", $thr);
  print STDERR  "Using GA threshold of $thr from command line\n";
} else { 
  $thr = $famObj->DESC->CUTGA; 
  print STDERR  "Using GA threshold from DESC ($thr) b/c neither -t nor -e was set on command line\n";
  $changed_thr = 1;
}    
if (! defined $thr) {
  die "ERROR: problem setting threshold\n";
}

# write TBLOUT's set of dependent files: "outlist", "species", "rin.dat", and "rinc.dat", if any of them don't exist
if ((! -s "outlist") || (! -s "species") || (! -s "rin.dat") || (! -s "rinc.dat")) { 
  my $rfamdb = $config->rfamlive;
  $io->writeTbloutDependentFiles($famObj, $rfamdb, $famObj->SEED, $thr, $config->RPlotScriptPath);
}

my $outlistI = "outlist";
if (! -s $outlistI) {
  die "ERROR: required file $outlistI does not exist or is empty\n";
}

# set the thresholds based on outlistI
setThresholds($famObj, $thr, $outlistI);

# make and write the SCORE file
$io->makeAndWriteScores($famObj, "outlist");

if ($do_align) { 
  # fetch sequences
  my $fetch_sqfile = Bio::Easel::SqFile->new({
    fileLocation => $fetchfile,
  });
  $fetch_sqfile->fetch_subseqs($famObj->SCORES->regions, 60, "$$.fa"); 
  $fetch_sqfile->close_sqfile();

  # use cmalign to do the alignment
  my $options = "-o align ";
  $options .= Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  # Run cmalign locally or on farm (autodetermined based on job size) 
  Bio::Rfam::Infernal::cmalign_wrapper($config, $user, "a.$$", $options, "CM", "$$.fa", "alignout", "a.$$.err", $famObj->SCORES->numRegions, $famObj->SCORES->nres, ($farm), (! $farm));

  # remove temporary fasta file
  if (! $dirty) {
    unlink "$$.fa";
  }
} # end of if($do_align)

# Output desc file
if ($changed_thr) { 
  $io->writeDESC($famObj->DESC);
}

exit 0;

###############
# SUBROUTINES #
###############
sub setThresholds { 
  my ($famObj, $ga, $outlist) = @_;

  my ($tc, $nc, $bits, $line);
  $nc = 0;
  $tc = 999999;

  open(OUTLIST, "$outlist") or die "FATAL: failed to open $outlist\n[$!]";

  while ($line = <OUTLIST>) {
    if ($line !~ m/^\#/) { 
      # first token is bit score
      chomp $line;
      $bits = $line;
      $bits =~ s/^\s+//;  # remove leading whitespace
      $bits =~ s/\s+.*$//;
	    
      if ($ga <= $bits && $bits < $tc) {
        $tc = $bits;
      }
      if ($ga  > $bits && $bits > $nc) {
        $nc = $bits;
      }
    }
  }
    
  if ((! defined $nc) || ($nc == 0)) {
    $nc = "undefined";
    die "ERROR, unable to set NC threshold, GA set too low. Rerun rfmake.pl with higher bit-score threshold";
  }

  $famObj->DESC->CUTGA($ga);
  $famObj->DESC->CUTTC($tc);
  $famObj->DESC->CUTNC($nc);

  return;
}

######################################################################
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
	    -a           create 'align' alignment with all hits above threshold, with cmalign (requires -t or -e)
	    --subalign   create 'subalign' alignment, with sampling of representative hits
 	    --farm       always run cmalign on the farm [default: don't, run locally]
	    --cmos <str> add extra arbitrary option to cmalign with '-<str>'. (Infernal 1.1, only option is '-g')
            --cmod <str> add extra arbitrary options to cmalign with '--<str>'. For multiple options use multiple
	                 -cmod lines. Eg. '-cmod cyk -cmod sub' will run cmalign with --cyk and --sub.

	    OTHER:
            --nocompare  do not create COMPARISON file
	    --notaxinfo  do not create TAXINFO file
	    --dirty      leave temporary files, don't clean up
  	    --verbose    print loads of cruft
  	    -h|-help     print this help, then exit

EOF
}
