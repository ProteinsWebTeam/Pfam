#!/usr/local/bin/perl -w

use strict;
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

my $starttime = time();

###################################################################
# PRELIMINARIES:
# - set default values that command line options may change
# - process command line options
# - set input/output file names, and ensure input files exist
# - process DESC file

# set default values that command line options may change
# build related options
my $force_build = 0;            # TRUE to force build
my $do_nostruct = 0; # TRUE to allow building of CMs with no structure
# calibration related options
my $force_calibrate = 0;        # TRUE to force calibration
my $ncpus_cmcalibrate;          # number of CPUs for cmsearch calls
# search related options
my $evalue;                     # cmsearch E-value to use
my $dbchoice = "rfamseq";
my $no_search = 0;              # TRUE to do cmsearch
my $ncpus_cmsearch;             # number of CPUs for cmsearch calls
my @cmosA = ();             # extra single - cmalign options (e.g. -g)
my @cmodA = ();          # extra double - cmalign options (e.g. --cyk)
# other options
my $queue;    # queue to submit all jobs to (default: auto-determined)
my $dirty = 0;                  # TRUE to leave files on file system
my $verbose = 0;                # TRUE to be verbose with output
my $do_help = 0;             # TRUE to print help and exit, if -h used
my $do_nosearch = 0;            # TRUE to set --nosearch

&GetOptions( "b"          => \$force_build,
	     "nostruct"   => \$do_nostruct,
	     "c"          => \$force_calibrate,
	     "ccpu"       => \$ncpus_cmcalibrate,
             "e=s",       => \$evalue,
	     "dbchoice=s" => \$dbchoice,
	     "nosearch"   => \$no_search,
	     "scpu"       => \$ncpus_cmsearch,
             "cmos=s@"    => \@cmosA,
             "cmod=s@"    => \@cmodA,
	     "queue=s"    => \$queue,
	     "dirty"      => \$dirty,
	     "verbose"    => \$verbose,
	     "h|help"     => \$do_help );

if ( $do_help ) {
  &help();
  exit(1);
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
                                    #    'CM'     => $io->parseCM("CM"),
                                   );
my $msa  = $famObj->SEED;
my $desc = $famObj->DESC;

# extra processing of command-line options 
if ($do_nosearch) { # --nosearch, verify incompatible options are not set
  if (defined $ncpus_cmsearch) {
    die "ERROR --nosearch and --scpu are incompatible";
  }
  if (defined $evalue) {
    die "ERROR --nosearch and -e are incompatible";
  }
  if (@cmosA) {
    die "ERROR --nosearch and --cmosA are incompatible";
  }
  if (@cmodA) {
    die "ERROR --nosearch and --cmodA are incompatible";
  }
}

# ncpus_cmsaerch and ncpus_cmcalibrate must be >= 0
if (defined $ncpus_cmsearch    && $ncpus_cmsearch    < 0) {
  die "ERROR with --scpu <n>, <n> must be >= 0";
}
if (defined $ncpus_cmcalibrate && $ncpus_cmcalibrate < 0) {
  die "ERROR with --ccpu <n>, <n> must be >= 0";
}


if ($do_nostruct) { # --nostruct: verify SEED either has SS_cons with 0 bps or does not have SS_cons
  if ($msa->has_sscons) {
    my $nbps = $msa->num_basepairs;
    if ($nbps > 0) {
      die "ERROR with --nostruct, SEED must have zero bp SS_cons, or no SS_cons";
    }
  } else {    # --nostruct enabled, but msa does not have one, add one
    $msa->set_blank_sscons($sscons);
  }
}

# no all gap columns allowed in SEED
if ($msa->any_allgap_columns) { 
  die "ERROR all gap columns exist in SEED";
}

###################################################################################################
#Gather some info for logging:
my $pwd   = getcwd;
my $phost = hostname;
my $user  = getlogin() || getpwuid($<);
if (! defined $user || length($user) == 0) { 
  die "FATAL: failed to run [getlogin or getpwuid($<)]!\n[$!]";
}
&printlog( "USER: [$user] " );
&printlog( "RFSEARCH USING INFERNAL VERSION 1.1");
&printlog( "Using database $dbchoice\n");

# Deal with rfsearch.log and log directory
if (-e "rfsearch.log") { 
  if (! -w "rfsearch.log") {
    die("FATAL: check permissions on rfsearch.log");
  }
  unlink "rfsearch.log";
}
print "Making farm and log directories....";
umask(002);
mkdir( "$pwd/$$", 0775 ) or die "FATAL: failed to mkdir [$pwd/$$]\n[$!]";
my $dumpdir = "/nfs/nobackup/xfam/$user/$$"; #path for dumping data to on the farm
mkdir ("$dumpdir") or die "FATAL: failed to mkdir [$dumpdir]\n[$!]";

##############
# Build step #
##############
# Get CM
my $cm;
if (-e "$pwd/CM") { 
  $famObj->CM($io->parseCM("$pwd/CM"));
  $cm = $famObj->CM($io->parseCM("$pwd/CM"));
}

my $is_cm_calibrated = 0;
if (defined $cm && $cm->is_calibrated) { 
  $is_cm_calibrated = 1;
}

# figure out if we have to build
my $do_build = 0;
if ($force_build) {
  $do_build = 1;
}
if (! defined $cm) {
  $do_build = 1;
}
if (! $is_cm_calibrated) {
  $do_build = 1;
}
if (youngerThan("$pwd/SEED", "$pwd/CM")) {
  $do_build = 1;
}

if ($do_build) { 
  # get cmbuild options 
  my $buildopts = $desc->{'BM'};
  if (! defined $buildopts) {
    $buildopts = "";
  }
  $buildopts = "" unless $buildopts;
  $buildopts =~ s/-F CM SEED//;
  $buildopts =~ s/cmbuild//;
  # deal with RF related buildopts
  # if RF exists:         replace --rf in $buildopts with --hand
  # if RF does not exist: remove --rf or --hand from buildopts
  if ($msa->has_rf) { 
    $buildopts =~ s/\-\-rf\s*/\-\-hand /;
  } else {                      # no RF
    $buildopts =~ s/\-\-hand\s*//;
    $buildopts =~ s/\-\-rf\s*//;
  }
    
  # clean up any files from previous runs
  if (-e "$pwd/CM")     unlink "$pwd/CM";
  if (-e "$pwd/CM.xxx") unlink "$pwd/CM.xxx";

  # run cmbuild to create new CM
  #TODO don't use system call ask Rob about how he calls esl-sfetch
  Bio::Rfam::Infernal::cmbuild_wrapper($config->infernal_path . "/cmbuild", "-F $buildopts", "CM", "SEED");

  # define (or possibly) redefine $cm, or
  if (! -e "CM") {
    die "ERROR CM does not exist after apparently successful cmbuild";
  }

  $famObj->CM($io->parseCM("CM"));
  $cm = $famObj->CM($io->parseCM("CM"));
  $is_cm_calibrated = 0;
}                               # end of if($do_build)
my $build_endtime = time();

####################
# Calibration step #
####################
if ($force_calibrate || (! $is_cm_calibrated)) { 
  Bio::Rfam::Infernal::cmcalibrate_wrapper($config->infernal_path . "/cmcalibrate", "$pwd/CM");
}
my $calibrate_endtime = time();

###############
# Search step #
###############
if (! $no_search) { 
  # First, determine bit score or E-value threshold to use for cmsearch.
  # 4 possible cases:
  # Case 1: If user set -cme <f> option, use that with -E <f>.
  # If user did not use -cme hn:
  # Case 2:      if GA-2 corresponds to an E-value <= 1000  then use -E 1000
  # Case 3: else if GA-2 corresponds to an E-value >= 50000 then use -E 50000
  # Case 4: else use -T <x>, where <x> = GA-2.

  my $use_cmsearch_eval; # true to use -E $cmsearch_eval, false to use -T $cmsearch_bitsc
  my $bitsc          = 0; # bit score thr to use, irrelevant unless $use_cmsearch_eval is set to 0 below
  my $e_bitsc        = 0; # bit score corresponding to $cmsearch_eval
  my $ga_bitsc       = 0; # GA bitscore for this model
  my $ga_eval        = 0; # E-value corresponding to GA bit score
  my $max_eval       = 50000; # hard-coded max E-value allowed, not applied if -cme used
  my $min_bitsc      = 0; # bit score corresponding to $max_eval, set below
  my $min_eval       = 1000; # hard-coded min E-value allowed, not applied if -cme used

  if (defined $evalue) {        # -e option used on cmdline
    $use_cmsearch_eval = 1;
  } else {                      # -e not used 
    # set default as case 2:
    $cmsearch_eval     = 1000;
    $use_cmsearch_eval = 1;

    # get GA from that and check to see if cases 3 or 4 apply
    $ga = $famObj->DESC->CUTGA; 
    $e_bitsc   = Bio::Rfam::Infernal::evalue_to_bitsc($cm, $evalue, $Z);
    $min_bitsc = Bio::Rfam::Infernal::evalue_to_bitsc($cm, $max_evalue, $Z);
    if (($ga_bitsc-2) < $min_bitsc) { # case 3
      $evalue = $max_eval; 
    } elsif (($ga_bitsc-2) < $e_bitsc) { # case 4
      $bitsc = $ga_bitsc-2;
      $use_cmsearch_eval = 0;
    }
  }

  # define options for cmsearch
  my $ncpus; 
  if (! defined $ncpus_cmsearch) { 
    $ncpus_cmsearch = 4;
  }
  my $options = " -Z $Z --cpu $ncpus_cmsearch ";
  if ($use_cmsearch_eval) {
    $options .= " -E $cmsearch_eval ";
  } else {
    $options .= " -T $cmsearch_bitsc ";
  }
  my $extra_options = Bio::Rfam::Infernal::stringize_infernal_cmdline_options(\@cmosA, \@cmodA);
  $options .= $extra_options;

  # TODO: fix this to use an array of sequence files
  # setup dbfile 
  my $dbconfig     = $config->seqdbConfig($dbchoice);
  my $dbfile       = $dbconfig->{"path"};
  my $Z            = $dbconfig->{"dbsize"};
  my $cmsearchPath = $config->infernalPath . "/cmsearch";
  my $requiredMb   = $ncpus * 4000;

  $queue = "production-rh6 -n$ncpus [hosts=1]\" -M $requiredMb";

  my $cmround=0;
my $cmjobcount=0;
my $failedCmsearchJobs;
my $cmopts;
my (%db2ouput,%dbnames);
my $round;
my $numdbs = 0;
my @dbnames = ();
my $bigCommand;

CMSEARCH: {
  #printf("EPN dbdir: $dbdir\n");
  my @seqdb = glob( "$dbpath/*.fa.gz" ) if not defined $failedCmsearchJobs;
  foreach my $sdb (@seqdb) {
    #printf("EPN sdb: $sdb\n");
    my $cmoutput        = "$$.OUTPUT.$cmround.$cmjobcount";
    my $cmtabfile       = "$$.TABFILE.$cmround.$cmjobcount";
    my $cmsearchTimeOut = "$$.CPUTIME.$cmround.$cmjobcount";
    $db2ouput{$sdb}    = $cmoutput;
    #$sdb =~ s/$dbdir/$dbdir2/g;

    $bigCommand = "/usr/bin/time -f \'\%S \%U\' -o $lustre/$cmsearchTimeOut $command $options --tblout $lustre/$cmtabfile $lustre/$$.CM $sdb > $lustre/$cmoutput;";
    if ($cmjobcount == 0) { 
      &printlog( "###########\nbsub -q $queue -J$pname -o $lustre/$$\.cmsearch.err.$cmround.$cmjobcount > $pwd/$$/$$\.cmsearch.out.$cmround.$cmjobcount" );
      &printlog( $bigCommand . "\n###########" );
      printf("Listing job submission index as they are submitted (%d total; only the first submission command (above) is printed):\n", scalar(@seqdb));
    }
	 
    my $fh = new IO::File;
    $fh -> open("| bsub -q  $queue -J$pname -o $lustre/$$\.cmsearch.err.$cmround.$cmjobcount > $pwd/$$/$$\.cmsearch.out.$cmround.$cmjobcount" ) or die "$!";
    $fh -> print( "$bigCommand\n" );
    $fh -> close;
    $dbnames{$sdb}=1;
    $numdbs++;
    $cmjobcount++;

    printf("%2d ", $numdbs);
    if (($numdbs % 10 == 0) || ($numdbs == scalar(@seqdb))) {
      printf(" (%2d remaining)\n", scalar(@seqdb) - $numdbs);
    }
    ; 
  }
}


$cmopts=$options;
Bio::Rfam::Utils::wait_for_farm($pname, "cmsearch", $numdbs ); 
#Check jobs completed normally...

my @tabFiles = glob("$lustre/$$.TABFILE.*");
my @notOkSearches;

unlink "$pwd/TABFILE" if -e "$pwd/TABFILE";
foreach my $tabFile (@tabFiles) {
  open (tF, "tail -1 $tabFile |") ;
  while (my $l = <tF>) {
    unless ($l =~ m/ok/) {
      print "Job not ok!!\n";
      push (@notOkSearches, $tabFile);
    }
  }
  close tF;
  my $number_failed_jobs = scalar @notOkSearches;
  if ($number_failed_jobs != 0) {
    print "Some jobs have failed! consider rerunning!\n";
  } 

  system("cat $tabFile >> $pwd/TABFILE") and die "FATAL: cant concatenate tabfile files on the farm [$tabFile >> $pwd/TABFILE]\n[$!]";
}


my @cputimes = glob("$lustre/$$.CPUTIME.*");
unlink "$pwd/CPUTIME" if -e "$pwd/CPUTIME";
foreach my $cputime (@cputimes) {
  system("cat $cputime >> $pwd/CPUTIME") and die "FATAL: cant concatenate cputime files on the farm [$cputime >> $pwd/CPUTIME]\n[$!]";
}

system("date >> $pwd/CMSEARCH_JOBS_COMPLETE") and die "FATAL: failed to create $pwd/CMSEARCH_JOBS_COMPLETE\n[$!]";

###################################
# Cleanup all the files on the farm:
if (!defined($dirty) && @warnings==0) {
  system("rm -rf $lustre/*") == 0 or die "FATAL: failed to clean up files on the farm\n[$!]";
  system("rm -rf $lustre") == 0 or die "FATAL: failed to clean up directories on farm!\n[$!]";
}

#Update $buildopts and write to DESC file:
$buildopts = "cmbuild " . $buildopts . " -F CM SEED" unless ($buildopts =~ m/-F CM SEED/);
$desc->{'BM'} = $buildopts;
#$familyIO->writeDESC($desc);
#Write cmopts to desc file:
$desc->{'SM'} = "cmsearch " . $cmopts;
$familyIO->writeDESC($desc);

&printlog( "FINISHED! See OUTPUT and TABFILE." );

###################################
#Time usage reports:
#removed by SWB
&printlog( "##############" );

###################################
#Report warnings:
if (scalar(@warnings)) {
  print "There were " . scalar(@warnings) . " warnings:\n";
  foreach my $w (@warnings) {
    print $w;
  }
}

#FINISHED!
exit(0);
######################################################################

sub printlog {
  my $m = join( '', @_ );
  my  $time = localtime();
  open( LOG, ">>rfsearch.log" ) or die;
  if ( $m ) {
    printf LOG    "%s [%s]\n", $m, $time;
    printf STDERR "%s [%s]\n", $m, $time;
  } else {
    print LOG "\n";
  }
  close LOG;
}
sub update_desc {
  my ($buildopts, $searchopts) = @_;
  open( DNEW, ">DESC.new" ) or die;
  open( DESC, "DESC" ) or die;
  while (<DESC>) {
    if ( /^BM   cmbuild\s+/ ) {
      if ( $buildopts ) {
        print DNEW "BM   cmbuild $buildopts -F CM SEED; cmcalibrate --mpi CM\n";
      } else {
        print DNEW "BM   cmbuild  -F CM SEED; cmcalibrate --mpi CM\n";
      }
      next;
    }
    if ( /^BM   cmsearch\s+/ ) {
      print DNEW "BM   cmsearch $searchopts CM SEQDB\n";
      next;
    }
    print DNEW $_;
  }
  close DESC;
  close DNEW;
  rename( "DESC", "DESC.old" ) or die;
  rename( "DESC.new", "DESC" ) or die;
}


######################################################################
#Validatelsfoutputs: Takes an array of lsf output files. Checks that each jobs finished successfully. Mails user if any jobs failed!
sub validateLsfOutputs {
  my ($user, $jobName, $family, $lsfOutputs) = @_;
  my @warning;
  foreach my $f (@$lsfOutputs) {
    open( F, "< $f") or push(@warning, "WARNING: failed to open [$f] for validating LSF output!");
    my ($ok, $notOk)=(0,0);
    my $warnStr = '';
    while (my $l = <F>) {
      $ok += 1 if $l =~ /Exited with exit code 1./;
      $ok += 10 if $l =~ /Successfully completed./;
      $ok += 100 if $l =~ /^Exited\n/;
      my $prefNotOk = $notOk;
      $notOk += 1 if $l =~ m/error/i;
      $notOk += 10 if $l =~ m/warn/i && $l !~ m/(hspmax|maximum achievable score)/;
      $notOk += 100 if $l =~ m/kill/i;
      $notOk += 1000 if $l =~ m/fatal/i;
      $warnStr .= "\t$l" if $notOk > $prefNotOk;
    }
    close(F);
	
    if ($notOk>0) {
      push(@warning, $f . "\tnotOk=$notOk\n\tthe bad lines in file were:\n$warnStr");
    } elsif ($ok==0) {
      push(@warning, $f. "\tok=$ok");
    }
  }
    
  if (@warning) {
    my $msg = "There were problems with the following lustre output files from\n[$family]:\n";
    $msg .= join("\n", @warning);
    #Bio::Rfam::RfamUtils::mailUser($user, "rfsearch problem job: $jobName $family", $msg);
    return 0;
  } else {
    return 1;
  }
}

#####################################################################

sub checkSEEDgap{
  my $file=shift;
  system("esl-reformat --mingap stockholm $file > $file\.gaptmp") and die "FATAL: \47sreformat --mingap stockholm $file\47 failed";
  my ($alignmentLength, $alignmentLengthT);
  open(ALI,"esl-alistat $file |") or die( "FATAL: Could not open alistat pipe on $file:[$!]");
  while (<ALI>) {
    if (/^Alignment length:\s+(\d+)/) { 
      $alignmentLength = $1; 
    }
  }
  close(ALI);
    
  open(ALI,"esl-alistat $file\.gaptmp |") or die( "FATAL: Could not open alistat pipe on $file:[$!]");
  while (<ALI>) {
    if (/^Alignment length:\s+(\d+)/) { 
      $alignmentLengthT = $1; 
    }
  }
  close(ALI);
    
  if ($alignmentLength != $alignmentLengthT) {
    return 1;                   #fail
	
  }
  return 0;                     #no gap so fine
    
}

######################################################################
# Read GA threshold from DESC and return it.
# If none exists, return "".

sub ga_thresh_from_desc {
  my $ga = "";
  open( DESC, "DESC" ) or die;
  while (<DESC>) {
    if ( /^GA\s+(\d+.\d+)/ ) { 
      $ga = $1;
    }
  }
  close(DESC);

  if ($ga ne "") {
    return $ga;
  } else {
    return "";
  } 
}

######################################################################
# Determine bit score threshold that corresponds to a given E-value threshold
# using cmstat.

sub cmstat_bit_from_E {
  my($infernal_path, $cm_file, $dbsize, $evalue, $use_glocal) = @_;

  open(CMS, "$infernal_path/cmstat -E $evalue -Z $dbsize $cm_file | ") or die "FATAL: failed to open pipe for cmstat -E $dbsize $cm_file\n[$!]";
  my $ok=0;
  my $bitsc;
  while (<CMS>) {
    # Example:
    ## idx   name                  accession   local-inside      local-cyk  glocal-inside     glocal-cyk  model
    ## ----  --------------------  ---------  -------------  -------------  -------------  -------------  -----
    #     1  Glycine               RF00504            24.16          20.49          24.03          22.25     cm
    if (! /^\#/) { 
      my @elA = split(/\s+/);
      if ($use_glocal) {
        $bitsc = $elA[6];
      } else {
        $bitsc = $elA[4];
      }
      $ok = 1;
    }
  }
  close(CMS);

  die "FATAL: failed to parse cmstat output" if not $ok;
  return $bitsc;
}

######################################################################
# Determine consensus length using cmstat.

sub cmstat_clen { 
  my($infernal_path, $cm_file) = $_[0];

  #printf("$Rfam::infernal_path/cmstat -$cm_file");
  open(CMS, "$infernal_path/cmstat $cm_file | ") or die "FATAL: failed to open pipe for cmstat $cm_file\n[$!]";
  my $ok=0;
  my $clen;
  while (<CMS>) {
    # Example:
    ## idx   name                  accession      nseq  eff_nseq   clen      W   bps  bifs  model     cm    hmm
    ## ----  --------------------  ---------  --------  --------  -----  -----  ----  ----  -----  -----  -----
    ##    1  RF00006               -                73      4.46    101    302    19     0     cm  0.590  0.469
    if (! /^\#/) { 
      my @elA = split(/\s+/);
      $clen = $elA[6]; 
      $ok = 1;
    }
  }
  close(CMS);

  die "FATAL: failed to parse cmstat output" if not $ok;
  return $clen;
}

######################################################################
#youngerThan(file1, file2): test if file1 is younger than file2 
sub youngerThan {
  my ($file1, $file2) = @_;
  my ($t1,$t2) = (0,0);
  $t1 = stat($file1)->mtime if -e $file1;
  $t2 = stat($file2)->mtime if -e $file2;
    
  if ($t1>$t2 or $t1==0 or $t2==0) {
    return 1;
  } else {
    return 0;
  }
}
######################################################################

sub help {
  print STDERR <<EOF;

rfsearch.pl: builds, calibrates and searches a CM against a sequence database.
             Run from within a directory containing "SEED" & "DESC" files. 
	     E.g., after running "rfupdate.pl RFXXXXX" or "rfco.pl RFXXXXX".
	     SEED contains a stockholm format alignment and DESC is an internal 
	     Rfam documentation describing each RNA family. 

Usage:      rfsearch.pl [options]

Options:    OPTIONS RELATED TO BUILD STEP (cmbuild):
	    -b      always run cmbuild (default: only run if 'CM' is v1.0 or doesn't exist)
	    --nostruct     set a zero basepair SS_cons in SEED prior to cmbuild (none must exist in SEED)

            OPTIONS RELATED TO CALIBRATION STEP (cmcalibrate):
	    -c      always run cmcalibrate (default: only run if 'CM' is not calibrated)
            --ccpu <n>     set number of CPUs for MPI cmcalibrate job to <n>

            OPTIONS RELATED TO SEARCH STEP (cmsearch):
            -e <f>  set cmsearch E-value threshold as <f> bits
            --dbchoice <s> set sequence database to search as <s> ('rfamseq' or 'testrfamseq')
            --nosearch     do not run cmsearch
            --scpu <n>     set number of CPUs for cmsearch jobs to <n>
	    --cmos <str> add extra arbitrary option to cmsearch with '-<str>'. (Infernal 1.1, only option is '-g')
            --cmod <str> add extra arbitrary options to cmsearch with '--<str>'. For multiple options use multiple
	                 -cmod lines. Eg. '-cmod cyk -cmod sub' will run cmalign with --cyk and --sub.

            OTHER OPTIONS:
            --queue <s>    submit all jobs to queue <s> 
	    --dirty      leave temporary files, don't clean up
  	    --verbose    print loads of cruft
  	    -h|-help     print this help, then exit
EOF
}

