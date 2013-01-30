#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use IO::File;
use File::stat;
use File::Copy;
use Sys::Hostname;
use Cwd;
use Data::Dumper; #Great for printing diverse data-structures.
use DBI;
use Bio::Rfam::Family::MSA;
use Bio::Rfam::Config;
use Bio::Rfam::FamilyIO;
use Bio::Rfam::Family;
use Data::Printer;
#use Rfam;
#use Rfam::RfamAlign;
#use RfamUtils;
use lib "/homes/swb/Rfam/Scripts/make";
use Rfamnew;
#use RfamUtils;
#use Rfam::RfamSearch;
#use RfamTimes;

#BLOCK 0: INITIALISE: 

my $starttime = time();

my( $help,
    $pname,
    $nostruct,
    $nobuild, 
    $glocal,
    $hmmonly,
    $onlyCalibrate,
    $forceCalibrate,
    $cmsearch_eval,
    @extraCmsearchOptionsSingle,
    @extraCmsearchOptionsDouble,
    $debug,
    $bigmem,
    $ncpus_cmsearch,
    $queue,
    $cqueue,
    $squeue,
    $dirty,
    @warnings,
    %runTimes,
    $schema,
    $rfamTimes,
    $status,
	$dbconfig,
	$dbpath,
	$dbchoice,
	$dbsize,
	$binaryconfig,
	$infernal_path
    );

$cmsearch_eval = "";
$queue         = "";
$cqueue        = "";
$squeue        = "";

&GetOptions( 
	     "h|help"                  => \$help,
	     "pname=s"                 => \$pname,
             "nostruct"                => \$nostruct,
	     "nobuild"                 => \$nobuild,
	     "g|glocal"                => \$glocal,
	     "hmmonly"                 => \$hmmonly,
             "cal|calibrate"           => \$onlyCalibrate,
             "fcal|forcecalibrate"     => \$forceCalibrate,
             "cme|cmsearchevalue=s"    => \$cmsearch_eval,
             "cmos|cmsearchoptions=s@" => \@extraCmsearchOptionsSingle,
             "cmod|cmsearchoptiond=s@" => \@extraCmsearchOptionsDouble,
             "debug"                   => \$debug,
	     "bigmem"	     	       => \$bigmem,
	     "cpu"	     	       => \$ncpus_cmsearch,
	     "queue=s"                 => \$queue,
	     "cqueue=s"                => \$squeue,
	     "squeue=s"                => \$cqueue,
		"dbchoice=s" => \$dbchoice,
             "dirty"                   => \$dirty
    );

#set some db options and some paths:
my $config = Bio::Rfam::Config->new;
$dbchoice = "testrfamseq" unless ($dbchoice);
print "database size is $dbsize\n";
$dbconfig = $config->seqdbConfig( $dbchoice);
$dbpath   = $dbconfig->{"path"};
$dbsize  = $dbconfig->{"dbsize"};

$infernal_path = $config->infernalPath;

###################################################################################################

# determine if user set cmsearch E-value, if not we potentially use
# bit score of (NC-2 bits) instead of an E-value cutoff.
my $cme_option_set;
if($cmsearch_eval eq "") { 
    $cme_option_set = 0;
}
else { # user must have set cmsearch e-value
    $cme_option_set = 0;
}
#SWB removed sanger queue options
# make sure queue options make sense

if( $help ) {
    &help();
    exit(1);
}


# make sure files are writable by group
umask(002);

#Gather some info for logging:
my $user =  getlogin() || getpwuid($<);
die "FATAL: failed to run [getlogin or getpwuid($<)]!\n[$!]" if not defined $user or length($user)==0;

&printlog( "USER: [$user] " );
my $pwd = getcwd;
my $phost = hostname;

&printlog( "RFSEARCH USING INFERNAL VERSION 1.1");
&printlog( "Using database $dbchoice\n");
my $buildopts;
#Validate SEED and DESC files
#Replace with Rob's new code for parsing DESC files
#

my ($desc, $descfile);
my $familyIO = Bio::Rfam::FamilyIO->new( ); 

if (-s 'DESC') {
	$descfile = 'DESC';
	$desc = $familyIO->parseDESC( $descfile );
}

#Need to update this desc creation step to use RDFs new code:
else {
    $desc = RfamUtils::generateDesc();
    open(DE, "> DESC") or die "FATAL: failed to open DESC\n[$!]";
    RfamUtils::writeDesc($desc,\*DE);
    RfamUtils::writeDesc($desc,\*STDOUT);
    close(DE);
}

#Read cmbuild/cmsearch flags from the DESC file:
$desc->{'BM'} =~ /cmbuild\s+(.*)\s+CM\s+SEED\s+;/ and do {
    $buildopts = $1;
	print "$buildopts\n";
};

# We no longer do glocal, since we switched to 1.1 which handles fragments sensibly
#$desc->{'BM'} =~ /cmsearch.*\s+-g\s+/ and do {
#    $glocal = 1;
#};

######################################################################
#Give runTimes table an accession/ID - removed by SWB for EBI move, no longer necessary

######################################################################
#Validate the SEED & check for RF and SS_cons annotation
#Needs updating to RDFs family/DESC handler
#
$buildopts = "" unless $buildopts;
if (-e "SEED"){
    open(S, "SEED") or die("SEED exists but couldn't be opened!");
    my $seen_rf;
    my $seen_sscons;
    while(<S>) {
	if( /^\#=GC\s+RF/ ) {
	    $seen_rf = 1;
	}
	if( /^\#=GC\s+SS_cons/ ) {
	    $seen_sscons = 1;
	}
    }
    close(S);

    #check SEED for gaps
    if(&checkSEEDgap("SEED")){
	die  ("FATAL: gap columns in SEED!\n run sreformat --mingap SEED");
    }
    #Using a reference coordinate system
    # Infernal v1.1
    # if no RF in alignment, remove --rf or --hand from $buildopts
    if( !$seen_rf ) { 
	if($buildopts =~ /--rf\s+/)   { $buildopts =~ s/--rf\s+//g;   }
	if($buildopts =~ /--hand\s+/) { $buildopts =~ s/--hand\s+//g; }
    }
    else { # RF does exist, if --rf is in $buildopts, replace with --hand
	if($buildopts =~ /--rf\s+/)   { $buildopts =~ s/--rf\s+/--hand /g;   }
    }
    # Check if we need to add SS_cons to the SEED (only legal if --nostruct enabled)
    if(! $seen_sscons) { # SS_cons does not exist in SEED
	if(! $nostruct) { # --nostruct not enabled, die
	    die("FATAL: no SS_cons in SEED, use --nostruct to build a 0 bp CM");
	}
	else { # --nostruct enabled, if v1p1: add --noss to buildopts, else add zero bp SS_cons to alignment
	    $buildopts .= " --noss";
	}
    } # end of if(! $seen_sscons)
    elsif($nostruct) { # --nostruct enabled, but SS_cons exists, die 
	die "ERROR --nostruct enabled but SEED has SS_cons annotation";
    }
}

######################################################################
#Check for the existence and correct permissions of essential files:
if (-e "rfsearch.log" && -w "rfsearch.log"){
    unlink("rfsearch.log");
}
elsif (-e "rfsearch.log") {
    die("FATAL: check permissions on rfsearch.log");
}
######################################################################
#user must have log dir!
&printlog( "Making farm and log directories....");
#&printlog( "mkdir $pwd/$$" );
umask(002);
mkdir( "$pwd/$$", 0775 ) or die "FATAL: failed to mkdir [$pwd/$$]\n[$!]";

######################################################################
#my $lustre = "$Rfam::scratch_farm/$user/$$"; #path for dumping data to on the farm
my $lustre = "/nfs/nobackup/xfam/$user/$$"; #path for dumping data to on the farm
#eet the stripe pattern on the lustre (farm) file system:- removed by SWB for move to EBI
#http://scratchy.internal.sanger.ac.uk/wiki/index.php/Farm_II_User_notes
#&printlog( "mkdir $lustre" );
mkdir ("$lustre") or die "FATAL: failed to mkdir [$lustre]\n[$!]";
&printlog ("done");
######################################################################
#open a connection to the DB:
#removed by SWB for EBI move
#$schema = RfamTimes->connect("dbi:mysql:host=$Rfam::rdb_host;port=$Rfam::rdb_port;dbname=rfam_times",$Rfam::rdb_user,$Rfam::rdb_pass);
#######

#Build and calibrate the CM if necessary:

my $qchoice = "";
if ($queue  ne "") { $qchoice = $queue; }
if ($cqueue ne "") { $qchoice = $cqueue; }

unless( $nobuild) { 
    my $buildCm = 0;

    $buildCm = 1 if Rfamnew::youngerThan("$pwd/SEED", "$pwd/CM");
    $buildCm = 1 if defined $forceCalibrate;
    $buildCm = 1 if defined $onlyCalibrate;
    #check if CM is calibrated: 
    if (-e "$pwd/CM"){
	$buildCm = 1 if not Rfamnew::isCmCalibrated("$pwd/CM"); 
    }
    else {
	$buildCm = 1;
    }
    
    if (-e "$pwd/CM" && not -w "$pwd/CM") {
	die("FATAL: $pwd/CM file exists but you don't have write access");
    }
    
    if ($buildCm){
	unlink("$pwd/CM.xxx") if -e "$pwd/CM.xxx"; #Clean up old tmp files from cmbuild:
	Rfamnew::cmBuild("$pwd/CM","$pwd/SEED",'1.1', $buildopts, $desc->{'AC'});
	#calibrate model:
	my $iscalibrated=0;
	for (my $try=1; $try<4; $try++){
	    copy("$pwd/CM", "$lustre/CM") or die "FATAL: failed to copy [$pwd/CM] to [$lustre/CM]\n[$!]";
	    
	    $runTimes{'calibration'}=Rfamnew::cmCalibrate("CM",$lustre, $pwd, $debug, $bigmem, $qchoice);
	    # this should work for 1.0 or 1.1
	    $iscalibrated=Rfamnew::isCmCalibrated("$lustre/CM");
	    &printlog( "        cmcalibration took:             " . $runTimes{'calibration'} . " secs" ) if $iscalibrated;
	    last if $iscalibrated;
	    &printlog( "FAILED to calibrate the $lustre/CM, retry number $try");
	}
	&printlog( "FATAL: failed to calibrate the model after 3 tries! Check or ssh settings & ...") if !$iscalibrated;
	die "FATAL: failed to calibrate the model after 3 tries! Check or ssh settings & ..." if !$iscalibrated; 
	#Update time DB now...
	exit(0) if defined $onlyCalibrate;
    }
    else {
	&printlog( "$pwd/CM has already been calibrated, use -fcal if you want to re-calibrate..." );
    }
}

my $initendtime = time();
# swb removed $dbdir, $dbdir2 as redundant with new paths:
#my $dbdir  = $Rfamnew::rfamseq_current_dir;        # glob files from here
#my $dbdir2 = $Rfamnew::rfamseq_current_dir;      # but run things from here

#Find out how big the database is (used for e-value computations for infernal)
# (If we upgrade script to allow alternate databases, we'll need to update this to
# determine size of alternate db.)

&printlog( "DBSIZE: $dbsize");

#####################################################
# Determine bit score or E-value threshold to use.
# 4 possible cases:
# Case 1: If user set -cme <f> option, use that with -E <f>.
# If user did not use -cme hn:
# Case 2:      if GA-2 corresponds to an E-value <= 1000  then use -E 1000
# Case 3: else if GA-2 corresponds to an E-value >= 50000 then use -E 50000
# Case 4: else use -T <x>, where <x> = GA-2.

$dbsize  = ($dbsize*2.)/1000000; #both strands in Mb for infernal

my $use_cmsearch_eval;      # true to use -E $cmsearch_eval, false to use -T $cmsearch_bitsc
my $cmsearch_bitsc = 0;     # irrelevant unless $use_cmsearch_eval is set to 0 below
my $e_bitsc        = 0;     # bit score corresponding to $cmsearch_eval
my $ga_bitsc       = 0;     # GA bitscore for this model
my $ga_eval        = 0;     # E-value corresponding to GA bit score
my $max_eval       = 50000; # hard-coded max E-value allowed, not applied if -cme used
my $min_bitsc      = 0;     # bit score corresponding to $max_eval, set below
my $min_eval       = 1000;  # hard-coded min E-value allowed, not applied if -cme used

if($cme_option_set) { # $cmsearch_eval already set during option processing, case 1:
    $use_cmsearch_eval = 1;
}
else { # -cme not used 
    # set default as case 2:
    $cmsearch_eval     = 1000;
    $use_cmsearch_eval = 1;

    # if DESC exists, get GA from that and check to see if cases 3 or 4 apply
    if(-e "DESC") { 
	$e_bitsc   = cmstat_bit_from_E("$pwd/CM", $dbsize, $cmsearch_eval, (defined $glocal) ? 1 : 0);
	$min_bitsc = cmstat_bit_from_E("$pwd/CM", $dbsize, $max_eval,      (defined $glocal) ? 1 : 0);
	$ga_bitsc  = ga_thresh_from_desc();
	if(($ga_bitsc-2) < $min_bitsc) { # case 3
	    $cmsearch_eval = $max_eval; 
	}
	elsif(($ga_bitsc-2) < $e_bitsc) { # case 4
	    $cmsearch_bitsc    = $ga_bitsc-2;
	    $use_cmsearch_eval = 0;
	}
    }
}
#####################################################

my $command = "$infernal_path/cmsearch";
my $ncpus; 
if(! defined $ncpus_cmsearch) { 
    $ncpus_cmsearch = 2;
}
if($ncpus_cmsearch == 0) { # special case use -n1 with bsub
    $ncpus = 1; 
}
else { 
    $ncpus = $ncpus_cmsearch; 
}
my $options = " -Z $dbsize --cpu $ncpus_cmsearch ";
$options .= " --hmmonly " if( $hmmonly );
$options .= " -g " if( defined $glocal );
if($use_cmsearch_eval) { 
    $options .= " -E $cmsearch_eval ";
}
else { 
    $options .= " -T $cmsearch_bitsc ";
}

# add extra options    
# "-<x>"
if (@extraCmsearchOptionsSingle){ 
    foreach my $opts (@extraCmsearchOptionsSingle){
	$options .= " \-$opts ";
    }
}
# "--<x>    
if (@extraCmsearchOptionsDouble){
    foreach my $opts (@extraCmsearchOptionsDouble){
	$options .= " \-\-$opts ";
    }
}
    
$pname = "cm$$" if( not $pname );
    
&printlog( "Queueing cmsearch jobs" );
copy("$pwd/CM", "$lustre/$$.CM") or die "FATAL: failed to copy $pwd/$$.CM to $lustre/$$.CM\n[$!]";

# determine total amount of memory required 2Gb per CPU by default, 4Gb per cpu if $bigmem is required
my $requiredMb  = 2000    * $ncpus;
my $requiredKb  = 2000000 * $ncpus;
if(defined $bigmem) { 
    $requiredMb *= 2;
    $requiredKb *= 2;
}

# determine queue to use
my $estimatedWallSeconds = cmstat_clen("$pwd/CM") * 0.032 * 3600.; 
# 0.032 is a safe-ish estimate for hours per cpos for most cmsearches. Using
# 0.032 will put all models with CLEN < 250 on the normal queue, and models
# with CLEN >= 250 on the long queue, see
# ~en1/notebook/12_1129_rfam_hangout_ga_threshold/00LOG, Nov 30, 2012 for 
# details on why I choose 0.032.
#
if($qchoice eq "") { # else $qchoice was passed in
    if($estimatedWallSeconds    < (60.   * 20.)) { $qchoice = "small";    } # less than 20 minutes? small queue
    elsif($estimatedWallSeconds < (3600. * 8.))  { $qchoice = "normal";   } # less than 8 hours? normal queue
    elsif($estimatedWallSeconds < (3600. * 36.)) { $qchoice = "long";     } # less than 36 hours? long queue
    else                                         { $qchoice = "basement"; } # more than 36 hours? basement queue
}

#$queue = "$qchoice -n$ncpus -R \"select[type==X86_64] && select[mem>$requiredMb] rusage[mem=$requiredMb] span[hosts=1]\" -M $requiredKb";
# swb: Changed $queue to always use production-rh6 at ebi:
#

$queue = "production-rh6 -n$ncpus -R \"select[type==X86_64] && select[mem>$requiredMb] rusage[mem=$requiredMb] span[hosts=1]\" -M $requiredMb";

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
	if($cmjobcount == 0) { 
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
	 if(($numdbs % 10 == 0) || ($numdbs == scalar(@seqdb))) { printf(" (%2d remaining)\n", scalar(@seqdb) - $numdbs); }; 
     }
 }


$cmopts=$options;
Rfamnew::wait_for_farm($pname, "cmsearch", $numdbs ); 
#Check jobs completed normally...
# removed as part of rfam code refactor
######################################################################
# EPN: when we switch to infernal 1.1...get rid of this. I don't think 'mailUser' works anymore anyhow.
#validate cmsearch LSF outputs:


# EPN original rfsearch.pl script does another validation check here. We've already done enough of that, right?
##############
#Copy OUTPUT files from the farm -- do some validation to ensure all the nodes completed successfully:
#open (lOP, "cat $lustre/$$.OUTPUT.*  |") or die "FATAL: failed to open a pipe for cat $lustre/$$.OUTPUT.* > $pwd/OUTPUT\n[$!]";
#open(pOP, "> $pwd/OUTPUT") or die "FATAL: failed to open $pwd/OUTPUT\n[$!]";
#my $cmsearchTerminalCount=0;
#while(my $op = <lOP>){
#    print pOP $op; #print to the all important OUTPUT file!
#}
#close(lOP);
#close(pOP);
###########validation block ends
#system("cat $lustre/$$.OUTPUT.* > $pwd/OUTPUT")   and die "FATAL: cant concatenate output files on the farm\n[$!]";

#system("cat $lustre/$$.TABFILE.* > $pwd/TABFILE") and die "FATAL: cant concatenate tabfile files on the farm\n[$!]";
#Using glob because the above fails on SRP and friends:
# Rewrite this bit using pg5's pipe method and check for [ok] for validation

my @tabFiles = glob("$lustre/$$.TABFILE.*");
my @notOkSearches;

unlink "$pwd/TABFILE" if -e "$pwd/TABFILE";
foreach my $tabFile (@tabFiles){
    open (tF, "tail -1 $tabFile |") ;
	while (my $l = <tF>) {
		unless ($l =~ m/ok/) {
			print "Job not ok!!\n";
			push (@notOkSearches, $tabFile);
		}
	}
my $number_failed_jobs = scalar @notOkSearches;
if ($number_failed_jobs != 0) {
	print "Some jobs have failed! consider rerunning!\n";
} 

	system("cat $tabFile >> $pwd/TABFILE") and die "FATAL: cant concatenate tabfile files on the farm [$tabFile >> $pwd/TABFILE]\n[$!]";
}


#system("cat $lustre/$$.CPUTIME.* > $pwd/CPUTIME") and die "FATAL: cant concatenate time files on the farm\n[$!]";
my @cputimes = glob("$lustre/$$.CPUTIME.*");
unlink "$pwd/CPUTIME" if -e "$pwd/CPUTIME";
foreach my $cputime (@cputimes){
    system("cat $cputime >> $pwd/CPUTIME") and die "FATAL: cant concatenate cputime files on the farm [$cputime >> $pwd/CPUTIME]\n[$!]";
}

system("date >> $pwd/CMSEARCH_JOBS_COMPLETE") and die "FATAL: failed to create $pwd/CMSEARCH_JOBS_COMPLETE\n[$!]";

###################################
# Cleanup all the files on the farm:
if (!defined($dirty) && @warnings==0){
    system("rm -rf $lustre") and die "FATAL: failed to clean up files on the farm\n[$!]";
}

&update_desc( $buildopts, $cmopts ) unless( !-e "DESC" );
&printlog( "FINISHED! See OUTPUT and TABFILE." );

###################################
#Time usage reports:

my $endtime = time();
my $runtime = $endtime - $starttime;              my $runtimeH      = RfamUtils::secs2human($runtime);
my $inittime = $initendtime - $starttime;         my $inittimeH     = RfamUtils::secs2human($inittime);
my $cmsearchtime = $endtime - $initendtime;       my $cmsearchtimeH = RfamUtils::secs2human($cmsearchtime);

print( "##############\n" );
&printlog( "INIT walltime:     $inittime secs    \t$inittimeH" );
&printlog( "CMSEARCH walltime: $cmsearchtime secs\t$cmsearchtimeH" );
&printlog( "Total walltime:    $runtime secs     \t$runtimeH" );
&printlog( "##############" );

#For the rfam_times DB:
open(CPU, "< CPUTIME") or die "FATAL: could not open CPUTIME file for reading!\n[$!]";
$runTimes{'cmsearch'}=0.0;
while(<CPU>){
    if(/(\d+\.\d+)\s+(\d+\.\d+)/){
	$runTimes{'cmsearch'}+=($1+$2);
    }
}
close(CPU);
$runTimes{'rfsearchwall'}=$runtime;

&printlog( "CPU Times for rfam_acc: |" . $runTimes{'rfam_acc'} . "|" );

foreach my $k (qw(calibration cmsearch rfsearchwall)){
    next if not defined $runTimes{$k};
    my $humanTime = RfamUtils::secs2human($runTimes{$k}); 
    &printlog( "CPU Times: $k\t$runTimes{$k} secs\t[$humanTime]" );
}
&printlog( "##############" );

###################################
#Report warnings:
if (scalar(@warnings)){
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
    if( $m ) {
        printf LOG    "%s [%s]\n", $m, $time;
        printf STDERR "%s [%s]\n", $m, $time;
    }
    else {
        print LOG "\n";
    }
    close LOG;
}

sub update_desc {
    my ($buildopts, $searchopts) = @_;
    open( DNEW, ">DESC.new" ) or die;
    open( DESC, "DESC" ) or die;
    while(<DESC>) {
	if( /^BM   cmbuild\s+/ ) {
	    if( $buildopts ) {
		print DNEW "BM   cmbuild $buildopts -F CM SEED; cmcalibrate --mpi CM\n";
	    }
	    else {
		print DNEW "BM   cmbuild  -F CM SEED; cmcalibrate --mpi CM\n";
	    }
	    next;
	}
	if( /^BM   cmsearch\s+/ ) {
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
    foreach my $f (@$lsfOutputs){
	open( F, "< $f") or push(@warning, "WARNING: failed to open [$f] for validating LSF output!");
	my ($ok, $notOk)=(0,0);
	my $warnStr = '';
	while (my $l = <F>){
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
	
	if ($notOk>0){
	    push(@warning, $f . "\tnotOk=$notOk\n\tthe bad lines in file were:\n$warnStr");
	}
	elsif ($ok==0){
	    push(@warning, $f. "\tok=$ok");
	}
    }
    
    if (@warning){
	my $msg = "There were problems with the following lustre output files from\n[$family]:\n";
	$msg .= join("\n", @warning);
	RfamUtils::mailUser($user, "rfsearch problem job: $jobName $family", $msg);
	return 0;
    }
    else{
	return 1;
    }
}

#####################################################################

sub checkSEEDgap{
    my $file=shift;
    system("esl-reformat --mingap stockholm $file > $file\.gaptmp") and die "FATAL: \47sreformat --mingap stockholm $file\47 failed";
    my ($alignmentLength, $alignmentLengthT);
    open(ALI,"esl-alistat $file |") or die( "FATAL: Could not open alistat pipe on $file:[$!]");
    while(<ALI>) {
	if (/^Alignment length:\s+(\d+)/){ 
	    $alignmentLength = $1; 
    }
    }
    close(ALI);
    
    open(ALI,"esl-alistat $file\.gaptmp |") or die( "FATAL: Could not open alistat pipe on $file:[$!]");
    while(<ALI>) {
	if (/^Alignment length:\s+(\d+)/){ 
	    $alignmentLengthT = $1; 
	}
    }
    close(ALI);
    
    if ($alignmentLength != $alignmentLengthT){
	return 1; #fail
	
    }
    return 0; #no gap so fine
    
}

######################################################################
# Read GA threshold from DESC and return it.
# If none exists, return "".

sub ga_thresh_from_desc {
    my $ga = "";
    open( DESC, "DESC" ) or die;
    while(<DESC>) {
	if( /^GA\s+(\d+.\d+)/ ) { 
	    $ga = $1;
	}
    }
    close(DESC);

    if   ($ga ne "") { return $ga; }
    else             { return "";  } 
}

######################################################################
# Determine bit score threshold that corresponds to a given E-value threshold
# using cmstat.

sub cmstat_bit_from_E {
    my($cm_file, $dbsize, $evalue, $use_glocal) = @_;

    #printf("$Rfam::infernal_path/cmstat -E $evalue -Z $dbsize $cm_file");
    open(CMS, "$Rfamnew::infernal_path/cmstat -E $evalue -Z $dbsize $cm_file | ") or die "FATAL: failed to open pipe for cmstat -E $dbsize $cm_file\n[$!]";
    my $ok=0;
    my $bitsc;
    while(<CMS>){
        # Example:
        ## idx   name                  accession   local-inside      local-cyk  glocal-inside     glocal-cyk  model
        ## ----  --------------------  ---------  -------------  -------------  -------------  -------------  -----
        #     1  Glycine               RF00504            24.16          20.49          24.03          22.25     cm
	if(! /^\#/) { 
	    my @elA = split(/\s+/);
	    if($use_glocal) { $bitsc = $elA[6]; }
	    else            { $bitsc = $elA[4]; }
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
    my($cm_file) = $_[0];

    #printf("$Rfam::infernal_path/cmstat -$cm_file");
    open(CMS, "$Rfamnew::infernal_path/cmstat $cm_file | ") or die "FATAL: failed to open pipe for cmstat $cm_file\n[$!]";
    my $ok=0;
    my $clen;
    while(<CMS>){
        # Example:
        ## idx   name                  accession      nseq  eff_nseq   clen      W   bps  bifs  model     cm    hmm
        ## ----  --------------------  ---------  --------  --------  -----  -----  ----  ----  -----  -----  -----
        ##    1  RF00006               -                73      4.46    101    302    19     0     cm  0.590  0.469
	if(! /^\#/) { 
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

sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches a covariance model against a sequence database.
             Run from within a directory containing "SEED" & "DESC" files. 
	     Eg, after running "rfupdate.pl RFXXXXX" or "rfco.pl RFXXXXX".
	     SEED contains a stockholm format alignment and DESC is an internal 
	     Rfam documentation describing each RNA family. 

Usage:   rfsearch.pl <options>
Options:       --h                           show this help
	       
	       --pname <s>                   Set name for jobs to <s> 
	       --nobuild                     Skip the cmbuild step
	       --nostruct                    Add a zero basepair SS_cons to SEED prior to cmbuild (requires that none exists in SEED)
	       -g|--glocal                   Run cmsearch in glocal mode (DESC cmsearch command is always ignored!)
	       --hmmonly                     An option for long models (eg. SSU/LSU rRNA,...),
	                                     This runs "cmsearch -hmmonly", requires infernal version >=1.1 
	       -cal|--calibrate              Calibrate model and exit
	       -fcal|--forcecalibrate        Force re-calibrating the model
	       -cme|--cmsearchevalue   <num> Set an evalue threshold for cmsearch [Default: 1000]
	       -cmos|--cmsearchoptions <str> Add extra arbitrary options to cmsearch with a single '-'. For multiple options use multiple 
	                                     -cmos lines. Eg. '-cmos g' will run cmsearch in global mode
	       -cmod|--cmsearchoptiond <str> Add extra arbitrary options to cmsearch with a double '-'. For multiple options use multiple 
	                                     -cmod lines. Eg. '-cmod mid' will run cmsearch in 'mid' filter mode
	       --debug                       run cmcalibrate in special debugging mode to try to debug MPI problems			     
	       --bigmem			     Request 3.5Gb memory for the cmalign step. This is only necessary for long alignments (over 1kb) 
	       --cpu <n>	             pass --cpu <n> to cmsearch

                QUEUE
		--queue <s>                  Submit all         jobs to queue <s> (<s>=small, normal, long, basement)
                --cqueue <s>                 Submit cmcalibrate jobs to queue <s>
                --squeue <s>                 Submit cmsearch    jobs to queue <s>

		CLEANUP
		--dirty                      Leave the files on the cluster. 
		
TO ADD:
Alternative filters: fasta, hmmsearch, ...
Add a cmsensitive option - using indels \& local
Run and store local, glocal and trCYK modes. 

ADD A CHECK THAT ALL THE CMSEARCH JOBS COMPLETED!
-(ls 22111.minidb* | wc -l) == (grep -c "//" OUTPUT)
XNU filters also?

checkpointing?
http://scratchy.internal.sanger.ac.uk/wiki/index.php/Checkpoint_and_Restart

EOF
}

