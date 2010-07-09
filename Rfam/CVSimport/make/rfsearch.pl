#!/software/bin/perl -w

use strict;
use Getopt::Long;
use IO::File;
use File::stat;
use File::Copy;
use Sys::Hostname;
use Cwd;
use Data::Dumper; #Great for printing diverse data-structures.
use DBI;

use Rfam;
use Rfam::RfamAlign;
use RfamUtils;
use Rfam::RfamSearch;
use RfamTimes;


#BLOCK 0: INITIALISE: 

my $starttime = time();

my( 
    $nobuild, 
    $glocal,
    $pname,
    $blastonly,
    $blast_eval,
    $blast_eval_sens,
    $blast_eval_spec,
    $nolowcomplexityseg,
    $nolowcomplexitydust,
    $blastpname,
    $help,
    $minidbpname,
    $nolcmask,
    $window,
    $wublastcpus,
    $nofilters,
    $hmmonly,
    $gap_thresh,
    $nuc_freq_thresh,
    $max_pid_thresh,
    $minseqs4cmsearchlimits,
    $maxseqs4cmsearchlimits,
    $altdb,
    $incldb,
    $dirty,
    $onlybuildminis,
    $nomerge,
    @warnings,
    $onlyCalibrate,
    $forceCalibrate,
    %runTimes,
    $debug,
    $schema,
    $rfamTimes,
    $cmsearch_eval,
    @extraCmsearchOptionsSingle,
    @extraCmsearchOptionsDouble
    );

#BLAST/MINIDB defaults
$wublastcpus = 1; #Number of CPUs wu-blast will run on "-cpu N". 
$blast_eval_sens = 100;
#$blast_eval_spec = 0.001;
$blast_eval_spec = 10;
$maxseqs4cmsearchlimits=3000;
$minseqs4cmsearchlimits=100;

#lcmask defaults:
$gap_thresh = 0.5;      #Gap threshold for lcmask
$nuc_freq_thresh = 0.1; #Nucleotide frequency threshold for lcmask
$max_pid_thresh = 0.95;

#Cmsearch e-value threshold:
$cmsearch_eval=1000000;

&GetOptions( 
             "e=s"                 => \$blast_eval,
	     "g|glocal"            => \$glocal,
             "bo|blastonly:s"      => \$blastonly,
	     "wublastcpus=s"       => \$wublastcpus,
	     "nolowcomplexityseg"  => \$nolowcomplexityseg,
             "nolowcomplexitydust" => \$nolowcomplexitydust,
	     "nobuild"             => \$nobuild,
	     "pname=s"             => \$pname,
             "altdb=s"             => \$altdb,
             "incldb=s"            => \$incldb,
	     "m|mini|minidb|minidbpname=s" => \$minidbpname,
             "obm|onlybuildminis"  => \$onlybuildminis,
	     "nf|nofilters"        => \$nofilters,
	     "nolcmask"            => \$nolcmask,
	     "long|hmmonly"        => \$hmmonly,
	     "gapthresh=s"         => \$gap_thresh,
	     "nucfreqthresh=s"     => \$nuc_freq_thresh,
	     "maxpidthresh=s"      => \$max_pid_thresh,
	     "window=s"            => \$window,
             "maxseqs4cmsearchlimits=s"          => \$maxseqs4cmsearchlimits,
             "minseqs4cmsearchlimits=s"          => \$minseqs4cmsearchlimits,
             "dirty"                   => \$dirty,
             "nomerge"                 => \$nomerge,
             "cal|calibrate"           => \$onlyCalibrate,
             "fcal|forcecalibrate"     => \$forceCalibrate,
             "cme|cmsearchevalue=s"    => \$cmsearch_eval,
             "cmos|cmsearchoptions=s@" => \@extraCmsearchOptionsSingle,
             "cmod|cmsearchoptiond=s@" => \@extraCmsearchOptionsDouble,
             "debug"                   => \$debug,
	     "h|help"                  => \$help 
    );

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

######################################################################
my $buildopts;
#Validate SEED and DESC files
my $desc;
if (-s 'DESC'){
    $desc = RfamUtils::slurpDesc();
}
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
};

$desc->{'BM'} =~ /cmsearch.*\s+-g\s+/ and do {
    $glocal = 1;
};

######################################################################
#Give runTimes table an accession/ID
if (defined $desc->{'AC'} && length($desc->{'AC'})>0){
    $runTimes{'rfam_acc'}=$desc->{'AC'};
}
elsif (defined $desc->{'ID'} && length($desc->{'ID'})>0){
    $runTimes{'rfam_acc'}=$desc->{'ID'};    
}
else {
    my @pwd = split(/\//,$pwd);
    $runTimes{'rfam_acc'} = '';
    while(length($runTimes{'rfam_acc'})==0 && @pwd){
	$runTimes{'rfam_acc'} = pop(@pwd);
    }
}

&printlog( "RFSEARCH RUN ON: " . $runTimes{'rfam_acc'} . ":" . $desc->{'ID'} );

######################################################################
#Validate the SEED & check for an RF line:
$buildopts = "" unless $buildopts;
if (-e "SEED"){
    open( S, "SEED" ) or die("SEED exists but couldn't be opened!");
    my $seenrf;
    while(<S>) {
	if( /^\#=GC RF/ ) {
	    $seenrf = 1;
	    last;
	}
    }
    
#Using a reference coordinate system
    if( !$seenrf and $buildopts =~ /--rf/ ) {
	$buildopts =~ s/--rf //g;
    }
}
elsif (!(-e $blastonly)) {
    die("FATAL: check SEED or $blastonly exists");
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
&printlog( "mkdir $phost:$pwd/$$" );
umask(002);
mkdir( "$pwd/$$", 0775 ) or die "FATAL: failed to mkdir [$pwd/$$]\n[$!]";

######################################################################
my $queueSmall     = 'small  -R \"select[type==X86_64]\"';
my $queueNormal    = 'normal -R \"select[type==X86_64]\"';
my $lustre = "$Rfam::scratch_farm/$user/$$"; #path for dumping data to on the farm

#Set the stripe pattern on the lustre (farm) file system:
#http://scratchy.internal.sanger.ac.uk/wiki/index.php/Farm_II_User_notes
&printlog( "mkdir $lustre" );
mkdir("$lustre") or die "FATAL: failed to mkdir [$lustre]\n[$!]";
system("lfs setstripe $lustre 0 -1 -1") and die "FATAL: \42lfs setstripe $lustre 0 -1 -1\42 on the farm failed\n[$!]";
#See the wiki page "LustreStripeSize" for more detail. 
######################################################################
#open a connection to the DB:
$schema = RfamTimes->connect("dbi:mysql:host=$Rfam::rdb_host;port=$Rfam::rdb_port;dbname=rfam_times",$Rfam::rdb_user,$Rfam::rdb_pass);
#######

#Build and calibrate the CM if necessary:
unless( $nobuild || $blastonly ) {
    my $buildCm = 0;

    $buildCm = 1 if RfamUtils::youngerThan("$pwd/SEED", "$pwd/CM");
    $buildCm = 1 if defined $forceCalibrate;
    $buildCm = 1 if defined $onlyCalibrate;
    #check if CM is calibrated: 
    if (-e "$pwd/CM"){
	$buildCm = 1 if not Rfam::RfamSearch::isCmCalibrated("$pwd/CM"); 
    }
    else {
	$buildCm = 1;
    }
    
    if (-e "$pwd/CM" && not -w "$pwd/CM") {
	die("FATAL: $pwd/CM file exists but you don't have write access");
    }
    
    if ($buildCm){
	unlink("$pwd/CM.xxx") if -e "$pwd/CM.xxx"; #Clean up old tmp files from cmbuild:
	Rfam::RfamSearch::cmBuild("$pwd/CM","$pwd/SEED",'1.0', $desc->{'AC'});
	#calibrate model:
	my $iscalibrated=0;
	for (my $try=1; $try<4; $try++){
	    copy("$pwd/CM", "$lustre/CM") or die "FATAL: failed to copy [$pwd/CM] to [$lustre/CM]\n[$!]";
	    
	    $runTimes{'calibration'}=Rfam::RfamSearch::cmCalibrate("CM",$lustre, $pwd, $debug);
	    $iscalibrated=Rfam::RfamSearch::isCmCalibrated("$lustre/CM");
	    &printlog( "        cmcalibration took:             " . $runTimes{'calibration'} . " secs" ) if $iscalibrated;
	    last if $iscalibrated;
	    &printlog( "FAILED to calibrate the $lustre/CM, retry number $try");
	}
	&printlog( "FATAL: failed to calibrate the model after 3 tries! Check or ssh settings & ...") if !$iscalibrated;
	die "FATAL: failed to calibrate the model after 3 tries! Check or ssh settings & ..." if !$iscalibrated; 
	#Update time DB now...
	$rfamTimes   = $schema->resultset('RfamTimes')
	    ->update_or_create(%runTimes
			       ,);
	exit(0) if defined $onlyCalibrate;
    }
    else {
	&printlog( "$pwd/CM has already been calibrated, use -fcal if you want to re-calibrate..." );
    }
}

##get the W value from the CM file.
#Make a fetchWindow() function?
my $cmwindow = 0;
if (-e "$pwd/CM"){
    open (CMS, "$Rfam::infernal_path/cmstat -m $pwd/CM |") or die "FATAL: failed to open a pipe for $Rfam::infernal_path/cmstat -m $pwd/CM\n[$!]";
    while(<CMS>){
	next if /^\#/;
	my @vals = split(/\s+/, $_);
	$cmwindow = $vals[8] if RfamUtils::isNumeric($vals[8]);
    }
    close(CMS);
}
elsif (!(-e $blastonly)) {
    die ("The $pwd/CM file is missing! SEED file exists, check cmbuild ran. Check directory is mounted."); 
}

#Moderately paranoid checking of the all important window parameter:
if ( (int($cmwindow) == $cmwindow) && $cmwindow>0 && !(defined($window)) ){
    $window=$cmwindow; 
    &printlog( "Using window [$window] from CM file" );
}
elsif (defined(($window)) && (int($window) == $window) && $window>0){
    &printlog( "Using window [$window] from user input" );
}
else {#Indesperation we choose an ugly default:
    $window=200;
    push(@warnings, "WARNING: essential window parameter not given, using a dodgy default: W=$window\n");
    &printlog( "WARNING: essential window parameter not given, using a dodgy default: $window" );
}

if( -e "CMSEARCH_JOBS_COMPLETE" ) {
    unlink( "CMSEARCH_JOBS_COMPLETE" ) or die "can't remove file [CMSEARCH_JOBS_COMPLETE]\n";
}

my $initendtime = time();

#BLOCK 1: RUN BLAST

my $fafile = "$$.fa";

if(defined($blast_eval)){
    $blast_eval_sens = $blast_eval;
    $blast_eval_spec = $blast_eval;
}

#WU-BLAST low-complexity filteres on or off. Default is on.
my $blastfiltersstring;
if (!defined($nolowcomplexityseg) && !defined($nolowcomplexitydust)){
    $blastfiltersstring = " filter=seg filter=dust";
}
elsif (defined($nolowcomplexityseg) && !defined($nolowcomplexitydust)){
    $blastfiltersstring = " filter=dust";
    &printlog( "WARNING: turning off the the seg low complexity filter!" );
}
elsif (!defined($nolowcomplexityseg) && defined($nolowcomplexitydust)){
    $blastfiltersstring = " filter=seg";
    &printlog( "WARNING: turning off the the dust low complexity filter!" );    
}
else {
    $blastfiltersstring = " ";
    &printlog( "WARNING: turning off the the seg and dust low complexity filters!" );
}

my $queue = "long -n$wublastcpus -R \"span[hosts=1] && select[type==X86_64]\"";

my %seedseqlist;
if (defined($blastonly) && -e $blastonly) {
    system "sreformat -u fasta $blastonly > $fafile" and die "can't convert $blastonly to $fafile";
}
elsif ($nolcmask) {
    system "sreformat -u fasta SEED > $fafile" and die "can't convert SEED to $fafile";
}
else {
    make_lcmask_file("SEED", $fafile, \%seedseqlist);
}

my $blastdbdir  = $Rfam::rfamseq_current_dir;        # glob files from here
my $blastdbdir2 = $Rfam::rfamseq_farm2_run_dir;      # but run things from here

if (defined($altdb)){
    $blastdbdir .= "/" . $altdb;
    $blastdbdir2 .= "/" . $altdb;
}

my @blastdb = (glob( "$blastdbdir/*.fa.xnd"), glob( "$blastdbdir/*.fasta.xnd" ));

if (defined($incldb)){
    push(@blastdb,glob( "$blastdbdir/$incldb/*.xnd"));
}

my @tmparray;
foreach my $bdb (@blastdb) {
    if ($bdb !~ /rfamseq.fa.xnd/){
	push(@tmparray, $bdb);
    }
}
@blastdb = @tmparray;

if (@blastdb==0){
    my @files = glob( "$blastdbdir/*" );
    
    &printlog( "RUN:");
    foreach my $f (@files){
	&printlog( "xdformat -n -I -o $f $f; seqstat $f | grep ^Total | tr -d \42a-zA-Z#: \42 >> $$.dbsize");
    }
    &printlog( "cat $$.dbsize | perl -lane \47\$a=\$a+\$_; print \$a\47 | tail -n 1 >$blastdbdir/DBSIZE");
    die("No BLASTDBs in $blastdbdir/*.fa.xnd");
    
}

#Find out how big the database is (used for e-value computations for blast & HMMer):
my $dbsize=0;
if (defined($altdb)){ #if altdb, check for DBSIZE file or die - LEB
	open(DBSIZE, "< $blastdbdir/DBSIZE") or die "FATAL: failed to open $blastdbdir\n[$!]";
	while(<DBSIZE>){
		if (/(\d+)/){
			$dbsize=$1;
			last;
		}
	}
} elsif (defined($incldb)){
	open(DBSIZE, "< $blastdbdir/DBSIZE") or die "FATAL: failed to open $blastdbdir\n[$!]";
	while(<DBSIZE>){
		if (/(\d+)/){
			$dbsize=$1;
			last;
		}
	}
	$dbsize  += Rfam::RfamSearch::getDbSize();
}
else {
	$dbsize  =  Rfam::RfamSearch::getDbSize();
}
#&printlog( "DBSIZE: $dbsize");

my $nobjobs = scalar(@blastdb);

my @index = ( 1..$nobjobs );
my $round = 0;
#things to copy to lustre file system
copy("$pwd/$fafile", "$lustre/$fafile") or die "FATAL: failed to copy $pwd/$fafile to $lustre/$fafile\n[$!]";


unless( $minidbpname || $nofilters ) {
    my $numBlastJobsPerCpu = computeNumberBlastJobsPerCpu($nobjobs, $runTimes{'rfam_acc'});
    &printlog( "Submitting [$numBlastJobsPerCpu] jobs to each node." );
    
    my @runIndex=@index;
  BLAST: { 
      if( $round or !$blastpname ) {
	  #&printlog( "Queuing up blast jobs [round ".$round."]" );
	  $blastpname = $$ if( !$blastpname );
	  #foreach my $i ( @index ) { #This is cleverer than it looks, see below for rerunning failed jobs:
	  my $node=1;
	  while(@index){
	      my @subIndex = splice(@index, 0, $numBlastJobsPerCpu);
	      my $bigCommand='';
	      foreach my $i ( @subIndex ) { 
		  
		  my $blastdb = $blastdb[$i-1];
	      
		  $blastdb =~ s/\.xnd$//g;
		  $blastdb =~ s/$blastdbdir/$blastdbdir2/g;
		  my $blastcmd = "";
		  my $blastcmdsens = "";
		  my $blastcmdspec = "";
		  $blastcmdsens = "wublastn $blastdb $lustre/$fafile W=7 B=150000 V=150000 E=$blast_eval_sens mformat=3 hspsepSmax=$window -lcmask cpus=$wublastcpus Z=$dbsize $blastfiltersstring"; 
		  $blastcmdspec = "wublastn $blastdb $lustre/$fafile W=7 B=150000 V=150000 E=$blast_eval_spec mformat=3 hspsepSmax=$window -lcmask cpus=$wublastcpus Z=$dbsize M=1 N=-3 Q=3 R=3  $blastfiltersstring";
		  
		  $bigCommand .= "/usr/bin/time -f \'\%S \%U\' -o $lustre/$blastpname\.blastout.cputime1.$i $blastcmdspec >  $lustre/$blastpname\.blastout.$i;\n";
		  $bigCommand .= "/usr/bin/time -f \'\%S \%U\' -o $lustre/$blastpname\.blastout.cputime2.$i $blastcmdsens >> $lustre/$blastpname\.blastout.$i;\n";
		  $bigCommand .= "wublast2minidb.pl -d $blastdb -w $window -l $maxseqs4cmsearchlimits -f $lustre/$blastpname\.blastout.$i -o $lustre/$blastpname\.minidb.$i;";
		  &printlog( "###########\nbsub -q $queue -J\"rf$$\" -o $lustre/$$\.berr.$node > $pwd/$$/$$\.bout.$node") if $i==1;
		  &printlog( $bigCommand . "\n###########" ) if $i==1;
	      }
	      
	      my $fh = new IO::File;
	      $fh -> open("| bsub -q $queue -J\"rf$$\" -o $lustre/$$\.berr.$node > $pwd/$$/$$\.bout.$node\n") or die "$!";
	      $fh -> print( "$bigCommand\n" );
	      $fh -> close;
	      $node++;
	  }
	  
	  RfamUtils::wait_for_farm("rf$blastpname", 'blast', $node-1 ); 
	  
      }
      
      my @rerun = ();      
      foreach my $ii ( @runIndex ) {           #/lustre/scratch103/sanger/pg5/29077/29077.blastout.32.error
	  if (-e "$lustre/$blastpname\.blastout\.$ii\.err" or -e "$lustre/$blastpname\.blastout\.$ii\.error"){
	      &printlog( "Blast job didnt finish [$blastpname\.blastout\.$ii] -- rerunning blast search." );
	      push( @rerun, $ii );
	      unlink("$lustre/$blastpname\.blastout\.$ii\.err") if -e "$lustre/$blastpname\.blastout\.$ii\.err";
	      unlink("$lustre/$blastpname\.blastout\.$ii\.error") if -e "$lustre/$blastpname\.blastout\.$ii\.error";
	      push(@warnings, "WARNING: a blast job didn't finish [$lustre/$blastpname\.blastout\.$ii]\n");
	      $dirty=1;
	  }
      }
      
      if( @rerun ) {
	  $round ++;
	  if( $round > 3 ) {
	      &printlog( "FATAL: Maximum number of Blast failures" );
	      die( "FATAL: Maximum number of Blast failures" );
	  }
	  
	  if (scalar(@rerun) == $nobjobs){
	      &printlog( "FATAL: all the bsub jobs need rerunning! Indicates farm is fucked up! Or your ssh keys to and from the farm have not been generated! Or the bsub paramaters are screwed! Or farm2-login:/lustre/scratch103 is full! Or ..." );
	      die( "FATAL: all the bsub jobs need rerunning! Indicates farm is fucked up! Or your ssh keys to and from the farm have not been generated! Or the bsub paramaters are screwed! Or farm2-login:/lustre/scratch103 is full! Or ..." );
	  }
	  else {
	      @index = @rerun;
	      my $rerun = scalar(@rerun);
	      push(@warnings, "WARNING: rerunning $rerun blast jobs\n");
	  }
	  redo BLAST;
      }
  }
    
  #Log the times taken by blast:
  my @blastTimes = glob("$lustre/$blastpname.blastout.cputime*");
  foreach my $timeFile (@blastTimes){
      open(T, "< $timeFile") or die "FATAL: failed to open $timeFile\n[$!]";
      while (<T>){
	  if(/(\d+\.\d+)\s+(\d+\.\d+)/){
	      $runTimes{'blast'}+=($1+$2);
	  }
      }
      close(T);
      unlink($timeFile) if not defined $dirty;
  }

    $rfamTimes   = $schema->resultset('RfamTimes')
	->update_or_create(%runTimes
			   ,);
    
    my @lsfOutputs = glob("$lustre/$blastpname\.berr.*");
    $dirty = 1 if not validateLsfOutputs($user, "blastJobs:rf$blastpname", $runTimes{'rfam_acc'} . ":" . $desc->{'ID'} . " $pwd", \@lsfOutputs);
##
}

my $blastendtime = time();

#BLOCK 2: BUILD MINIDBs.
#############

my @minidbnames = ();
if( $minidbpname ) {
    @minidbnames = glob( "$minidbpname\.minidb.*" );
    if( @minidbnames ) {
	foreach my $minidbname (@minidbnames){
	    #This loop is required - can be too many files for a copy $minidbpname\.minidb.* $lustre/:
	    copy("$minidbname", "$lustre/") or die "FATAL: failed to copy $minidbname to $lustre\n[$!]";
	}
    }
    else {
	&printlog( "FATAL: no minidb files [$minidbpname\.minidb.*]" );
	die "FATAL: there were no minidb files [$minidbpname\.minidb.*]!";
    }
}
elsif (!$nofilters && !$minidbpname) {

    $minidbpname = $blastpname;
    @minidbnames = glob( "$lustre/$minidbpname\.minidb.*" );
    die "FATAL: there were no minidb files [$lustre/$minidbpname\.minidb.*]!" if @minidbnames == 0;
    my (@toosmalldbname, @toosmallnumseqs, @numseqs, $minlargeminidbname);
    my $minlargeminidbnumseqs=0;
    #Gather info on minidbs: 
    foreach my $mdb (@minidbnames){
	
	chomp($mdb);
	copy($mdb, "$pwd/") or warn "WARNING: error copying $mdb to $pwd/\n[$1]";
	$mdb =~ s/$lustre\///;
	my $numseqs=0;
	if ($mdb =~ /\d+\.minidb\.\d+\.\d+\.(\d+)/){
	    $numseqs=$1;
	    push(@numseqs,$numseqs);
	}
	
	if ($numseqs<$minseqs4cmsearchlimits){
	    push(@toosmalldbname,$mdb);
	    push(@toosmallnumseqs,$numseqs);
	}
	elsif ( $numseqs < $minlargeminidbnumseqs){
	    $minlargeminidbname = $mdb;
	    $minlargeminidbnumseqs = $numseqs;
	}
    }

    if (defined($onlybuildminis)){
	&printlog( "Only build minidb\47s option invoked. Stopping here." );
	exit();
    }

    
    #Merge small files!
    if (!$nomerge){
	while (@toosmalldbname>1){
	    my $dbname1    = pop(@toosmalldbname);
	    my $dbname2    = pop(@toosmalldbname);
	    my $dbnumseq1  = pop(@toosmallnumseqs);
	    my $dbnumseq2  = pop(@toosmallnumseqs);
	    my $ttlnumseqs = $dbnumseq1+$dbnumseq2;
	    my $newdbname  = $dbname1 . ".m" . $ttlnumseqs;
	    
	    if (!(-e $newdbname)){#only merge and rm if new does not exist
		system("cat $dbname1 $dbname2 > $newdbname; rm $dbname1 $dbname2");
	    }
	    
	    if ($ttlnumseqs<$minseqs4cmsearchlimits){
		unshift(@toosmalldbname,$newdbname);
		unshift(@toosmallnumseqs,$ttlnumseqs);
	    }
	    elsif ($ttlnumseqs < $minlargeminidbnumseqs){
		$minlargeminidbname = $newdbname;
		$minlargeminidbnumseqs = $ttlnumseqs;
	    }
	}
    
	if (@toosmalldbname==1 && defined($minlargeminidbname) && defined($toosmallnumseqs[0])){
	    my $ttlnumseqs = $minlargeminidbnumseqs+$toosmallnumseqs[0];
	    my $newdbname = $minlargeminidbname . ".m" . $ttlnumseqs;
	    system("cat $toosmalldbname[0] $minlargeminidbname > $newdbname");
	    system("rm $toosmalldbname[0] $minlargeminidbname");
	}
    }
    
    if (defined($blastonly)){
	&printlog( "--blastonly option invoked - stopping. Cleanup $$ files on the farm!");
	exit();
    }
    
    @minidbnames = glob( "$minidbpname\.minidb.*" );
    if (!$nomerge){
	foreach my $mdb (@minidbnames){
	    copy($mdb, "$lustre/") or die "FATAL: failed to copy $mdb to $lustre/";
	}
    }
}

my $numminidbs = scalar(@minidbnames);  # number of minidb files. 
&printlog( "Submit $numminidbs minidbs.");

my $minidbendtime = time();

#BLOCK 3: RUN CMSEARCH:
my $command = "$Rfam::infernal_path/cmsearch";
$dbsize  = int($dbsize/1000000); #In Mb for infernal
my $options = " -Z $dbsize -E $cmsearch_eval "; #
$options .= " --hmmonly " if( $hmmonly );
$options .= " --toponly " if( !$nofilters );
$options .= " -g " if( defined $glocal );

if (@extraCmsearchOptionsSingle){#covers the '-' options
    foreach my $opts (@extraCmsearchOptionsSingle){
	$options .= " \-$opts ";
    }
}

if (@extraCmsearchOptionsDouble){#covers the '--' options 
    foreach my $opts (@extraCmsearchOptionsDouble){
	$options .= " \-\-$opts ";
    }
}

$pname = "cm$$" if( not $pname );
#copy( "CM",  "$lustre/$$.CM") or die "FATAL: error copying CM file to $$.CM\n[$!]";

&printlog( "Queueing cmsearch jobs" );
copy("$pwd/CM", "$lustre/$$.CM") or die "FATAL: failed to copy $pwd/$$.CM to $lustre/$$.CM\n[$!]";
my $queuecm      = 'long -R \"select[type=X86_64]\"';
my $cmround=0;
my $cmjobcount=0;
my $failedCmsearchJobs;
my $cmopts;
my (%minidb2ouput,%minidbnames);
$round=0;
CMSEARCH: {
    if ($nofilters){
	my @seqdb = glob( "$blastdbdir/*.fa.gz" ) if not defined $failedCmsearchJobs;
	@minidbnames=@seqdb if not defined $failedCmsearchJobs;
	$numminidbs=0 if not defined $failedCmsearchJobs;
	$numminidbs = scalar(@seqdb);
	foreach my $sdb (@seqdb) {
	    my $cmoutput        = "$$.OUTPUT.$cmround.$numminidbs";
	    my $cmtabfile       = "$$.TABFILE.$cmround.$numminidbs";
	    my $cmsearchTimeOut = "$$.CPUTIME.$cmround.$cmjobcount";
	    $minidb2ouput{$sdb} = $cmoutput;
	    $sdb =~ s/\.gz$//g;
	    $sdb =~ s/$blastdbdir/$blastdbdir2/g;
	    my $fhcm = IO::File->new();
	    $fhcm -> open( "| bsub -q $queuecm -o $lustre/$$.err.\%I  -J$pname > $pwd/$$/$$.out.\%I" ) or die "$!";
	    $fhcm -> print( "/usr/bin/time -f \'\%S \%U\' -o $lustre/$cmsearchTimeOut $command $options --tabfile $lustre/$cmtabfile $lustre/$$.CM $sdb > $lustre/$cmoutput\n" );
	    $fhcm -> close;
	    $minidbnames{$sdb}=1;
	    $numminidbs++;
	    $cmjobcount++;
	}
    }
    else {
	$numminidbs = scalar(@minidbnames);
	foreach my $minidb ( @minidbnames ) { 
	    my $cmoutput        = "$$.OUTPUT.$cmround.$cmjobcount";
	    my $cmtabfile       = "$$.TABFILE.$cmround.$cmjobcount";
	    my $cmsearchTimeOut = "$$.CPUTIME.$cmround.$cmjobcount";
	    $minidb2ouput{$minidb} = $cmoutput;
	    my $fhcm = new IO::File;
	    $fhcm -> open("| bsub -q $queue -J$pname -o $lustre/$$.cmsearch.err.$cmround.$cmjobcount > $pwd/$$/$$.cmsearch.out.$cmround.$cmjobcount") or die "$!";
	    &printlog( "\tR1: | bsub -q $queue -J$pname -o $lustre/$$.cmsearch.err.$cmround.$cmjobcount > $pwd/$$/$$.cmsearch.out.$cmround.$cmjobcount" ) if $cmjobcount==0;
	    $fhcm -> print( "/usr/bin/time -f \'\%S \%U\' -o $lustre/$cmsearchTimeOut $command $options --tabfile $lustre/$cmtabfile $lustre/$$.CM $lustre/$minidb > $lustre/$cmoutput\n" );
	    &printlog( "\tR2: /usr/bin/time -f \'\%S \%U\' -o $lustre/$cmsearchTimeOut $command $options --tabfile $lustre/$cmtabfile $lustre/$$.CM $lustre/$minidb > $lustre/$cmoutput" ) if $cmjobcount==0;
	    $minidbnames{"$lustre/$minidb"}=1;
	    $fhcm -> close;
	    $cmjobcount++;
      }
    }


$cmopts=$options;
RfamUtils::wait_for_farm($pname, "cmsearch", $numminidbs ); 

#Check jobs completed normally...
##what is the difference between $truncatedminidb and $mdb?
my @cmrerun = ();
foreach my $mdb (@minidbnames){
    
    my $cmoutput = $minidb2ouput{$mdb};
    
    if (-e "$lustre/$cmoutput\.err"){
	&printlog( "WARNING: a cmsearch job didn't finish properly [$lustre/$cmoutput] saw:[$lustre/$cmoutput\.err] -- rerunning cmsearch search." );
	push(@warnings,"WARNING: a cmsearch job didn't finish properly [$lustre/$cmoutput] saw:[$lustre/$cmoutput\.err] -- rerunning cmsearch search.");
	open(CMERR, "< $lustre/$cmoutput\.err") or die "Can't open $lustre/$cmoutput\.err\n[$!]";
	my $truncatedminidb = <CMERR>;
	chomp($truncatedminidb);
	close(CMERR);
	push( @cmrerun, $truncatedminidb );
	system("rm $lustre/$cmoutput\.err");
	next; #Found a problem -- skip the below validation
    }
    
##################VALIDATING $cmoutput
    open(HD, "head -n 6 $lustre/$cmoutput | ") or warn "WARNING: failed to run head on [$lustre/$cmoutput]";
    my $headOk=0;
    while(my $hd=<HD>){
	$headOk++ if $hd=~/\# cmsearch/;
	$headOk++ if $hd=~/\# INFERNAL 1.0/;
	$headOk++ if $hd=~/\# command\:.+cmsearch.+CM/;
    }
    close(HD);
    
    open(TL, "tail -n 4 $lustre/$cmoutput | ") or warn "WARNING: failed to run tail on [$lustre/$cmoutput]";
    my $tailOk=0;
    while(my $tl=<TL>){
	$tailOk++ if $tl=~/\/\//;
	$tailOk++ if $tl=~/\# CPU time/;
    }
    close(TL);
    
    if ($headOk<3 or $tailOk<2){
	&printlog( "WARNING: a cmsearch job didn't finish properly [$lustre/$cmoutput] [headOk:$headOk<3 or tailOk:$tailOk<2] -- rerunning cmsearch search." );
	push(@warnings,"WARNING: a cmsearch job didn't finish properly [$lustre/$cmoutput] [headOk:$headOk<3 or tailOk:$tailOk<2] -- rerunning cmsearch search.");
	push( @cmrerun, $mdb );
    }
###################
}

if( @cmrerun ) {
    $cmround++;
    @minidbnames = @cmrerun;
    $failedCmsearchJobs=scalar(@cmrerun);
    if ($round < 4){#Retry a maximum of 3 times
	&printlog( "WARNING: restarting jobs [$failedCmsearchJobs]!" );
	redo CMSEARCH;
    }
    else {
	&printlog( "FATAL: Maximum number of cmsearch failures, cleanup the farm \47$lustre\47 manually" );
	die;
    }
}
}#CMSEARCH block ends
######################################################################
#validate cmsearch LSF outputs:
my @lsfOutputsCmsearch1 = glob("$lustre/$$.cmsearch.err.*");
my @lsfOutputsCmsearch2 = glob("$lustre/$$.cmsearch.checkerrs.*");
my @lsfOutputsCmsearch3 = glob("$lustre/$$.err.*");
#print "DELME:[user:{" .$user ."}   ". "{blastJobs:rf$pname}" ."   {". $runTimes{'rfam_acc'} . "}:{" . $desc->{'ID'} . "} pwd{$pwd}]\n";
$dirty = 1 if not validateLsfOutputs($user, "cmsearchJobs:$pname", $runTimes{'rfam_acc'} . ":" . $desc->{'ID'} . " $pwd", \@lsfOutputsCmsearch1);
$dirty = 1 if not validateLsfOutputs($user, "cmsearchJobs:$pname", $runTimes{'rfam_acc'} . ":" . $desc->{'ID'} . " $pwd", \@lsfOutputsCmsearch2);
$dirty = 1 if not validateLsfOutputs($user, "cmsearchJobs:$pname", $runTimes{'rfam_acc'} . ":" . $desc->{'ID'} . " $pwd", \@lsfOutputsCmsearch3);

##############
#Copy OUTPUT files from the farm -- do some validation to ensure all the nodes completed successfully:
open (lOP, "cat $lustre/$$.OUTPUT.*  |") or die "FATAL: failed to open a pipe for cat $lustre/$$.OUTPUT.* > $pwd/OUTPUT\n[$!]";
open(pOP, "> $pwd/OUTPUT") or die "FATAL: failed to open $pwd/OUTPUT\n[$!]";
my $cmsearchTerminalCount=0;
while(my $op = <lOP>){
    print pOP $op; #print to the all important OUTPUT file!
    if ($op =~ /^\# command:.*cmsearch.*CM.*\s+(\S+)/){
	if(defined $minidbnames{$1}){
	    $minidbnames{$1}++;
	}
	else {
	    warn "WARNING: your minidb file [$1] failed to be registered, or I have screwed up parsing this line:\n\t[$op]";
	    push(@warnings, "WARNING: your minidb file [$1] failed to be registered, or I have screwed up parsing this line:\n\t[$op]\n");
	}
    }
    $cmsearchTerminalCount++ if ($op=~/\/\//);
}
close(lOP);
close(pOP);

if(($cmsearchTerminalCount<$cmjobcount or $cmsearchTerminalCount<$numminidbs) && $round==0){
    warn "WARNING: the counts do not agree! cmsearchTerminalCount=[$cmsearchTerminalCount], cmjobcount=[$cmjobcount], numminidbs=[$numminidbs]. Cmsearch failures?";
    push(@warnings, "WARNING: the counts do not agree! cmsearchTerminalCount=[$cmsearchTerminalCount], cmjobcount=[$cmjobcount], numminidbs=[$numminidbs]. Cmsearch failures?\n");
}

foreach my $mdb (keys %minidbnames){
    if($minidbnames{$mdb}<2){
	warn "WARNING: cmsearch failed for [$mdb] minidbnames{mdb}=$minidbnames{$mdb}?";
	push(@warnings, "WARNING: cmsearch failed for [$mdb] minidbnames{mdb}=$minidbnames{$mdb}?\n");
    }
}
###########validation block ends
#system("cat $lustre/$$.OUTPUT.* > $pwd/OUTPUT")   and die "FATAL: cant concatenate output files on the farm\n[$!]";

#system("cat $lustre/$$.TABFILE.* > $pwd/TABFILE") and die "FATAL: cant concatenate tabfile files on the farm\n[$!]";
#Using glob because the above fails on SRP and friends:
my @tabFiles = glob("$lustre/$$.TABFILE.*");
unlink "$pwd/TABFILE" if -e "$pwd/TABFILE";
foreach my $tabFile (@tabFiles){
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
my $blasttime = $blastendtime-$initendtime;       my $blasttimeH    = RfamUtils::secs2human($blasttime);
my $minidbtime = $minidbendtime - $blastendtime;  my $minidbtimeH   = RfamUtils::secs2human($minidbtime);
my $cmsearchtime = $endtime - $minidbendtime;     my $cmsearchtimeH = RfamUtils::secs2human($cmsearchtime);

print( "##############\n" );
&printlog( "INIT walltime:     $inittime secs    \t$inittimeH" );
&printlog( "BLAST walltime:    $blasttime secs   \t$blasttimeH" );
&printlog( "MINIDB walltime:   $minidbtime secs  \t$minidbtimeH" );
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

foreach my $k (qw(calibration blast cmsearch rfsearchwall)){
    next if not defined $runTimes{$k};
    my $humanTime = RfamUtils::secs2human($runTimes{$k}); 
    &printlog( "CPU Times: $k\t$runTimes{$k} secs\t[$humanTime]" );
}
&printlog( "##############" );

$rfamTimes   = $schema->resultset('RfamTimes')
    ->update_or_create(%runTimes
		       ,);
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
		print DNEW "BM   cmbuild $buildopts -F CM SEED; cmcalibrate --mpi -s 1 CM\n";
	    }
	    else {
		print DNEW "BM   cmbuild  -F CM SEED; cmcalibrate --mpi -s 1 CM\n";
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

#computeNumberBlastJobsPerCpu: calculates the number of jobs for
#   running on a single node on the farm.  If an entry for this family
#   exists in the rfam_times DB then it will base the calculation on
#   the average runtime per CPU from the last run.
#   Otherwise it returns 1.

sub computeNumberBlastJobsPerCpu{
    my ($nobjobs, $acc) = @_;
    
    my $schema = DBI->connect("dbi:mysql:host=$Rfam::rdb_host;port=$Rfam::rdb_port;dbname=rfam_times",$Rfam::rdb_user,$Rfam::rdb_pass, {
	    PrintError => 1, #Explicitly turn on DBI warn() and die() error reporting. 
	    RaiseError => 1
	}    );
    
    # Query to search for the description of embl entries with the embl id
    my $query = qq(
           select blast
                   from rfam_times where rfam_acc=?;         
   );
    
    my $sth = $schema->prepare($query);
    $sth->execute($acc);
    
    my $timeTotal;
    my $res = $sth->fetchall_arrayref;
    foreach my $row (@$res){
	$timeTotal .= $row->[0];
    }
    $schema->disconnect;
    my $maxJobs = int($nobjobs/2 +1); #ceil function would be better
    my $minTime=300; #Desired time for each node
    if (defined $timeTotal && RfamUtils::isInteger($timeTotal) && $timeTotal>0){
	my $jobs = int($minTime*$nobjobs/$timeTotal); 
	$jobs = $maxJobs if $jobs>$maxJobs;
	$jobs = 1        if $jobs<1;
	
	return $jobs;
    }
    
    return 1;
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

######################################################################
##########
#make_lcmask_file: takes as input a stockholm formatted filename and an output filename. 
#                  Writes the sequences from the stockholm file to the output file in 
#                  fasta format. Gappy columns and low frequency nucleotides are masked.
#                  Also, too similar sequences are filtered.
sub make_lcmask_file {
my $stk_file = shift; # stockholm file to use
my $out_file = shift; # output file
my $list = shift;


if(!defined($out_file)){
    my @stk_file = split(/\./, $stk_file);
    my $out_file = pop(@stk_file); # 
    $out_file = $out_file . ".fa";
}

#Read sequences into hashes:
open( STK, "$stk_file" ) or die ("FATAL: Couldn't open $stk_file \n[$!]");
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*STK );
close(STK);
my @list = $seed->each_seq();

my $length = length($list[0]->seq);
my $noseqs = @list;
my @aofh_nuc_counts; #array of hashes to store nuc-counts.

foreach my $seqobj ( @list ) {
    
    my $seqname = $seqobj->id . "/" . $seqobj->start . "-" . $seqobj->end;
    
    push( @{ $list->{$seqobj->id} }, { 'start'  => $seqobj->start,
				       'end'    => $seqobj->end
	  } );
    
    
    my $seq = uc($seqobj->seq); #Must be uppercase,U->T!
    $seq =~ tr/U/T/;
    my @seq = split(//,$seq);
    for (my $i=0; $i<$length; $i++){
	if (RfamUtils::is_nucleotide($seq[$i])){
	    if (defined($aofh_nuc_counts[$i]{$seq[$i]})){
		$aofh_nuc_counts[$i]{$seq[$i]}++;
	    }
	    else {
		$aofh_nuc_counts[$i]{$seq[$i]}=1;
	    }
	}
    }
}

#Calculate the per site nucleotide  frequencies:
my @fract_nucs;
for (my $i=0; $i<$length; $i++){
    $fract_nucs[$i]=0;
    foreach my $n ( keys %{ $aofh_nuc_counts[$i] } ) {
	$fract_nucs[$i]+=$aofh_nuc_counts[$i]{$n};
	$aofh_nuc_counts[$i]{$n} = $aofh_nuc_counts[$i]{$n}/$noseqs;
    }
    $fract_nucs[$i]=$fract_nucs[$i]/$noseqs;
}

my @accepted_seqs;
open( OUT, ">$out_file" ) or die ("FATAL: Couldn't open $out_file \n[$!]");
foreach my $seqobj ( @list ) {

    my $seqname = $seqobj->id . "/" . $seqobj->start . "-" . $seqobj->end;
    my $seq = uc($seqobj->seq);#Must be uppercase!
    $seq =~ tr/U/T/;
    
    my $max_pid = 0;
    foreach my $aseq (@accepted_seqs){
	my $p = RfamUtils::pid($aseq, $seq);
	if ($max_pid < $p){
	    $max_pid = $p;
	}
    }
    
    if ($max_pid<$max_pid_thresh){
	push(@accepted_seqs, $seq);
	my @seq = split(//,$seq);
	printf OUT ">$seqname\n";
	for (my $i=0; $i<$length; $i++){
	    if (RfamUtils::is_nucleotide($seq[$i])){
		if (defined($aofh_nuc_counts[$i]{$seq[$i]}) && $aofh_nuc_counts[$i]{$seq[$i]}>$nuc_freq_thresh && $fract_nucs[$i]>$gap_thresh){
		    printf OUT "$seq[$i]";
		}
		else {
		    $seq[$i] = lc($seq[$i]);
		    printf OUT "$seq[$i]";
		}
	    }
	}
	printf OUT "\n";
    }
}
close(OUT);

return 1;

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
Options:       --h                  show this help
	       
	       make_lcmask_file options
	       --gapthresh <num>      For the blast-filters, soft-mask sites in a sequence in columns with gap-freq > num. [Default = $gap_thresh].
	       --nucfreqthresh <num>  For the blast-filters, soft-mask sites in a sequence in columns with nuc-freq < num. [Default = $nuc_freq_thresh].
	       --maxpidthresh <num>   For the blast-filters, only search with sequences with max-pid < num, (max-pid is computed relative 
				      to sequences that\47ve already been searched.) [Default = $max_pid_thresh].

	       BLAST OPTIONS:
	       --nf|--nofilters      Skip the blast filters. This option still needs some work - it should use \42--hmmonly\42 
	                             to build minidbs - then to the full cmsearches. 
	       --e <n>               use blast evalue of <n> [Defaults: blast_eval_sens=$blast_eval_sens/blast_eval_spec=$blast_eval_spec]
	       --wublastcpus <n>     number of cpus to run a single wublast job over [Default: wublastcpus=$wublastcpus]
	       --nolowcomplexityseg  turn off WU-BLAST low complexity filter seq
	       --nolowcomplexitydust turn off WU-BLAST low complexity filter dust
	       --pname <str>         give lsf a process name, "str", for the cmsearch jobs
	       --altdb <YOURDIR>     Give a directory name in \42$Rfam::rfamseq_current_dir\42 containing xdformat\47ted blastdbs. Use this INSTEAD of the normal RFAMSEQ DBs.
	                             (xdformat -n  -o blastdb.fa -I blastdb.fa). Remember to: 
	                             \42scp -r $Rfam::rfamseq_current_dir/YOURDIR farm2-login:$Rfam::rfamseq_run_dir/\42 before running.
				     And make an $Rfam::rfamseq_current_dir/YOURDIR/DBSIZE file.
	       --incldb <YOURDIR>    Give a directory name in \42$Rfam::rfamseq_current_dir\42 containing xdformat\47ted blastdbs. Use this AS WELL AS the normal RFAMSEQ DBs.
	                             (xdformat -n  -o blastdb.fa -I blastdb.fa). Remember to: 
	                             \42scp -r $Rfam::rfamseq_current_dir/YOURDIR farm2-login:$Rfam::rfamseq_run_dir/\42 before running.
				     And make an $Rfam::rfamseq_current_dir/YOURDIR/DBSIZE file.
				     
	       MINIDB OPTIONS
	       -m|--minidbpname <str>        Restart a job with pname \42str\42 from after the minidb creation (also works with -m, -mini or -minidb)
	       -minseqs4cmsearchlimits <num> Set a minimum number of sequences for adding to the minidbs [Default: $minseqs4cmsearchlimits]
	       -maxseqs4cmsearchlimits <num> Set a maximum number of sequences for adding to the minidbs [Default: $maxseqs4cmsearchlimits]
	       -nomerge                      Do not merge the small minidb\47s
	       
	       EARLY EXIT STRATEGIES
	       -obm|--onlybuildminis     Build minidbs and stop
	       --blastonly <opt:str>     Run the BLAST jobs, build and merge the minidbs and stop. Optionally takes a fasta file as input.
	       
	       INFERNAL/CM OPTIONS:
	       --nobuild                     Skip the cmbuild step
	       -g|--glocal                   Run cmsearch in glocal mode (override DESC cmsearch command)
	       --window <str>                Use this window size for fetching blast sequences rather than from CM.
	       --hmmonly|--long              An option for long models (eg. SSU/LSU rRNA,...),
	                                     This runs "cmsearch -hmmfilter", requires infernal version >0.7 
	       -cal|--calibrate              Calibrate model and exit
	       -fcal|--forcecalibrate        Force re-calibrating the model
	       -cme|--cmsearchevalue   <num> Set an evalue threshold for cmsearch [Default: $cmsearch_eval]
	       -cmos|--cmsearchoptions <str> Add extra arbitrary options to cmsearch with a single '-'. For multiple options use multiple 
	                                     -cmos lines. Eg. '-cmos g -cmos x' will run cmsearch in global mode and annotate 
					     non-compensatory bps in output alignments with 'x'.
	       -cmod|--cmsearchoptiond <str> Add extra arbitrary options to cmsearch with a double '-'. For multiple options use multiple 
	                                     -cmod lines. Eg. '-cmod fil-no-hmm' will run cmsearch without filtering usin HMM Forward 
					     algorithm.
	       
		CLEANUP
		--dirty            Leave the files on the cluster. 
		
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

