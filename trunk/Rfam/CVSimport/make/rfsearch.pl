#!/software/bin/perl -w

# 

use strict;
use Getopt::Long;
use IO::File;
use Data::Dumper; #Great for printing diverse data-structures.
use Rfam;
use Rfam::RfamAlign;
use SeqFetch;
use Sys::Hostname;
use Cwd;

#BLOCK 0: INITIALISE: 

my $starttime = time();

my( 
    $nobuild, 
    $local,
    $global,
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
    $dirty,
    $onlybuildminis,
    $nomerge,
    @warnings
    );

#BLAST/MINIDB defaults
$wublastcpus = 1; #Number of CPUs wu-blast will run on "-cpu N". 
$blast_eval_sens = 100;
$blast_eval_spec = 10;
$maxseqs4cmsearchlimits=2000;
$minseqs4cmsearchlimits=20;

#lcmask defaults:
$gap_thresh = 0.5;      #Gap threshold for lcmask
$nuc_freq_thresh = 0.1; #Nucleotide frequency threshold for lcmask
$max_pid_thresh = 0.95; 

&GetOptions( 
             "e=s"                 => \$blast_eval,
	     "local"               => \$local,
	     "global"              => \$global,
             "bo|blastonly:s"      => \$blastonly,
	     "wublastcpus=s"       => \$wublastcpus,
	     "nolowcomplexityseg"  => \$nolowcomplexityseg,
	     "nolowcomplexitydust" => \$nolowcomplexitydust,
	     "nobuild"             => \$nobuild,
	     "pname=s"             => \$pname,
             "altdb=s"             => \$altdb,
	     "m|mini|minidb|minidbpname=s" => \$minidbpname,
             "obm|onlybuildminis" => \$onlybuildminis,
	     "nf|nofilters"  => \$nofilters,
	     "nolcmask"      => \$nolcmask,
	     "long|hmmonly"  => \$hmmonly,
	     "gapthresh=s"     => \$gap_thresh,
	     "nucfreqthresh=s" => \$nuc_freq_thresh,
	     "maxpidthresh=s"  => \$max_pid_thresh,
	     "window"          => \$window,
             "maxseqs4cmsearchlimits"          => \$maxseqs4cmsearchlimits,
             "minseqs4cmsearchlimits"          => \$minseqs4cmsearchlimits,
             "dirty"           => \$dirty,
             "nomerge"         => \$nomerge,
	     "h"               => \$help 
    );

if( $help ) {
    &help();
    exit(1);
}

# make sure writable by group
umask(002);
#umask(000); #givese everyone write access

my $user = `whoami`;
chomp($user);

#Check for the existence and correct permissions of essential files:
if (-e "rfsearch.log" && -w "rfsearch.log"){
    unlink("rfsearch.log");
}
elsif (-e "rfsearch.log") {
    die("FATAL: check permissions on rfsearch.log");
}

#Set the stripe pattern on the lustre (farm) file system:
my $queue2     = 'small -R \"select[type==X86_64]\"';
my $lustre = "/lustre/scratch1/sanger/rfam/$$"; #path for dumping data to on the farm
my $fh0 = new IO::File;
$fh0 -> open("| bsub -I -q $queue2") or die "FATAL: bsub -I -q $queue2\n$!";
&printlog( "Making lustre run directory on the farm: $lustre" );
&printlog( "mkdir -p $lustre" );
$fh0 -> print("mkdir -p $lustre\n") or die "FATAL: \42mkdir -p $lustre\42 on the farm failed\n$!";
&printlog( "lfs setstripe $lustre 0 4 4" );
$fh0 -> print("lfs setstripe $lustre 0 4 4\n") or die "FATAL: \42lfs setstripe $lustre 0 4 4\42 on the farm failed\n$!"; #lsrun -m farm-login lfs setstripe $lustre 0 4 4
$fh0 -> close;
#See the wiki page "LustreStripeSize" for more detail. 

#Read cmbuild/cmsearch flags from the DESC file:
&printlog( "Read cmbuild/cmsearch flags from the DESC file" );
my $buildopts;
if( -s "DESC" ) {
    open( D, "DESC" ) or die "DESC exists but can't be opened";
    while( <D> ) {
	/^BM\s+cmbuild\s+(.*)\s+CM\s+SEED$/ and do {
	    $buildopts = $1;
	};
	/^BM\s+cmsearch.*-local/ and do {
	    unless( $global ) {
		$local = 1;
		&printlog( "Using --local mode as specified in DESC file" );
	    }
	};
    }
}

my $pwd = getcwd;
my $phost = hostname;

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

unless( $nobuild || $blastonly ) {
    if (-w "CM" or not -e "CM"){
	&printlog( "Building model: cmbuild -F $buildopts CM SEED" );
	system "cmbuild -F $buildopts CM SEED" and die "can't build CM from SEED";
    }
    elsif (-e "CM") {
	die("FATAL: CM file exists but you don't have write access");
    }
}

##get the cmsearch W value from the CM file.
&printlog( "get the cmsearch W value from the CM file" );
my $cmwindow = 0;
if (-e "$pwd/CM"){
    my $string = `grep \"^W\" $pwd/CM`; #GREP! HAHAHAHA!!! ;-)
    my @string = split('\s+', $string);
    $cmwindow = $string[1];
}
elsif (!(-e $blastonly)) {
    die ("The $pwd/CM file is missing! SEED file exists, check cmbuild ran. Check directory is mounted."); 
}

#Moderately paranoid checking of the all important window parameter:
if ( (int($cmwindow) == $cmwindow) && $cmwindow>0 && !(defined($window) && (int($window) == $window)) ){
    $window=$cmwindow; 
    &printlog( "Using window [$window] from CM file" );
}
elsif (defined(($window)) && (int($window) == $window) && $window>0){
    &printlog( "Using window [$window] from user input" );
}
else {
    $window=200;
    push(@warnings, "WARNING: essential window parameter not given, using a dodgy default: $window\n");
    &printlog( "WARNING: essential window parameter not given, using a dodgy default: $window" );
}

if( -e "CMSEARCH_JOBS_COMPLETE" ) {
    unlink( "CMSEARCH_JOBS_COMPLETE" ) or die "can't remove file [CMSEARCH_JOBS_COMPLETE]\n";
}

my $initendtime = time();

#BLOCK 1: RUN BLAST
&printlog( "" );
&printlog( "Starting rfsearch" );
my $blastdbdir  = $Rfam::rfamseq_current_dir;  # glob files from here
my $blastdbdir2 = $Rfam::rfamseq_run_dir;      # but run things from here

if (defined($altdb)){
    $blastdbdir .= "/" . $altdb;
    $blastdbdir2 .= "/" . $altdb;
}

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

my @blastdb = glob( "$blastdbdir/*.fa.xnd" );
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
my( $dbsize);
open( DBSIZE, "$blastdbdir/DBSIZE" ) or die "FATAL: $blastdbdir/DBSIZE file doesn't exist\n";
while(<DBSIZE>) {
    if( /(\d+)/ ) {
        $dbsize = $1;
        last;
    }
}
close DBSIZE;

#user must have log dir!
&printlog( "Making log directory: $pwd/$$" );
umask(002);
mkdir( "$pwd/$$", 0775 ) or  die "cant create the dir for error logs" ;
my $nobjobs = scalar(@blastdb);

my @index = ( 1..$nobjobs );
my $round = 0;
#things to copy to lustre file system
system("/usr/bin/scp $pwd/$fafile farm-login:$lustre/$fafile") and die "/usr/bin/scp $pwd/$fafile farm-login:$lustre/$fafile\nerror scp-ing $pwd/$fafile to farm-login:$lustre/$fafile\n$!\n";

unless( $minidbpname || $nofilters ) {
  &printlog( "Running $nobjobs blast jobs" );
  BLAST: { #JT6 thinks this GOTO is evil - but then he thinks "needs fixed" is evil too. 
      if( $round or !$blastpname ) {
	  &printlog( "Queuing up blast jobs [round ".$round."]" );
	  $blastpname = $$ if( !$blastpname );
	  foreach my $i ( @index ) { #This is cleverer than it looks, see below for rerunning failed jobs:
	      my $blastdb = $blastdb[$i-1];
	      $blastdb =~ s/\.xnd$//g;
	      $blastdb =~ s/$blastdbdir/$blastdbdir2/g;
	      my $blastcmd = "";
	      my $blastcmdsens = "";
	      my $blastcmdspec = "";
	      #PPG: WU-BLAST is now the default. MYAHAHAHAHA!:
	      $blastcmdsens = "wublastn $blastdb $lustre/$fafile W=7 B=100000 V=100000 E=$blast_eval_sens mformat=3 hspsepSmax=$window -lcmask cpus=$wublastcpus Z=$dbsize $blastfiltersstring"; 
	      $blastcmdspec = "wublastn $blastdb $lustre/$fafile W=7 B=100000 V=100000 E=$blast_eval_spec mformat=3 hspsepSmax=$window -lcmask cpus=$wublastcpus Z=$dbsize M=1 N=-3 Q=3 R=3  $blastfiltersstring";

	      my $fh = new IO::File;
	      $fh -> open("| bsub -q $queue -J\"rf$blastpname\" -o $pwd/$$/$blastpname\.berr.$i\n") or die "$!";
	      $fh -> print("$blastcmdspec >  $lustre/$blastpname\.blastout.$i;\n");
	      $fh -> print("$blastcmdsens >> $lustre/$blastpname\.blastout.$i;\n");
	      $fh -> print("wublast2minidb.pl -d $blastdb -w $window -l $maxseqs4cmsearchlimits -f $lustre/$blastpname\.blastout.$i -o $lustre/$blastpname\.minidb.$i;\n");
	      $fh -> print("[ -f $lustre/$blastpname\.blastout.$i\.error ] && /usr/bin/scp farm-login:$lustre/$blastpname\.blastout.$i\.error $phost:$pwd/;\n") or die "error scp-ing farm-login:$lustre/$blastpname\.blastout.$i\.error to $phost:$pwd/\n$!\n"; 
	      $fh -> print("[ -f $lustre/$blastpname\.blastout.$i\.error ] && rm -f $lustre/$blastpname\.blastout.$i.error\n");
	      $fh -> close;
	  }
	  
	  &printlog( "Waiting for blast jobs" );
	  wait_for_farm("rf$blastpname", "blast", $nobjobs);
	  
      }
      &printlog( "checking blast result integrity" );
      
      my @rerun = ();
      
      foreach my $ii ( @index ) {
	  
	  if (-e "$blastpname\.blastout.$ii.error"){
	      &printlog( "Blast job didn't finish [$blastpname\.blastout.$ii] -- rerunning blast search." );
	      push( @rerun, $ii );
	      system("rm $blastpname\.blastout.$ii.error");
	  }
      }

      if( @rerun ) {
	  $round ++;
	  if( $round > 3 ) {
	      &printlog( "FATAL: Maximum number of Blast failures" );
	      die;
	  }
	  if (scalar(@rerun) == $nobjobs){
	      &printlog( "FATAL: all the bsub jobs need rerunning! Indicates farm is fucked up! Or your ssh keys to and from the farm have not been generated! Or the bsub paramaters are screwed! Or ..." );
	      die;
	  }
	  else {
	      @index = @rerun;
	      my $rerun = scalar(@rerun);
	      push(@warnings, "WARNING: rerunning $rerun blast jobs\n");

	  }
	  redo BLAST;
      }
  }

##
}


my $blastendtime = time();

#BLOCK 2: BUILD MINIDBs.
#############

my @minidbnames = ();
my $numminidbs = 0;  # number of minidb files. 
if( $minidbpname ) {
    @minidbnames = glob( "$minidbpname\.minidb.*" );
    if( @minidbnames ) {
	foreach my $minidbname (@minidbnames){
	    #This loop is required - can be too many files for a full glob:
	    system("scp $pwd/$minidbname farm-login:$lustre/") and die "error scp-ing mini db:$minidbname to farm-login:$lustre/\n$!";
	}
    }
    else {
	&printlog( "FATAL: no minidb files [$minidbpname\.minidb.*]" );
	die;
    }
}
elsif (!$nofilters && !$minidbpname) {


    my $fh01 = new IO::File;
    $fh01 -> open("| bsub -I -q $queue2") or die "FATAL: bsub -I -q $queue2\n$!";
    $fh01 -> print("ls -1 $lustre/$blastpname\.minidb\.* > $lustre/$blastpname\.filelist ; \n") or die "FATAL: \47ls -1 $lustre/$blastpname\.minidb\.* > $lustre/$blastpname\.filelist\47 failed\n$!";
    $fh01 -> close;
    
    system("scp farm-login:$lustre/$blastpname\.filelist $pwd/") and die "error scp-ing minidb filelist farm-login:$lustre/$blastpname\.filelist to $pwd/\n$!";
    
    open( FL, "$blastpname\.filelist" ) or die "$blastpname\.filelist failed to open";
    @minidbnames = <FL>;
    close(FL);
    
    $minidbpname = $blastpname;
#    @minidbnames = glob( "$minidbpname\.minidb.*" );
    my (@toosmalldbname, @toosmallnumseqs, @numseqs, $minlargeminidbname);
    my $minlargeminidbnumseqs=0;
    #Gather info on minidbs: 
    foreach my $mdb (@minidbnames){
	
	chomp($mdb);
	system("scp farm-login:$mdb $pwd/") and warn "error scp-ing mini db farm-login:$mdb to $pwd/\n$!";
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
		system("cat $dbname1 $dbname2 > $newdbname");
		system("rm $dbname1 $dbname2");
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
	    system("scp $mdb farm-login:$lustre/") and die "Failed to copy $mdb to lustre file system\n";  
	}
    }
}

$numminidbs = scalar(@minidbnames);
&printlog( "Submit $numminidbs minidbs.");

my $minidbendtime = time();

#BLOCK 3: RUN CMSEARCH:
my $command = "cmsearch";
my $options = "  "; #
$options .= " --local "   if( $local );
$options .= " --hmmonly " if( $hmmonly );
$options .= " --toponly " if( !$nofilters );

$pname = "cm$$" if( not $pname );
system "cp CM $$.CM" and die;

&printlog( "Queueing cmsearch jobs" );

my $fhcm = IO::File->new();

system("scp $pwd/$$.CM farm-login:$lustre/$$.CM") and die "error scp-ing $pwd/$$.CM to farm-login:$lustre/$$.CM\n$!\n";

&printlog( "" );

#my $queue = "long -n$wublastcpus -R \"span[hosts=1] && select[type==X86_64]\"";
my $queuecm      = 'long -R \"select[type=X86_64]\"';
if ($nofilters){
    my @seqdb = glob( "$blastdbdir/*.fa.gz" );
    $numminidbs=0;
    foreach my $sdb (@seqdb) {
	$sdb =~ s/\.gz$//g;
	$sdb =~ s/$blastdbdir/$blastdbdir2/g;

#	&printlog( "Running: | bsub -q $queuecm -o $pwd/$$/$$.err.\%I -J$pname" );
	$fhcm -> open( "| bsub -q $queuecm -o $pwd/$$/$$.err.\%I  -J$pname" ) or die "$!";
#	&printlog( "Running: $command $options $lustre/$$.CM $sdb > $lustre/$$.OUTPUT.$numminidbs");
	$fhcm -> print( "$command $options $lustre/$$.CM $sdb > $lustre/$$.OUTPUT.$numminidbs\n" );
	$fhcm -> close;
	$numminidbs++;
    }    
}
else {
    my $i=0;
 #   &printlog( "Submitting: @minidbnames to the farm.");
    foreach my $minidb ( @minidbnames ) { 
#	&printlog( "Running: $command $options $lustre/$$.CM $lustre/$minidb > $lustre/$$.OUTPUT.$i");
	my $fhcm = new IO::File;
	$fhcm -> open("| bsub -q $queue -J$pname -o $pwd/$$/$$.cmsearch.err.$i") or die "$!";
	$fhcm -> print( "$command $options $lustre/$$.CM $lustre/$minidb > $lustre/$$.OUTPUT.$i\n" );
	$fhcm -> close;
	$i++;
    }

#    $fhcm -> open( "| bsub -q $queuecm -o $pwd/$$/$$.err.\%I -J$pname\"[1-$numminidbs]\"" ) or die "$!";
#    $fhcm -> print( "$command $options $lustre/$$.CM $lustre/$minidbpname\.minidb.\$\{LSB_JOBINDEX\} > $lustre/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n" );
#    $fhcm -> close;
}

my $cmopts=$options . "";
&printlog( "Waiting for cmsearch jobs." );
wait_for_farm($pname, "cmsearch", $numminidbs);

# send something to clean up
print STDERR "set cm searches running, copy files and clean up....\n";
#umask(002);
$fhcm = new IO::File;
#&printlog( "bsub -q $queue2 -w\'done($pname)\'");
&printlog( "");
&printlog( "bsub -I -q $queue2");
&printlog( "cat $lustre/$$.OUTPUT.* > $lustre/$$.OUTPUT_full");
&printlog( "/usr/bin/scp $lustre/$$.OUTPUT_full  $phost:$pwd/OUTPUT");
&printlog( "");

$fhcm -> open("| bsub -I -q $queue2") or die "FATAL: bsub -I -q $queue2\n$!";
#$fhcm -> open("| bsub -q $queue2 -w\'done($pname)\'") or die "FATAL: bsub -q $queue2 -w\'done($pname)\'\n[$!]";
#$fhcm -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find scp
                   #$lustre/$$.OUTPUT.
$fhcm -> print("cat $lustre/$$.OUTPUT.* > $lustre/$$.OUTPUT_full\n")  or  die "cant concatenate output files on the farm\n$!\n";
$fhcm -> print("/usr/bin/scp $lustre/$$.OUTPUT_full  $phost:$pwd/OUTPUT\n") or die "cant copy $lustre/$$.OUTPUT_full to $phost:$pwd/OUTPUT\n$!\n";
$fhcm -> print("date >> /tmp/$$.cmerr\n");
$fhcm -> print("/usr/bin/scp /tmp/$$.cmerr $phost:$pwd/CMSEARCH_JOBS_COMPLETE\n") or die "cant copy /tmp/$$.cmerr to $phost:$pwd/CMSEARCH_JOBS_COMPLETE\n$!\n";
$fhcm -> print("rm -rf /tmp/$$.cmerr\n");
if (!defined($dirty)){
    $fhcm -> print("rm -rf $lustre\n") or die "failed to clean up files on the farm\n";
}
$fhcm -> close;

&update_desc( $buildopts, $cmopts ) unless( !-e "DESC" );
print STDERR "finished cleaning up... wait for cm searches to complete\n ";

&printlog( "" );
&printlog( "FINISHED! See OUTPUT file." );

my $endtime = time();
my $runtime = $endtime - $starttime;
my $inittime = $initendtime - $starttime;
my $blasttime = $blastendtime-$initendtime;
my $minidbtime = $minidbendtime - $blastendtime;
my $cmsearchtime = $endtime - $minidbendtime;

&printlog( "" );
&printlog( "INIT Runtime:     $inittime secs" );
&printlog( "BLAST Runtime:    $blasttime secs" );
&printlog( "MINIDB Runtime:   $minidbtime secs" );
&printlog( "CMSEARCH Runtime: $cmsearchtime secs" );
&printlog( "Total Runtime:    $runtime secs" );

if (scalar(@warnings)){
    print "There were " . scalar(@warnings) . " warnings:\n";
    foreach my $w (@warnings) {
	print $w;
    }
}

##############

sub printlog {
    my $m = join( '', @_ );
    #my $time = `date`;
    my  $time = localtime();
    #chomp $time;
    open( LOG, ">>rfsearch.log" ) or die;
    if( $m ) {
        printf LOG    "%-40s [%s]\n", $m, $time;
        printf STDERR "%-40s [%s]\n", $m, $time;
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
		print DNEW "BM   cmbuild $buildopts CM SEED\n";
	    }
	    else {
		print DNEW "BM   cmbuild CM SEED\n";
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
##########
#make_lcmask_file: takes as input a stockholm formatted filename and an output filename. 
#                  Writes the sequences from the stockholm file to the output file in 
#                  fasta format. Gappy columns and low frequency nucleotides are masked.
#                  Also, too similar sequences are filtered.
sub make_lcmask_file {
my $stk_file = shift; # stockholm file to use
my $out_file = shift; # output file
my $list = shift;

#my $gap_thresh = 0.5;      #Gap threshold for lcmask
#my $nuc_freq_thresh = 0.1; #Nucleotide frequency threshold for lcmask
#my $max_pid_thresh = 0.95; 


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
	if (is_nucleotide($seq[$i])){
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
	my $p = pid($aseq, $seq);
	if ($max_pid < $p){
	    $max_pid = $p;
	}
    }
    
    if ($max_pid<$max_pid_thresh){
	push(@accepted_seqs, $seq);
	my @seq = split(//,$seq);
	printf OUT ">$seqname\n";
	for (my $i=0; $i<$length; $i++){
	    if (is_nucleotide($seq[$i])){
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
#returns true if input character is a nucleotide (IUPAC codes):
sub is_nucleotide {
    my $a = shift;
    
    if (defined($a)){
	$a =~ tr/a-z/A-Z/;
    }
	
    if (defined($a) && length($a) && ($a =~ /[ACGUTRYWSMKBDHVN]/) ){
	return 1;
    }
    else {
	return 0;
    }
    
}

#
######################################################################
#function sits checking for status of jobs on the farm - returns true when the job is finished:
#eg.
#wait_for_farm("rf$blastpname", "blast")
sub wait_for_farm {
    
    my $bjobname = shift;
    my $jobtype  = shift;
    my $nobjobs  = shift;
    my $wait = 1;
    my $bjobcount = 1;
    my $bjobinterval = 15;
    my $jobs = $nobjobs;
    while($wait){
	
	sleep($bjobinterval); 
	
	$jobs = 0;
	my ($bjpend, $bjrun)  = (0, 0);
	open(S, "bjobs -J $bjobname |") or die;
	while(<S>) {
	    #if(/$bjobname/){
		#$jobs++;
	    #}
	    
	    if (/PEND/){
		$bjpend++;
	    }
	    
	    if (/RUN/){
		$bjrun++;
	    }
	}
	close(S);# || die "failed to close pipe on bjobs:[$!]\n"; <- this caused a "die" once all the jobs were finished!
	$jobs = $bjpend + $bjrun;
	
	if ($jobs < int($nobjobs*(1-0.95)) ){#Once 95% of jobs are finished, check frequently.
	    $bjobinterval=15;
	}	      
	elsif ($jobs < int($nobjobs*(1-0.90)) ){#Once 90% of jobs are finished, check a little more frequently.
	    $bjobinterval=15 + int(log($bjobcount/2)); 
	}
	else {#otherwise check less & less frequently (max. interval is ~300 secs = 5 mins).
	    if ($bjobinterval<600){ $bjobinterval = $bjobinterval + int(log($bjobcount));}
	}
	
	if($jobs){
	    print STDERR "There are $jobs $jobtype jobs of $nobjobs still running after $bjobcount checks (PEND:$bjpend,RUN:$bjrun). Check interval:$bjobinterval secs.\n"; 
	}else{
	    $wait = 0;
	}
	$bjobcount++;
    }
    
    return 1;
    
}

#pid: compute the identity between two sequences.
sub pid {
    my $a = shift;
    my $b = shift;
    
    my @a = split(//, $a);
    my @b = split(//, $b);
    my ($sim, $lena, $lenb) = (0, 0, 0);
    
    if (scalar(@a) != scalar(@b)){
	return 0;
    }
    else {
	
 	for (my $i=0; $i<scalar(@b); $i++){
	    if ( (is_nucleotide($a[$i]) || is_nucleotide($b[$i])) && $a[$i] eq $b[$i] ){
		$sim++;
	    }
	    
	    if ( is_nucleotide($a[$i]) ){
		$lena++;
	    }

	    if ( is_nucleotide($b[$i]) ){
		$lenb++;
	    }
	}
    }
    
    my $maxlen = max($lena, $lenb);
    if ($maxlen>0){
	return $sim/$maxlen;
    }
    else {
	return 0;
    }
}

#max
sub max {
  return $_[0] if @_ == 1;
  $_[0] > $_[1] ? $_[0] : $_[1]
}

#min
sub min {
  return $_[0] if @_ == 1;
  $_[0] < $_[1] ? $_[0] : $_[1]
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
	       --altdb <YOURDIR>     Give a directory name in \42$Rfam::rfamseq_current_dir\42 containing xdformat\47ted blastdbs 
	                             (xdformat -n  -o blastdb.fa -I blastdb.fa). Remember to: 
	                             \42scp -r $Rfam::rfamseq_current_dir/YOURDIR farm-login:$Rfam::rfamseq_run_dir/\42 before running.
				     
	       MINIDB OPTIONS
	       -m|--minidbpname <str>    restart a job with pname \42str\42 from after the minidb creation (also works with -m, -mini or -minidb)
	       -minseqs4cmsearchlimits   Set a minimum number of sequences for adding to the minidbs [Default: $minseqs4cmsearchlimits]
	       -maxseqs4cmsearchlimits   Set a maximum number of sequences for adding to the minidbs [Default: $maxseqs4cmsearchlimits]
	       -nomerge                  Do not merge the small minidb\47s
	       
	       EARLY EXIT STRATEGIES
	       -obm|--onlybuildminis     Build minidbs and stop
	       --blastonly <opt:str>     Run the BLAST jobs, build and merge the minidbs and stop. Optionally takes a fasta file as input.
	       
	       INFERNAL/CM OPTIONS:
	       --nobuild           skip cmbuild step
	       --local             run cmsearch with --local option
	       --global            run cmsearch in global mode (override DESC cmsearch command) [Default]
	       --window <str>      Use this window size for fetching blast sequences rather than from CM.
	       --hmmonly|--long    an option for long models (eg. SSU/LSU rRNA,...),
	                           This runs "cmsearch -hmmfilter", requires infernal version >0.7 
				   
		CLEANUP
		--dirty            Leave the files on the cluster. 
		
TO ADD:
Alternative filters: fasta, hmmsearch, ...
Add a cmsensitive option - using indels \& local
Run and store both global \& glocal modes. 
Add a check that all the cmsearch jobs completed!

EOF
}

