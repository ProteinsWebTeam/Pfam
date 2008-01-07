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

my( $quiet, 
    $nobuild, 
    $local,
    $global,
    $pname,
    $blast_eval,
    $blast_eval_sens,
    $blast_eval_spec,
    $blastpname,
    $help,
    $update,
    $minidbpname,
    $nolcmask,
    $queue,
    $window,
    $cpus,
    $wublastcpus,
    $nofilters,
    $hmmonly
    );

sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches a covariance model against a sequence database.
             Run from within a directory containing "SEED" & "DESC" files. 
	     Eg, after running "rfupdate RFXXXXX" or "rfco RFXXXXX".
	     SEED contains a stockholm format alignment and DESC is an internal 
	     Rfam documentation describing each RNA family. 

Usage:   rfsearch.pl <options>
Options:       -h                  show this help
	       -e <n>              use blast evalue of <n>
               -q <queue>          use lsf queue <queue> for the cmsearch step
	       --local             run cmsearch with --local option
	       --global            run cmsearch in global mode (override DESC cmsearch command)
	       --cpus <n>          number of cpus to run cmsearch job over
	       --wublastcpus <n>   number of cpus to run a single wublast job over
	       --nobuild           skip cmbuild step
	       --pname <str>       give lsf a process name, "str", for the cmsearch jobs
	       --blastpname <str>  restart a job with pname "str" from after the blast runs (also works with -b or -blast)
	       --minidbpname <str> restart a job with pname "str" from after the minidb creation (also works with -m, -mini or -minidb)
	       --update            update the family to a new underlying sequence db (expert!)
	       --nowu              use ncbi-blast rather than wu-blast (default).
	       --long              [TO BE IMPLEMENTED] 
	                           
	       --window <str>      Use this window size for fetching blast sequences rather than from CM.
	       --nofilters         Skip the blast filters.
	       --hmmonly/--long    an option for long models (eg. SSU/LSU rRNA,...),
	                           This runs "cmsearch -hmmfilter", requires infernal version >0.81. 
TO ADD:
	An option to skip BLAST filters
	

EOF
}

#ADD A VERBOSE OPTION!!!
&GetOptions( "e=s"           => \$blast_eval,
	     "q=s"           => \$queue,
	    # "bq=s"          => \$bqueue,
	     "local"         => \$local,
	     "global"        => \$global,
	     "cpus=s"        => \$cpus,
	     "wublastcpus=s" => \$wublastcpus,
	     "nobuild"       => \$nobuild,
	     "pname=s"       => \$pname,
	     "b|blast|blastpname=s"  => \$blastpname,
	     "m|mini|minidb|minidbpname=s" => \$minidbpname,
	     "nf|nofilters"  => \$nofilters,
	     "update"        => \$update,
	     "nolcmask"      => \$nolcmask,
	     "long|hmmonly"  => \$hmmonly,
	     "window"        => \$window,
	     "h"             => \$help );

if( $help ) {
    &help();
    exit(1);
}

# make sure writable by group
umask(002);
my $user = `whoami`;
chomp($user);

#Check for the existence and correct permissions of essential files:
if (-e "rfsearch.log" && -w "rfsearch.log"){
    unlink("rfsearch.log");
}
elsif (-e "rfsearch.log") {
    die("FATAL: check permissions on rfsearch.log");
}

if (not -e "SEED"){
    die("FATAL: check SEED exists");
}

#Set the stripe pattern on the lustre file system:
my $queue2     = 'small -R \"select[type==X86_64]\"';
my $lustre = "/lustre/scratch1/sanger/rfam/$$"; #path for dumping data to on the farm
my $fh0 = new IO::File;
$fh0 -> open("| bsub -I -q $queue2") or die "FATAL: bsub -I -q $queue2\n$!";
&printlog( "Making lustre run directory on the farm: $lustre" );
&printlog( "mkdir -p $lustre" );
$fh0 -> print("mkdir -p $lustre\n") or die "FATAL: lsrun -m farm-login mkdir -p $lustre\n$!";
&printlog( "lfs setstripe $lustre 0 4 4" );
$fh0 -> print("lfs setstripe $lustre 0 4 4\n") or die "FATAL: lsrun -m farm-login lfs setstripe $lustre 0 4 4\n$!";
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
open( S, "SEED" ) or die("SEED exists but couldn't be opened!");
my $seenrf;
while(<S>) {
    if( /^\#=GC RF/ ) {
	$seenrf = 1;
	last;
    }
}

if( $seenrf and $buildopts !~ /--rf/ ) {
    $buildopts = "--rf $buildopts";
}   
elsif( !$seenrf and $buildopts =~ /--rf/ ) {
    $buildopts =~ s/--rf //g;
}

unless( $nobuild ) {
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
else {
    die ("The $pwd/CM file is missing! SEED file exists, check cmbuild ran."); 
}

#Moderately paranoid checking of the all important window parameter:
if ( (int($cmwindow) == $cmwindow) && $cmwindow>0 && !(defined($window) && (int($window) == $window)) ){
    $window=$cmwindow; #convert string so we can use it
    &printlog( "Using window [$window] from CM file" );
}
elsif (defined(($window)) && (int($window) == $window) && $window>0){
    &printlog( "Using window [$window] from user input" );
}
else {
    die( "Failed to read window parameter from CM or the command line!" );     
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
my $fafile = "$$.fa";
if(!defined($wublastcpus)){
    $wublastcpus = 2; #Number of CPUs wu-blast will run on "-cpu N". 
}

if(!defined($blast_eval)){
    $blast_eval_sens = 100;
    $blast_eval_spec = 10;
}
else {
    $blast_eval_sens = $blast_eval;
    $blast_eval_spec = $blast_eval;
}

if (!defined($queue)){
    $queue      = "long -n$wublastcpus -R \"span[hosts=1] && select[type==X86_64]\"";
}

if ($nolcmask) {
    system "sreformat -u fasta SEED > $fafile" and die "can't convert SEED to $fafile";
}
else {
    make_lcmask_file("SEED", $fafile);
}

my @blastdb;
if( $update ) {      # run over the new.fa.* databases
    @blastdb = glob( "$blastdbdir/new.fa.*[0-9].xnd" );
    &update_output( "OUTPUT", "OUTPUT.0" );
}
else {               # run over *.fa databases
    @blastdb = glob( "$blastdbdir/*.fa.xnd" );
    my @tmparray;
    foreach my $bdb (@blastdb) {
	if ($bdb !~ /rfamseq.fa.xnd/){
	    push(@tmparray, $bdb);
	}
    }
    @blastdb = @tmparray;
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

#PPG: If there are going to be a stupid number of hits we should lower the threshold. 
#Sample ~3 sequences using "squid/weight -s 3" - run blast with default threshold. Extrapolate from this the 
#estimated total number of hits - choose a lower sensible threshold if this is greater than 100,000.

unless( $minidbpname || $nofilters ) {
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
	      $blastcmdsens = "wublastn $blastdb $lustre/$fafile W=7 B=100000 V=100000 E=$blast_eval_sens mformat=3 hspsepSmax=$window -lcmask cpus=$wublastcpus Z=$dbsize filter=seg filter=dust"; 
	      $blastcmdspec = "wublastn $blastdb $lustre/$fafile W=7 B=100000 V=100000 E=$blast_eval_spec mformat=3 hspsepSmax=$window -lcmask cpus=$wublastcpus Z=$dbsize M=1 N=-3 Q=3 R=3  filter=seg filter=dust";

  	      my( $div ) = $blastdb =~ /$blastdbdir\/(\S+)$/;
	      my $fh = new IO::File;
	      $fh -> open("| bsub -q $queue -J\"rf$blastpname\" -o $pwd/$$/$blastpname\.berr.$i") or die "$!";
	      $fh -> print(". /usr/local/lsf/conf/profile.lsf\n");       # so we can find lsrcp
	      $fh -> print("$blastcmdspec >  $lustre/$blastpname\.blastlist.$i\n");
	      $fh -> print("$blastcmdsens >> $lustre/$blastpname\.blastlist.$i\n");
	      $fh -> print("mergelines.pl -f $lustre/$blastpname\.blastlist.$i -o $lustre/$blastpname\.blastlistmerged.$i\n");
	      $fh -> print("wc -l $lustre/$blastpname\.blastlist.$i > $lustre/$blastpname\.blastlistsize.$i\n");
              $fh -> print("/usr/bin/scp farm-login:$lustre/$blastpname\.blastlistmerged.$i $phost:$pwd/$blastpname\.blastlistmerged.$i\n") or die "error scp-ing farm-login:$lustre/$blastpname\.blastlistmerged.$i to $phost:$pwd/$blastpname\.blastlistmerged.$i\n$!\n"; 
              $fh -> print("/usr/bin/scp farm-login:$lustre/$blastpname\.blastlistsize.$i $phost:$pwd/$blastpname\.blastlistsize.$i\n") or die "error scp-ing farm-login:$lustre/$blastpname\.blastlistsize.$i to $phost:$pwd/$blastpname\.blastlistsize.$i\n$!\n"; 
	      $fh -> close;
	  }
	  
	  &printlog( "Waiting for blast jobs" );
	  wait_for_farm("rf$blastpname", "blast", $nobjobs);
	  
      }
      &printlog( "checking blast result integrity" );
      
      my @rerun = ();
      
      #for (my $ii = 1; $ii < scalar(@blastdb)+1; $ii++) {
      foreach my $ii ( @index ) {
	  
	  my $blastlistsize=0;
	  if (-e "$blastpname\.blastlistsize.$ii"){
	      open( BLASTLISTSIZE, "$blastpname\.blastlistsize.$ii" ) or die "FATAL: $blastpname\.blastlistsize.$ii file doesn't exist\n";
	      while(<BLASTLISTSIZE>) {
		  if( /(\d+)\s+\S+/ ) {
		      $blastlistsize = $1;
		      last;
		  }
	      }
	      close BLASTLISTSIZE;
	      system("rm $blastpname\.blastlistsize.$ii");
	  }
	  
	  if( $blastlistsize<25 ) {
	      &printlog( "Zero size output [$blastpname\.blastlist.$ii] -- rerun blast search." );
	      push( @rerun, $ii );
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
	  }
	  redo BLAST;
      }
  }
}

my $blastendtime = time();

#BLOCK 2: PARSE BLAST OUTPUT AND BUILD MINIDBs.
#############
#PPG: Grap the lengths of each seed seq, store in hash %seed_seq_lengths. 
#     Used for fancy calculation of seq boundaries:
my %seed_seq_lengths=();
my $ave_length = 0;
my $min_length = 0;
open(SEQSTAT,"seqstat -a SEED|") || die "cannot run \"seqstat -a SEED\" pipe";
while(my $sqst = <SEQSTAT>){
    if ($sqst =~ /^\*\s+(\S+)\s+(\d+)/){
	$seed_seq_lengths{$1}=$2;
	if ($2>$min_length){
	    $min_length=$2;
	}
    }
    elsif ($sqst =~ /^Average length:\s+(\d+)/){
	$ave_length = int($1);
    }
}
#############

if (!defined($cpus)){
    $cpus       = 20;
}

my $k = 1;  # number of minidb files. 
if( $minidbpname ) {
    if( my @t = glob( "$minidbpname\.minidb.*" ) ) {
	$k = scalar(@t);
	
#	for  (my $ij = 0; $ij < $k; $ij++){
	system("scp $pwd/$minidbpname\.minidb.* farm-login:$lustre/") and die "error scp-ing mini db:$minidbpname\.minidb.* to farm-login:$lustre/\n$!";
	#}
	
    }
    else {
	&printlog( "FATAL: no minidb files [$minidbpname\.minidb.*]" );
	die;
    }
}
elsif (!$nofilters) {
    
    #What's the limit here? Can (or should) we increase the number of nodes we run on?
    &printlog( "parsing blast list" );
    $minidbpname = $$;
    my %seqlist;
    for( my $iii=1; $iii <= scalar(@blastdb); $iii++ ) {
	&parse_list( \%seqlist, "$blastpname\.blastlistmerged.$iii" );
    }
    
    my $numseqs = scalar( keys %seqlist );
    if ($numseqs == 0){
	&printlog("FATAL: There were 0 hits in the BLAST files. Check BLAST files. Malformed WUBLASTFILTER and/or WUBLASTMAT environment variables?");
	die;
    }
    
    my $count = int( $numseqs/$cpus ) + 1;
    my @seqids = keys %seqlist;
    &printlog( "Building mini database of $numseqs sequences spread across $cpus files" );
    
    $k=1;
    while( @seqids ) {
	my @tmpids = splice( @seqids, 0, $count ); 
	my(%forward, %reverse);
	open( FA, ">$minidbpname\.minidb.$k" ) or die;
	foreach my $seqid ( @tmpids ) {
	    foreach my $reg ( @{ $seqlist{$seqid} } ) {
		if($reg->{'strand'} == 1){
		    push( @{ $forward{$seqid} }, $reg); 
		}else{
		    push( @{ $reverse{$seqid} }, $reg); 
		} 
	    }
	}
	SeqFetch::fetchSeqs(\%forward, $Rfam::rfamseq, 0, \*FA);
	SeqFetch::fetchSeqs(\%reverse, $Rfam::rfamseq, 1, \*FA);
	close(FA) || die "Could not close fasta file:[$!]\n";
	$k++;
    }
    $k--;
    
    system("scp $minidbpname\.minidb.* farm-login:$lustre/") and die "Failed to copy $minidbpname\.minidb.\* to lustre file system\n";  

    undef( %seqlist );             # free up memory
}

my $minidbendtime = time();

#BLOCK 3: RUN CMSEARCH:

my $queuecm      = 'long -R \"select[type=X86_64]\"';
my $command = "cmsearch";
my $options = "  "; #add the hmm filter? still using 0.72 - not yet
$options .= " --local "   if( $local );
$options .= " --hmmonly " if( $hmmonly );
$options .= " --toponly " if( !$nofilters );

$pname = "cm$$" if( not $pname );
system "cp CM $$.CM" and die;

&printlog( "Queueing cmsearch jobs" );

my $fhcm = IO::File->new();

system("scp $pwd/$$.CM farm-login:$lustre/$$.CM") and die "error scp-ing $pwd/$$.CM to farm-login:$lustre/$$.CM\n$!\n";

&printlog( "" );

if ($nofilters){
    my @seqdb = glob( "$blastdbdir/*.fa.gz" );
    $k=0;
    foreach my $sdb (@seqdb) {
	$sdb =~ s/\.gz$//g;
	$sdb =~ s/$blastdbdir/$blastdbdir2/g;

	&printlog( "Running: | bsub -q $queuecm -o $pwd/$$/$$.err.\%I -J$pname" );
	$fhcm -> open( "| bsub -q $queuecm -o $pwd/$$/$$.err.\%I  -J$pname" ) or die "$!";
	&printlog( "Running: $command $options $lustre/$$.CM $sdb > $lustre/$$.OUTPUT.$k");
	$fhcm -> print( "$command $options $lustre/$$.CM $sdb > $lustre/$$.OUTPUT.$k\n" );
	$fhcm -> close;
	$k++;
    }    
}
else {
    &printlog( "Running: | bsub -q $queuecm -o $pwd/$$/$$.err.\%I -J$pname\"[1-$k]\"" );
    $fhcm -> open( "| bsub -q $queuecm -o $pwd/$$/$$.err.\%I -J$pname\"[1-$k]\"" ) or die "$!";
#$fhcm -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find scp
    &printlog( "Running: $command $options $lustre/$$.CM $lustre/$minidbpname\.minidb.\$\{LSB_JOBINDEX\} > $lustre/$$.OUTPUT.\$\{LSB_JOBINDEX\}");
    $fhcm -> print( "$command $options $lustre/$$.CM $lustre/$minidbpname\.minidb.\$\{LSB_JOBINDEX\} > $lustre/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n" );
    $fhcm -> close;
}

my $cmopts=$options . "";
&printlog( "Waiting for cmsearch jobs." );
wait_for_farm($pname, "cmsearch", $k);

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
$fhcm -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find scp
                   #$lustre/$$.OUTPUT.
$fhcm -> print("cat $lustre/$$.OUTPUT.* > $lustre/$$.OUTPUT_full\n")  or  die "cant concatenate output files on the farm\n$!\n";
$fhcm -> print("/usr/bin/scp $lustre/$$.OUTPUT_full  $phost:$pwd/OUTPUT\n") or die "cant copy $lustre/$$.OUTPUT_full to $phost:$pwd/OUTPUT\n$!\n";
$fhcm -> print("date >> /tmp/$$.cmerr\n");
$fhcm -> print("/usr/bin/scp /tmp/$$.cmerr $phost:$pwd/CMSEARCH_JOBS_COMPLETE\n") or die "cant copy /tmp/$$.cmerr to $phost:$pwd/CMSEARCH_JOBS_COMPLETE\n$!\n";
$fhcm -> print("rm -rf /tmp/$$.cmerr\n");
$fhcm -> print("rm -rf $lustre\n") or die "failed to clean up files on the farm\n";
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

##############

sub printlog {
    my $m = join( '', @_ );
    my $time = `date`;
    chomp $time;
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

##############
#Parse "merged" wu-blast output:  
sub parse_list {
    my $list       = shift;
    my $blastfile  = shift;
    my $linenumber = 0;
    my( $name, $start, $end, $strand );
    open( BL, $blastfile ) or die;
    while( <BL> ) {
	$linenumber++;
       
	if( /^(\S+)\s+(\S+)\s+(\d+)\s+(\d+)/ ) {
	    $name  =  $1;
	    $strand = $2;
	    $start =  min($4 - $window,$3); #This looks strange but is correct.
	    $end   =  max($3 + $window,$4);  #Ditto.
	    $start = 1 if( $start < 1 );
	}
	else {
	    chomp;
	    warn "failed to parse blast line $linenumber in file $blastfile [$_]\n";
	    next;
	}
	
	# avoid having multiple copies of one region in minidb
	my $already;
	if( defined($name) && defined($start) && defined($end) && exists($list->{$name}) ) {
	    foreach my $se ( @{ $list->{$name} } ) {
		
		if( $se->{'strand'} == $strand and $start <= $se->{'start'} and $se->{'start'} <= $end ) {
		    $se->{'start'} = $start;
		    $already = 1;
		}
		
		if( $se->{'strand'} == $strand and $start <= $se->{'end'} and $se->{'end'} <= $end ) {
		    $se->{'end'} = $end;
		    $already = 1;
		}
		
		if( $se->{'strand'} == $strand and $se->{'start'} <= $start and $end <= $se->{'end'} ) {
		    $already = 1;
		}
	    }
	}
	
	if( !$already ) {
	    push( @{ $list->{$name} }, { 'start'  => $start,
					 'end'    => $end,
				         'strand' => $strand} );
	}
    }
    

}
#########
#########

sub update_output { 
    my $oldfile = shift;
    my $newfile = shift;
    
    my %delup;
    my $difffile = "$Rfam::rfamseq_current_dir/rfamseq.diff";
    my $svfile   = "$Rfam::rfamseq_current_dir/embl_sv.txt";
    open( DIFF, $difffile ) or die;
    while(<DIFF>) {
	if( my( $acc ) = /^(\S+)\.(\d+)\s+(UPDATE|DELETE)/ ) {
	    $delup{ $acc } = $2;
	}
    }
    close DIFF;

    my %sv;
    open( SV, $svfile ) or die;
    while(<SV>) {
	if( my( $acc, $ver ) = /^(\S+)\.(\d+)/ ) {
	    $sv{ $acc } = $ver;
	}
    }
    close SV;

    my $skip;
    open( NEW, ">$newfile" ) or die;
    open( OLD, "$oldfile" ) or die;
    while(<OLD>) {
	if( my( $acc, $junk ) = /^sequence\:\s+(\w+)\.?\d*(.*)/ ) {
#	    print STDERR "$_\n$acc;$junk\n";
	    if( $delup{ $acc } ) {
		$skip = 1;
		next;
	    }
	    else {
		$_ = "sequence: $acc.$sv{$acc}".$junk."\n";
		$skip = 0;
	    }
	}
	print NEW $_ unless $skip;
    }
    close OLD;
    close NEW;
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

sub is_integer {
    
    my $test = shift;
    
    if (defined($test) && $test =~ /^(\-|\+)?(0|[1-9])\d*$/){
	return 1;
    }
    else {
	return 0;
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

my $gap_thresh = 0.5;      #Gap threshold for lcmask
my $nuc_freq_thresh = 0.1; #Nucleotide frequency threshold for lcmask
my $max_pid_thresh = 0.95; 

if(!defined($out_file)){
    my @stk_file = split(/\./, $stk_file);
    my $out_file = pop(@stk_file); # 
    $out_file = $out_file . ".fa";
}

#Read sequences into hashes:
open( STK, "$stk_file" ) or die ("FATAL: Couldn't open $stk_file [$!]\n $!\n");
my $seed = new Rfam::RfamAlign;
$seed -> read_stockholm( \*STK );
close(STK);
my @list = $seed->each_seq();

my $length = length($list[0]->seq);
my $noseqs = @list;
my @aofh_nuc_counts; #array of hashes to store nuc-counts.

foreach my $seqobj ( @list ) {
    
    my $seqname = $seqobj->id . "/" . $seqobj->start . "-" . $seqobj->end;
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
open( OUT, ">$out_file" ) or die ("FATAL: Couldn't open $out_file [$!]\n $!\n");
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
	    if(/$bjobname/){
		$jobs++;
	    }
	    
	    if (/PEND/){
		$bjpend++;
	    }
	    
	    if (/RUN/){
		$bjrun++;
	    }
	}
	close(S);# || die "failed to close pipe on bjobs:[$!]\n"; <- this caused a "die" once all the jobs were finished!
	
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

