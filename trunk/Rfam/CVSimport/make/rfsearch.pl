#!/software/bin/perl -w

# This is mostly pretty NFS friendly now.  Should run rfsearch itself on 
# something with local storage, but the jobs all get sent off to blades
# using nice socket things, pfetch etc.  

use strict;
use Getopt::Long;
use IO::File;
use Data::Dumper; #Great for printing diverse data-structures.
use Rfam;

my $starttime = time();

my( $quiet, 
    $nobuild, 
    $local,
    $global,
    $pname,
    $blastpname,
    $help,
    $update,
    $minidbpname,
    $nowublast,
    $long
    );

my $lustre = "/lustre/scratch1/sanger/rfam/$$"; #path for dumping data to on the farm
my $wublastdb = "/lustre/pfam/rfam/Production/rfamseq/CURRENT";
my $wublastcpus = 4; #Number of CPUs wu-blast will run on "-cpu N". Not used at present!
my $blast_eval = 10;
my $window;
my $cpus       = 20;
#ADD MULTI-CPU OPTIONS FOR BSUB? Compile MPI Infernal! Find mpirun on the farm!
#-n4 -R "span[hosts=1]"
#select[type==X86_64] -> See mail from Tim Cutts:
#my $queue      = 'long -R \"select[type=LINUX64]\"';
my $queue      = 'long -R \"select[type==X86_64]\"';
#my $queue      = 'long -n4 -R \"span[hosts=1]\"';
#my $queue2     = 'small -R \"select[type=LINUX64]\"';
my $queue2     = 'small -R \"select[type==X86_64]\"';

#Set the stripe pattern on the lustre file system:
#lfs setstripe <filename> <strip-esize> <start-ost> <stripe-cnt>
#&printlog( "Making lustre run directory on the farm: $lustre" );
#&printlog( "lsrun -m farm-login mkdir -p $lustre" );
#system("lsrun -m farm-login mkdir -p $lustre") and die "Failed to make directory $lustre\n";
#&printlog( "lsrun -m farm-login lfs setstripe $lustre 0 4 4" );
#system("lsrun -m farm-login lfs setstripe $lustre 0 4 4") and die "Failed to set stripe\n";
#&printlog( "See the wiki page \"LustreStripeSize\" for more detail" );
#See the wiki page "LustreStripeSize" for more detail. 

my $fh0 = new IO::File;
$fh0 -> open("| bsub -I -q $queue2") or die "FATAL: bsub -I -q $queue2\n$!";
&printlog( "Making lustre run directory on the farm: $lustre" );
&printlog( "lsrun -m farm-login mkdir -p $lustre" );
$fh0 -> print("lsrun -m farm-login mkdir -p $lustre\n") or die "FATAL: lsrun -m farm-login mkdir -p $lustre\n$!";
&printlog( "lsrun -m farm-login lfs setstripe $lustre 0 4 4" );
$fh0 -> print("lsrun -m farm-login lfs setstripe $lustre 0 4 4\n") or die "FATAL: lsrun -m farm-login lfs setstripe $lustre 0 4 4\n$!";
$fh0 -> close;
#See the wiki page "LustreStripeSize" for more detail. 



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
	       --cpu               number of cpus to run cmsearch job over
	       --nobuild           skip cmbuild step
	       --pname <str>       give lsf a process name, "str", for the cmsearch jobs
	       --blastpname <str>  restart a job with pname "str" from after the blast runs
	       --minidbpname <str> restart a job with pname "str" from after the minidb creation
	       --update            update the family to a new underlying sequence db (expert!)
	       --nowu              use ncbi-blast rather than wu-blast (default).
	       --long              [TO BE IMPLEMENTED] an option for long models (eg. SSU/LSU rRNA, Xist, AIR,...),
	                           This runs "cmsearch -hmmfilter", requires infernal version >0.81. 
	       --window <str>      Use this window size for fetching blast sequences rather than from CM.
EOF
}

&GetOptions( "e=s"           => \$blast_eval,
	     "q=s"           => \$queue,
	    # "bq=s"          => \$bqueue,
	     "local"         => \$local,
	     "global"        => \$global,
	     "cpu=s"         => \$cpus,
	     "nobuild"       => \$nobuild,
	     "pname=s"       => \$pname,
	     "blastpname=s"  => \$blastpname,
	     "minidbpname=s" => \$minidbpname,
	     "update"        => \$update,
	     "nowu"          => \$nowublast,
	     "long"          => \$long,
	     "window"        => \$window,
	     "h"             => \$help );

if( $help or not -e "SEED" ) {
    &help();
    exit(1);
}
elsif ($long) {
    die("Sorry: --long is not supported yet. Wait for Infernal 0.8!");
}

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

my $pwd   = `pwd`;
my $phost = `uname -n`;
chomp $pwd;
chomp $phost;

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
if( !$seenrf and $buildopts =~ /--rf/ ) {
    $buildopts =~ s/--rf //g;
}

unless( $nobuild ) {
    &printlog( "Building model: cmbuild -F $buildopts CM SEED" );
    system "cmbuild -F $buildopts CM SEED" and die "can't build CM from SEED";
}

##get the cmsearch W value from the CM file.
&printlog( "get the cmsearch W value from the CM file" );
my $cmwindow = 0;
if (-e "$pwd/CM"){
    my $string = `grep \"^W\" $pwd/CM`;
    my @string = split('\s+', $string);
    $cmwindow = $string[1];
}
else {
    die ("The $pwd/CM file is missing! SEED file exists, check cmbuild ran."); 
}

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

&printlog( "" );
&printlog( "Starting rfsearch" );

# defaults
my $blastdbdir  = $Rfam::rfamseq_current_dir;  # glob files from here
my $blastdbdir2 = $Rfam::rfamseq_run_dir;      # but run things from here
my $fafile = "$$.fa";

#PPG: mask out (lower case) low-scoring/high-indel regions of each sequence for the BLAST searches with "-lcmask"?
#     Requires replacing sreformat. 
system "sreformat fasta SEED > $fafile" and die "can't convert SEED to $fafile";


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

my @blastdb;
if( $update ) {      # run over the new.fa.* databases
    @blastdb = glob( "$blastdbdir/new.fa.*[0-9].nhr" );
    &update_output( "OUTPUT", "OUTPUT.0" );
}
else {               # run over *.fa databases
    @blastdb = glob( "$blastdbdir/*.fa.nhr" );
}    

# make sure writable by group
umask(002);
my $user = `whoami`;
chomp($user);

#user must have log dir!
&printlog( "Making log directory: $pwd/$$" );
mkdir( "$pwd/$$", 0775 ) or  die "cant create the dir for error logs" ;
my $nobjobs = scalar(@blastdb);
my @index = ( 1..$nobjobs );
my $round = 0;
#things to copy to lustre file system
system("/usr/bin/scp $pwd/$fafile farm-login:$lustre/$fafile") and die "/usr/bin/scp $pwd/$fafile farm-login:$lustre/$fafile\nerror scp-ing $pwd/$fafile to farm-login:$lustre/$fafile\n$!\n";

unless( $minidbpname ) {
  BLAST: { #JT6 thinks this GOTO is evil - but then he thinks "needs fixed" is evil too. 
      if( $round or !$blastpname ) {
	  &printlog( "Queuing up blast jobs [round ".$round."]" );
	  $blastpname = $$ if( !$blastpname );
	  #for (my $i = 1; $i < scalar(@blastdb)+1; $i++) {
	  foreach my $i ( @index ) { #This is cleverer than it looks, see below for rerunning failed jobs:
	      my $blastdb = $blastdb[$i-1];
	      $blastdb =~ s/\.nhr$//g;
	      $blastdb =~ s/$blastdbdir/$blastdbdir2/g;
	      my $blastcmd = "";
	      #PPG: WU-BLAST is now the default. MYAHAHAHAHA!:
	      if (!$nowublast){
		  #system("setenv WUBLASTDB $wublastdb");
		  #ADD MULTI-CPU OPTIONS? eg. "-cpus 4". DOCS SAY THAT BLAST USES AS MANY AS POSS (upto 4) BY DEFAULT... 
		  #NB. memory usages grows linearly, but wu-blast seems to deal with this sensibly...
		  $blastcmd = "wublastn $blastdb $lustre/$fafile W=7 B=100000 V=100000 E=$blast_eval mformat=3 hspsepSmax=$window"; # cpus=$wublastcpus";
		  #PPG: SHALL WE MASK OUT LOW-PROB/HIGH-INDEL SEQUENCE?: 
		  #- requires writing a tool to identify low-prob nucs.
                  #see '-lcmask' option.
	      }
	      else {
		  &printlog( "WARNING: using NCBI-BLAST instead of WU-BLAST. Deprecated and not recommended!");
		  $blastcmd = "blastall -p blastn -d $blastdb -i $lustre/$fafile -F F -W 7 -b 100000 -v 100000 -e $blast_eval -m 9";
	      }
  	      my( $div ) = $blastdb =~ /$blastdbdir\/(\S+)$/;
	      my $fh = new IO::File;
	      $fh -> open("| bsub -q $queue -J\"rf$blastpname\" -o $pwd/$$/$blastpname\.berr.$i") or die "$!";
	      $fh -> print(". /usr/local/lsf/conf/profile.lsf\n");       # so we can find lsrcp
	      $fh -> print("$blastcmd > $lustre/$blastpname\.blastlist.$i\n");
	      #&printlog( "$blastcmd > $lustre/$blastpname\.blastlist.$i" );
              $fh -> print("/usr/bin/scp farm-login:$lustre/$blastpname\.blastlist.$i $phost:$pwd/$blastpname\.blastlist.$i\n") or die "error scp-ing farm-login:$lustre/$blastpname\.blastlist.$i to $phost:$pwd/$blastpname\.blastlist.$i\n$!\n"; 
	      $fh -> close;
	  }
	  
	  #PPG: There must be an easier way to do this. Check bjobs output.
	  #     MAKE THIS INTO A FUNCTION?
	  &printlog( "Waiting for blast jobs" );
	  my $wait = 1;
	  my $bjobcount = 1;
	  my $bjobinterval = 15;
	  my $jobs = $nobjobs;
	  while($wait){
	      
	      sleep($bjobinterval); 
	      
	      $jobs = 0;
	      system("bjobs -J rf$blastpname > rf$blastpname\.log");
	      open(LOG, "rf$blastpname\.log") || die "Failed to open blast log\n";
	      while(<LOG>){
		  if(/rf$blastpname/){
		      $jobs++;
		  }
	      }
	      close(LOG);
	      
	      if ($jobs < int($nobjobs*(1-0.95)) ){#Once 95% of jobs are finished, check frequently.
		  $bjobinterval=15;
	      }	      
	      elsif ($jobs < int($nobjobs*(1-0.80)) ){#Once 80% of jobs are finished, check a little more frequently.
		  $bjobinterval=15 + int(log($bjobcount/2)); 
	      }
	      else {#otherwise check less & less frequently (max. interval is ~150 secs).
		  if ($bjobinterval<150){ $bjobinterval = $bjobinterval + int(log($bjobcount));}
	      }
	      
	      if($jobs){
		  print STDERR "There are $jobs blast job still running after $bjobcount checks. Check interval is now $bjobinterval secs.\n"; 
	      }else{
		  $wait = 0;
	      }
	      $bjobcount++;
	  }
      }
      &printlog( "checking blast result integrity" );
      
      my @rerun = ();
      
      #for (my $ii = 1; $ii < scalar(@blastdb)+1; $ii++) {
      foreach my $ii ( @index ) {
	  #PPG: Add a check to see if all the jobs need rerunning! Indicates farm is fucked up! Or ssh keys not generated! Or ...
	  if( !-s "$blastpname\.blastlist.$ii" ) {
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


my $k = 0;  # number of minidb files. 
if( $minidbpname ) {
    if( my @t = glob( "$minidbpname\.minidb.*" ) ) {
	$k = @t;
    }
    else {
	&printlog( "FATAL: no minidb files [$minidbpname\.minidb.*]" );
	die;
    }
}
else {
    #PPG: If there are a stupid number of hits we could just skim off the most significant 100,000 or so blast hits? 
    #This will save considerable compute for the huge families. eg. RF00177?
    #It'd require sorting every hit on e-val/score, which is probably more efficient than running Infernal! 
    #What's the limit here? Can (or should) we increase the number of nodes we run on?
    &printlog( "parsing blast list" );
    $minidbpname = $$;
    my %seqlist;
    for( my $iii=1; $iii <= scalar(@blastdb); $iii++ ) {
	&parse_list( \%seqlist, "$blastpname\.blastlist.$iii" );
    }
    
    &printlog( "Building mini database " );
    my $numseqs = scalar( keys %seqlist );
    my $count = int( $numseqs/$cpus ) + 1;
    my @seqids = keys %seqlist;

    while( @seqids ) {
	my @tmpids = splice( @seqids, 0, $count ); 
	my @nses;
	
	foreach my $seqid ( @tmpids ) {
	    foreach my $reg ( @{ $seqlist{$seqid} } ) {
		# this is a bit complex just to get an array of nses
		#PPG: Can't we xdget the seqs from in here instead?
		my( $start, $end, $strand ) = ( $reg->{'start'}, $reg->{'end'}, $reg->{'strand'} );
		
		if ($start>0 && $end>$start){
		    push( @nses, "$seqid:$start-$end\t$strand" );
		}
		else {
		    &printlog( "\nWARNING: malformed NSE:\n $seqid:($start)-($end)\t$strand");
		}
	    }
	}
	
	$k++;
	my $nse_count = 100; #PPG: What the hell is this for?
	my $seen_count = 0;
	
	open( FA, "> $minidbpname\.minidb.$k" ) or die;
	while( @nses ) {
	    # replace pfetch with xdget -- find old revisions to
	    # see pfetch code
	    my $nse = pop( @nses );
	    my( $n, $s, $e, $strnd ) = $nse =~ /(\S+)\:(\d+)-(\d+)\t(\S+)/;

	    #PPG: slightly paranoid checks here. Could add "is_integer()" checks also:
	    if (defined($n) && defined($s) && defined($e) && defined($strnd) && $s>0 && $e>$s){
		my $xdcmd = "";
		if ($strnd<0){#Grab minus strand:
#		    $xdcmd = "xdget -n -r -a $s -b $e /lustre/pfam/rfam/Production/rfamseq/CURRENT/rfamseq.fa $n";
		    $xdcmd = "xdget -n -r -a $s -b $e $Rfam::rfamseq $n";
		}
		else {
#		    $xdcmd = "xdget -n -a $s -b $e /lustre/pfam/rfam/Production/rfamseq/CURRENT/rfamseq.fa $n";
		    $xdcmd = "xdget -n -a $s -b $e $Rfam::rfamseq $n";
		}
		#print "$xdcmd\n";
		my $fhxd = IO::File->new();
		#$fhxd -> open( "$xdcmd |" ) or die "\nFATAL:xdget failure 1\n$xdcmd\n$!";
		$fhxd -> open( "$xdcmd |" ) or &printlog( "\nWARNING: xdget failure 1\n$xdcmd\nCheck: $minidbpname\.minidb.$k\n$!");
		while(<$fhxd>) {

		    if( /^\>/ ) {
			print FA ">$n/$s-$e\t$strnd\n";
		    }
		    else {
			print FA $_;
		    }
		    
		    
		}
		#$fhxd -> close or die "\nFATAL:xdget failure 2\nclose failed\n$xdcmd\n$!";
		$fhxd -> close or &printlog("WARNING: xdget failure 2. Close failed. \"$xdcmd\". Check: $minidbpname\.minidb.$k: $!");
		$seen_count++;
	    }#PPG: close paranoid "if".
	    else {
		&printlog("WARNING: malformed NSE: $nse");
	    }
	}
	close FA;
    }
    #undef( $seqlist );             # free up memory
}

my $endblast = time();


my $command = "cmsearch";
my $options = " --toponly "; #add the hmm filter? still using 0.71 - not yet
$options .= "--local " if( $local );
#$options .= "-W $cmwindow"; ##currently removed this as version 0.73 calculates this by default.
my $cmopts=$options . "-W $window"; #use this for updating DESC file

$pname = "cm$$" if( not $pname );
system "cp CM $$.CM" and die;

&printlog( "Queueing cmsearch jobs" );

my $fhcm = IO::File->new();
# preexec script copies files across and then tests for their presence
# if this fails then the job should reschedule for another go

system("scp $pwd/$$.CM farm-login:$lustre/$$.CM") and die "error scp-ing $pwd/$$.CM to farm-login:$lustre/$$.CM\n$!\n";
for  (my $ij = 1; $ij <= $k; $ij++){
   system("scp $pwd/$minidbpname\.minidb.$ij farm-login:$lustre/$minidbpname\.minidb.$ij") and die "error scp-ing mini db:$minidbpname\.minidb.$ij to farm-login:$lustre/$minidbpname\.minidb.$ij\n$!";
}
&printlog( "" );
&printlog( "Running: | bsub -q $queue -o $pwd/$$/$$.err.\%I -J$pname\"[1-$k]\"" );
$fhcm -> open( "| bsub -q $queue -o $pwd/$$/$$.err.\%I -J$pname\"[1-$k]\"" ) or die "$!";
#$fhcm -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find scp
&printlog( "Running: $command $options $lustre/$$.CM $lustre/$minidbpname\.minidb.\$\{LSB_JOBINDEX\} > $lustre/$$.OUTPUT.\$\{LSB_JOBINDEX\}");
$fhcm -> print( "$command $options $lustre/$$.CM $lustre/$minidbpname\.minidb.\$\{LSB_JOBINDEX\} > $lustre/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n" );
$fhcm -> close;

&printlog( "Waiting for cmsearch jobs." );
my $waitcm = 1;
while($waitcm){
    
    my $bjobs_out = `bjobs  -J\"$pname\" | grep $pname`; 
    my @jobscm = split(/\n/,$bjobs_out);
    my $jobscm = @jobscm;
    if($jobscm){
	print STDERR "There are $jobscm cmsearch job still running\n"; 
	sleep(15);
    }else{
	$waitcm = 0;
    }

}


# send something to clean up
print STDERR "set cm searches running, copy files and clean up....\n";

$fhcm = new IO::File;
&printlog( "bsub -q $queue2 -w\'done($pname)\'");
&printlog( "");
$fhcm -> open("| bsub -q $queue2 -w\'done($pname)\'") or die "$!";
$fhcm -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find scp
$fhcm -> print("cat $lustre/$$.OUTPUT.* > $lustre/$$.OUTPUT_full\n")  or  die "cant concatenate output files on the farm\n$!\n";
$fhcm -> print("/usr/bin/scp $lustre/$$.OUTPUT_full  $phost:$pwd/OUTPUT\n") or die "cant copy $lustre/$$.OUTPUT_full to $phost:$pwd/OUTPUT\n$!\n";
$fhcm -> print("date >> /tmp/$$.cmerr\n");
$fhcm -> print("/usr/bin/scp /tmp/$$.cmerr $phost:$pwd/CMSEARCH_JOBS_COMPLETE\n") or die "cant copy /tmp/$$.cmerr to $phost:$pwd/CMSEARCH_JOBS_COMPLETE\n$!\n";
$fhcm -> print("rm -rf /tmp/$$.cmerr\n");
$fhcm -> print("rm -rf $lustre\n") or die "failed to clean up files on the farm\n";
$fhcm -> close;

&update_desc( $buildopts, $cmopts ) unless( !-e "DESC" );
print STDERR "finished cleaning up... wait for cm searches to complete\n ";

#cmsearch --toponly /lustre/scratch1/sanger/rfam/21964/21964.CM /lustre/scratch1/sanger/rfam/21964/21964.minidb.1 >/lustre/scratch1/sanger/rfam/21964/21964.OUTPUT.1


&printlog( "" );
&printlog( "FINISHED! See OUTPUT file." );

my $endtime = time();
my $runtime = $endtime - $starttime;
my $blasttime = $endblast-$starttime;
my $cmsearchtime = $endtime - $endblast;

&printlog( "" );
&printlog( "BLAST Runtime:    $blasttime secs" );
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


sub parse_list {
    my $list       = shift;
    my $blastfile  = shift;
    open( BL, $blastfile ) or die;
    my( $qname, $name, $qstart, $qend, $sstart, $send, $start, $end, $strand );
    while( <BL> ) {
	next if( /^\#/ );
	#PPG: Added a wu-blast parser and made the calculation of the 
	#sequence boundaries slightly more elegant, now we pad either 
	#end of the hit with the lengths either end of the query plus 
	#W2, which is (W-L)/2. 
	#WU parser (see $BLASTDIR/tabular.html):
	#                     1      2     3    4    5    6    7    8    9   10   11   12   13   14   15   16   17    18     19    20    21     22
	if(!$nowublast && /^(\S+)\t(\S+)\t\S+\t\d+\t\S+\t\d+\t\d+\t\d+\t\d+\t\d+\t\S+\t\S+\t\d+\t\d+\t\d+\t\S+\t(\S+)\t(\d+)\t(\d+)\t(\S+)\t(\d+)\t(\d+)/){
	    $qname  = $1;
	    $name   = $2;
	    $qstart = $4;
	    $qend   = $5;
	    $strand = $3*$6;
	    $sstart = $7;
	    $send   = $8;
	    
	    if ($qend<$qstart){#If the hit is to the minus strand of the query we 
			       #need to switch $qstart & $qend.  
		my $temp = $qstart;
		$qstart = $qend;
		$qend = $temp;
	    }

	    if ($send<$sstart){#I don't think this ever happens, but best to switch if it does.
		my $temp = $sstart;
		$sstart = $send;
		$send = $temp;
	    }
	    
	    my $qseq_len = $min_length; #If in doubt use the shortest seq-length. 
	                                #Gives more padding either side.
	    
	    #PPG: compute sequence boundaries plus some generous padding either 
	    #side for running cmsearch on:
	    if (defined($seed_seq_lengths{$qname}) && $seed_seq_lengths{$qname}>0){
		$qseq_len = $seed_seq_lengths{$qname};
	    }
	    if ($qseq_len<$qend){
		&printlog( "WARNING: Query length ($qseq_len) is less than Query end ($qend) in $qname with $name.");
	    }
	    my $window2 = int( abs($window - $qseq_len)/2 );
	    
	    
	    
	    $start = $sstart - $qstart - $window2;
	    $end   = $send + abs($qseq_len-$qend) + $window2;
	    $start = 1 if( $start < 1 );
	    	    
	}
	elsif( /^(\S+)\s+(\S+)\s+\S+\s+\d+\s+\d+\s+\d+\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+/ ) {
	    #PPG: RUN NCBI-BLAST instead! - not recommended. 
	    $qname  = $1;
	    $name   = $2;
	    $qstart = $3;
	    $qend   = $4;
	    $sstart = $5;
	    $send   = $6;
	    my $qseq_len = $ave_length;
	    if (defined($seed_seq_lengths{$name})){
		$qseq_len = $seed_seq_lengths{$name};
	    }
	    my $window2 = int(abs($window - $qseq_len)/2);
	    $start = $sstart - $qstart - $window2;
	    $end   = $send + ($qseq_len-$qend) + $window2;
	    $start = 1 if( $start < 1 );
	    
	}
	elsif( /^(\S+)\s+(\d+)\s+(\d+)/ ) {#WHEN DO WE GET HERE????
	    # add window length onto each end
	    $name  = $1;
	    $start = $2 - $window;
	    $end   = $3 + $window;
	    $start = 1 if( $start < 1 );
	}
	else {
	    chomp;
	    warn "failed to parse blast line [$_]\n";
	    next;
	}
	
	# avoid having multiple copies of one region in minidb
	my $already;
#	if( exists $list->{$name} ) {
#	&printlog( "defined($name) && defined($start) && defined($end) && exists($list)");
	if( defined($name) && defined($start) && defined($end) && exists($list->{$name}) ) {
		    foreach my $se ( sort @{ $list->{$name} } ) {
		
		if( $se->{'start'} >= $start and $se->{'start'} <= $end ) {
		    $se->{'start'} = $start;
		    $already = 1;
		}
		if( $se->{'end'} >= $start and $se->{'end'} <= $end ) {
		    $se->{'end'} = $end;
		    $already = 1;
		}
		if( $se->{'start'} <= $start and $se->{'end'} >= $end ) {
		    $already = 1;
		}
	    }
	}
	
#	print "$name/$start-$end";
	
	if( !$already ) {
#	    print " .... KEEP\n";
	    push( @{ $list->{$name} }, { 'start'  => $start,
					 'end'    => $end,
				         'strand' => $strand} );
	}
    }
    
    if (!defined($name) || !defined($start) || !defined($end)){
	&printlog( "MILD WARNING: no significant blast results in $blastfile");
    }
    elsif ($end<1 || $start<1 || $start>$end || !is_integer($start) || !is_integer($end)){#Add some paranoia checks:
	&printlog( "WARNING: malformed NSE: $name/($start)-($end)\t$strand.");
    }
    elsif ($list == 1){#
	&printlog( "WARNING: $list = 1!");
    }
    else {
	#return $list;
    }
}


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
