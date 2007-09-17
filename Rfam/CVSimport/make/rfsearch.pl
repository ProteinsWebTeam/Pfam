#!/software/bin/perl -w

# This is mostly pretty NFS friendly now.  Should run rfsearch itself on 
# something with local storage, but the jobs all get sent off to blades
# using nice socket things, pfetch etc.  

use strict;
use Getopt::Long;
use IO::File;

use Rfam;

my( $quiet, 
    $nobuild, 
    $local,
    $global,
    $name,
    $blast,
    $help,
    $update,
    $minidb,
    );

my $blast_eval = 10;
my $window;#     = 100;  # bad bad
my $cpus       = 20;
my $queue      = 'long -R \"select[type=LINUX64]\"';
my $queue2     = 'small -R \"select[type=LINUX64]\"';
my $lustre = "/lustre/scratch1/sanger/rfam/$$";

system("lsrun -m farm-login mkdir -p $lustre") and die "Failed to make directory $lustre\n";
system("lsrun -m farm-login lfs setstripe $lustre 0 4 4") and die "Failed to set stripe\n";

sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches covariance model against sequence database

Usage:   rfsearch.pl <options>
Options:       -h              show this help
	       -e <n>          use blast evalue of <n>
               -q <queue>      use lsf queue <queue> for the cmsearch step
               -bq <queue>     use lsf queue <queue> for the blast jobs
	      # -w <n>          window size <n> basepairs # no longer valid option with infernal 0.81
	       --name <str>    give lsf a name for the cmsearch jobs
	       --local         run cmsearch with --local option
	       --global        run cmsearch in global mode (override DESC cmsearch command)
	       --cpu           number of cpus to run cmsearch job over
	       --nobuild       skip cmbuild step
	       --update        update the family to a new underlying sequence db (expert!)

EOF
}

&GetOptions( "e=s"      => \$blast_eval,
	     "q=s"      => \$queue,
	    # "bq=s"     => \$bqueue,
	     "local"    => \$local,
	     "global"   => \$global,
	     "cpu=s"    => \$cpus,
             "w=s"      => \$window,
	     "nobuild"  => \$nobuild,
	     "name=s"   => \$name,
	     "blast=s"  => \$blast,
	     "minidb=s" => \$minidb,
	     "update"   => \$update,
	     "h"        => \$help );

if( $help or not -e "SEED" ) {
    &help();
    exit(1);
}


#Read cmbuild/cmsearch flags from the DESC file:
my $buildopts;
if( -s "DESC" ) {
    open( D, "DESC" ) or die "DESC exists but I can't open it";
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
	/^BM\s+cmsearch.*-W\s+(\d+)/ and do {
	    unless( $window ) {
		$window = $1;
		&printlog( "WARN: Using window [$window] from DESC file" );
	    }
	};
    }
}

$buildopts = "" unless $buildopts;
open( S, "SEED" ) or die;
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

if( -e "CMSEARCH_JOBS_COMPLETE" ) {
    unlink( "CMSEARCH_JOBS_COMPLETE" ) or die "can't remove file [CMSEARCH_JOBS_COMPLETE]\n";
}

&printlog( "" );
&printlog( "Starting rfsearch" );

# defaults
my $blastdbdir  = $Rfam::rfamseq_current_dir;  # glob files from here
my $blastdbdir2 = "/data/blastdb/Rfam/rfamseq";  # but run things from here

my $fafile = "$$.fa";

system "sreformat fasta SEED > $fafile" and die "can't convert SEED to $fafile";
unless( $nobuild ) {
    &printlog( "Building model" );
    system "cmbuild -F $buildopts CM SEED" and die "can't build CM from SEED";
}

#########################
##new CM window stuff

##overide the window value for cmsearch from the DESC file with that from CM file.
##get the window size from the CM file. want to use this for both updating the DESC file
##and for setting the window for the blastprefiltering.(rather than the inflexible value of 200)

my $pwd   = `pwd`;
my $phost = `uname -n`;
chomp $pwd;
chomp $phost;

##get the cmsearch W value from the new CM file.
open( CMD, "grep \"^W\"  $pwd/CM |" )
    or die "failed miserably: $!";
my $string = <CMD>;
close CMD;
my $cmwindow; #for updating DESC file
    if ($string =~ /^W\s+(\d+)\b/){
	$cmwindow=$1;
        &printlog( "Obtained cmwindow from CM file =$cmwindow" );
    }else {
	die "Failed to get cmwindow value from the CM file $!";
    }

$window=$cmwindow;
&printlog( "Set blastfilter window =$window" );

# the following should never end up being used... but leave in place for moment.
#This looks pretty dodgy. $window should vanish with Infernal >=0.8 thanks to QDB. 
#Still used for the BLAST prefiltering however:
#i failed to set it, use default;
if( !$window ) {
    $window = 200;
    $cmwindow = 200;
    &printlog( "WARN: Using default window [$window]" );
}

##############

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
mkdir( "$pwd/$$", 0775 ) or  die "cant create the dir for error logs" ;
my @index = ( 1..scalar(@blastdb) );
my $round = 0;

#things to copy to lustre file system
system("/usr/bin/scp $pwd/$fafile farm-login:$lustre/$fafile");

unless( $minidb ) {
  BLAST: {
      if( $round or !$blast ) {
	  &printlog( "Queuing up blast jobs [round ".$round."]" );
	  $blast = $$ if( !$blast );
	  foreach my $i ( @index ) {
	      my $blastdb = $blastdb[$i-1];
	      $blastdb =~ s/\.nhr$//g;
	      $blastdb =~ s/$blastdbdir/$blastdbdir2/g;
	      #setenv WUBLASTDB /lustre/pfam/rfam/Production/rfamseq/CURRENT
	      #my $blastcmd = "wublastn $blastdb $lustre/$fafile B=9999999 V=9999999 E=10 mformat=3 hspsepSmax=1000";
	      my $blastcmd = "blastall -p blastn -d $blastdb -i $lustre/$fafile -F F -W 7 -b 100000 -v 100000 -e 10 -m 9";
  	      my( $div ) = $blastdb =~ /$blastdbdir\/(\S+)$/;
	      my $fh = new IO::File;
	      $fh -> open("| bsub -q $queue -J\"rf$$\" -o $pwd/$$/$blast.berr.$i") or die "$!";
	      $fh -> print(". /usr/local/lsf/conf/profile.lsf\n");       # so we can find lsrcp
	      $fh -> print("$blastcmd > $lustre/$blast.blastlist.$i\n");
              $fh -> print("/usr/bin/scp farm-login:$lustre/$blast.blastlist.$i $phost:$pwd/$blast.blastlist.$i\n") or die "$!";
	      $fh -> close;
	  }
      
	  &printlog( "Waiting for blast jobs" );
	  # my $fh = new IO::File;
# 	  $fh -> open("| bsub -I -q $queue2 -w\'done(rf$blast)\'") or die "$!";
# 	  $fh -> print("echo \"blast jobs finished at:\" > /tmp/$blast.berr\n");
# 	  $fh -> print("date >> /tmp/$blast.berr\n");
# 	  $fh -> print("scp /tmp/$blast.berr $phost:$pwd/$blast.berr\n");
# 	  $fh -> print("rm -f /tmp/$blast.berr\n");
# 	  $fh -> close;
#       }
	  my $wait = 1;
	  while($wait){
	  	sleep(15);
	  	system("bjobs -J rf$blast > rf$blast.log");
	  	my $jobs;
	  	open(LOG, "rf$blast.log") || die "Failed to open blast log\n";
	  	while(<LOG>){
	  		if(/rf$blast/){
	  			$jobs++;
	  		}
	  	}
	  	close(LOG);
	  	if($jobs){
	  		print STDERR "There are $jobs blast job still running\n"; 
	  	}else{
	  		$wait = 0;
	  	}
	  }
	}
      &printlog( "checking blast result integrity" );
      my @rerun;
      foreach my $i ( @index ) {
	  if( !-s "$blast.blastlist.$i" ) {
	      &printlog( "Zero size output [$blast.blastlist.$i] -- rerun blast search." );
	      push( @rerun, $i );
	  }
      }
      if( @rerun ) {
	  $round ++;
	  if( $round > 3 ) {
	      &printlog( "FATAL: Maximum number of Blast failures" );
	      die;
	  }
	  @index = @rerun;
	  redo BLAST;
      }
  }
}


my $k = 0;  # number of minidb files
if( $minidb ) {
    if( my @t = glob( "$minidb.minidb.*" ) ) {
	$k = @t;
    }
    else {
	&printlog( "FATAL: no minidb files [$minidb.minidb.*]" );
	die;
    }
}
else {
    &printlog( "parsing blast list" );
    $minidb = $$;
    my $seqlist = {};
#    if( $blast ) {
#	$seqlist = &parse_list( $seqlist, "$blast" );
#    }
#    else {
	for( my $i=1; $i <= scalar(@blastdb); $i++ ) {
	    $seqlist = &parse_list( $seqlist, "$blast.blastlist.$i" );
	}
#    }

    &printlog( "Building mini database" );
    my $numseqs = scalar( keys %{ $seqlist } );
    my $count = int( $numseqs/$cpus ) + 1;
    my @seqids = keys %{ $seqlist };


    while( @seqids ) {
	my @tmpids = splice( @seqids, 0, $count ); 
	my @nses;

	foreach my $seqid ( @tmpids ) {
	    foreach my $reg ( @{ $seqlist->{$seqid} } ) {
		# this is a bit complex just to get an array of nses
		my( $start, $end ) = ( $reg->{'start'}, $reg->{'end'} );
		push( @nses, "$seqid:$start-$end" );
	    }
	}

	$k++;
	my $nse_count = 100;
	my $seen_count = 0;
	
	open( FA, "> $minidb.minidb.$k" ) or die;
	while( @nses ) {
	    # replace pfetch with xdget -- find old revisions to
	    # see pfetch code
	    my $nse = pop( @nses );
	    my( $n, $s, $e ) = $nse =~ /(\S+)\:(\d+)-(\d+)/;
	    my $fh = IO::File->new();
	    $fh -> open( "xdget -n -a $s -b $e /lustre/pfam/rfam/Production/rfamseq/CURRENT/rfamseq.fa $n |" ) or die "\nFATAL:xdget failure\n";
	    while(<$fh>) {
		if( /^\>/ ) {
		    print FA ">$n/$s-$e\n";
		}
		else {
		    print FA $_;
		}
	    }
	    $fh -> close or die "\nFATAL:xdget failure\n";
	    $seen_count ++;

	}
	close FA;
    }
    undef( $seqlist );             # free up memory
}

my $command = "cmsearch";
my $options = "";
$options .= "--local " if( $local );
#$options .= "-W $window"; ##currently removed this as version 0.73 calculates this by default.

my $cmopts= $options . "-W $cmwindow";  #use this for updating DESC file

$name = "cm$$" if( not $name );
system "cp CM $$.CM" and die;

&printlog( "Queueing cmsearch jobs" );

my $fh = IO::File->new();
# preexec script copies files across and then tests for their presence
# if this fails then the job should reschedule for another go

#&printlog( "\nRunning: scp $pwd/$$.CM farm-login:$lustre/$$.CM\n");
system("scp $pwd/$$.CM farm-login:$lustre/$$.CM") and die "error scp-ing $$.CM model to lustre filesystem";
for  (my $i = 1; $i <= $k; $i++){
#   &printlog( "\nRunning: scp $pwd/$minidb.minidb.$i farm-login:$lustre/$minidb.minidb.$i\n");
   system("scp $pwd/$minidb.minidb.$i farm-login:$lustre/$minidb.minidb.$i") and die "error scp-ing mini db:$minidb.minidb.$i to lustre filesystem";
}

$fh -> open( "| bsub -q $queue -o $pwd/$$/$$.err.\%I -J$name\"[1-$k]\"" ) or die "$!";
$fh -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find scp
&printlog( "\nRunning: $command $options $lustre/$$.CM $lustre/$minidb.minidb.\$\{LSB_JOBINDEX\} > $lustre/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n\n");
$fh -> print( "$command $options $lustre/$$.CM $lustre/$minidb.minidb.\$\{LSB_JOBINDEX\} > $lustre/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n" );
$fh -> close;

# send something to clean up
print STDERR "set cm searches set running, copy files and clean up....\n";

$fh = new IO::File;
$fh -> open("| bsub -q $queue2 -w\'done($name)\'") or die "$!";
$fh -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find scp
$fh -> print("cat $lustre/$$.OUTPUT.* > $lustre/$$.OUTPUT_full\n") or  die "cant catenate output files on the farm\n";
$fh -> print("/usr/bin/scp $lustre/$$.OUTPUT_full  $phost:$pwd/OUTPUT\n") or  die "failed to copy output files from the farm\n";
$fh -> print("date >> /tmp/$$.cmerr\n");
$fh -> print("/usr/bin/scp /tmp/$$.cmerr $phost:$pwd/CMSEARCH_JOBS_COMPLETE\n");
$fh -> print("rm -rf /tmp/$$.cmerr\n");
##PPG: remember to uncomment these:
$fh -> print("rm -rf $lustre/$$.*\n") || die "failed to clean up files on the farm\n";
$fh -> close;

&update_desc( $buildopts, $cmopts ) unless( !-e "DESC" );

print STDERR "finished cleaning up... wait for cm searches to complete\n ";
##############

sub printlog {
    my $m = join( '', @_ );
    my $time = `date`;
    chomp $time;
    open( LOG, ">>rfsearch.log" ) or die;
    if( $m ) {
        printf LOG "%-40s [%s]\n", $m, $time;
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
    while( <BL> ) {
	next if( /^\#/ );
	my( $name, $start, $end );
	if( /^\S+\s+(\S+)\s+\S+\s+\d+\s+\d+\s+\d+\s+\d+\s+\d+\s+(\d+)\s+(\d+)\s+/ ) {
	    $name = $1;
	    $start = $2 - $window;
	    $end   = $3 + $window;
	    $start = 1 if( $start < 1 );

	}
	elsif( /^(\S+)\s+(\d+)\s+(\d+)/ ) {
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
	if( exists $list->{$name} ) {
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

	if( $already ) {
#	    print " .... SKIP\n";
	    # skip
	}
	else {
#	    print " .... KEEP\n";
	    push( @{ $list->{$name} }, { 'start' => $start,
					 'end'   => $end } );
	}
    }
    return $list;
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
    my ($buildopts,$searchopts) = @_;
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
	    print DNEW "BM   cmsearch ", $searchopts, " CM SEQDB\n";
	    next;
	}
	print DNEW $_;
    }
    close DESC;
    close DNEW;
    rename( "DESC", "DESC.old" ) or die;
    rename( "DESC.new", "DESC" ) or die;
}
