#!/usr/local/bin/perl -w

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
    $update );

my $blast_eval = 10;
my $window     = 100;
my $cpus       = 20;
my $queue      = 'long -m bc_hosts';
my $bqueue     = 'long -m bc_hosts';
my $queue2     = 'normal';

sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches covariance model against sequence database

Usage:   rfsearch.pl <options>
Options:       -h              show this help
	       -e <n>          use blast evalue of <n>
               -q <queue>      use lsf queue <queue> for the cmsearch step
               -bq <queue>     use lsf queue <queue> for the blast jobs
	       -w <n>          window size <n> basepairs
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
	     "bq=s"     => \$bqueue,
	     "local"    => \$local,
	     "global"   => \$global,
	     "cpu=s"    => \$cpus,
             "w=s"      => \$window,
	     "nobuild"  => \$nobuild,
	     "name=s"   => \$name,
	     "blast=s"  => \$blast,
	     "update"   => \$update,
	     "h"        => \$help );

if( $help or not -e "SEED" ) {
    &help();
    exit(1);
}


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
		warn "Using --local mode as specified in DESC file\n";
	    }
	};
	/^BM\s+cmsearch.*-W\s+(\d+)/ and do {
	    unless( $window ) {
		$window = $1;
		warn "No window size specified - using $window from DESC file\n";
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
my $blastdbdir2 = "/data/blastdb/Rfam/Large";  # but run things from here

my $fafile = "$$.fa";

system "sreformat fasta SEED > $fafile" and die "can't convert SEED to $fafile";
unless( $nobuild ) {
    &printlog( "Building model" );
    system "/pfam/db/Rfam/bin/cmbuild -F $buildopts CM SEED" and die "can't build CM from SEED";
}

my $i = 0;
my $pwd   = `pwd`;
my $phost = `uname -n`;
chomp $pwd;
chomp $phost;

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
mkdir( "/pfam/db/Rfam/tmp/log/$$", 0755 );

unless( $blast ) {
    &printlog( "Queuing up blast jobs" );
    foreach my $blastdb ( @blastdb ) {
	$blastdb =~ s/\.nhr$//g;
	my $blastcmd = "blastall -p blastn -d $blastdb -i /tmp/$fafile -F F -W 7 -b 100000 -v 100000 -e 10 -m 8";

	$i ++;
	my( $div ) = $blastdb =~ /$blastdbdir\/(\S+)$/;
	my $fh = new IO::File;
	$fh -> open("| bsub -q $bqueue -J\"rf$$\" -o /pfam/db/Rfam/tmp/log/$$/$$.berr.\%J") or die "$!";
	$fh -> print(". /usr/local/lsf/conf/profile.lsf\n");       # so we can find lsrcp
	$fh -> print("PATH=\$\{PATH\}:/usr/local/ensembl/bin\n");  # so we can find blastall
	$fh -> print("lsrcp $phost:$pwd/$fafile /tmp/$fafile\n");
	$fh -> print("$blastcmd > /tmp/$$.blastlist.$i\n");
	$fh -> print("lsrcp /tmp/$$.blastlist.$i $phost:$pwd/$$.blastlist.$i\n");
	$fh -> print("rm -f /tmp/$$.blastlist.$i /tmp/$fafile\n");
	$fh -> close;
    }

    &printlog( "Waiting for blast jobs" );
    my $fh = new IO::File;
    $fh -> open("| bsub -I -q $queue2 -w\'done(rf$$)\'") or die "$!";
    $fh -> print("echo \"blast jobs finished at:\" > /tmp/$$.berr\n");
    $fh -> print("date >> /tmp/$$.berr\n");
    $fh -> print("lsrcp /tmp/$$.berr $phost:$pwd/$$.berr\n");
    $fh -> print("rm -f /tmp/$$.berr\n");
    $fh -> close;
}

&printlog( "parsing blast list" );
my $seqlist = {};
if( $blast ) {
    $seqlist = &parse_list( $seqlist, "$blast" );
}
else {
    for( my $j=1; $j<=$i; $j++ ) {
	$seqlist = &parse_list( $seqlist, "$$.blastlist.$j" );
    }
}

&printlog( "Building mini database" );
my $numseqs = scalar( keys %{ $seqlist } );
my $count = int( $numseqs/$cpus ) + 1;
my $k = 0;
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

    open( FA, "> $$.minidb.$k" ) or die;
    while( @nses ) {
#	my @pfetchids = ( [ splice( @nses, 0, $nse_count ) ],    # first round of fetches
#			  [],                                    # end > length failures
#			  [] );                                  # version failures


# replace pfetch code with an xdget for now
#####
	my $nse = pop( @nses );
#	print "$nse\n";
	my( $n, $s, $e ) = $nse =~ /(\S+)\:(\d+)-(\d+)/;
	my $fh = IO::File->new();
	$fh -> open( "xdget -n -a $s -b $e /pfam/db/rfamseq/CURRENT/rfamseq.fa $n |" ) or die "\nFATAL:pfetch failure\n";
	while(<$fh>) {
	    if( /^\>/ ) {
		print FA ">$n/$s-$e\n";
	    }
	    else {
		print FA $_;
	    }
	}
	$fh -> close or die "\nFATAL:pfetch failure\n";
	$seen_count ++;

#####

#	for( my $p=0; $p<@pfetchids; $p++ ) {
#	    my $listref = $pfetchids[$p];
#	    next unless( @{$listref} );

#	    my $options = "-d embl";
#	    if( $p == (@pfetchids-1) ) {
		# we're retreiving the version failures
#		$options .= " -a";
#	    }

#	    my $str = join( ' ', @{$listref} );
#	    my $fh = IO::File->new();
#	    $fh -> open( "pfetch $options $str |" ) or die "\nFATAL:pfetch failure\n";

#	    my $i = 0;
#	    my( $endmismatch );

#	    while(<$fh>) {
#		if( my( $pfetchid ) = /^\>\S+\s+(\S+)/ ) {
#		    my $nse = $listref->[$i];
#		    $nse =~ s/:/\//g;
#		    my( $tmpid ) = $nse =~ /(\S+\.\d+)\/\d+-\d+/;
#		    if( $pfetchid ne $tmpid ) {
			# catch stupid problems
#			die "\nFATAL: pfetch id problem - [$nse] mismatch [$pfetchid]\nReport this!\n";
#		    }
#		    print FA ">$nse\n";
#		    $i ++;
#		    $seen_count ++;
#		    $endmismatch = 0;
#		}
#		elsif( my( $end ) = /end is greater than sequence length.*?(\d+)/ ) {
#		    my $nse = $listref->[$i];
#		    if( $p == 1 ) {
			# this has failed before, something wrong
#			die "\nFATAL: failed to pfetch [$nse]\n";
#		    }
#		    my( $tmp ) = $nse =~ /(\S+\.\d+\:\d+-)\d+/;
#		    push( @{$pfetchids[1]}, "$tmp$end" );
#		    $i++;
#		    $endmismatch = 1;
#		}
#		elsif( /no match/ ) {
#		    if( $endmismatch ) {
			# we've already dealt with this sequence
#			$endmismatch = 0;
#		    }
#		    else {
#			my $nse = $listref->[$i];
#			if( $p == 2 ) {
			    # this has failed before, something wrong
#			    die "\nFATAL: failed to pfetch [$nse]\n";
#			}
#			push( @{$pfetchids[2]}, $listref->[$i] );
#			$i++;
#		    }
#		}
#		else {
#		    print FA "$_";
#		}
#	    }

#	    $fh -> close or die "\nFATAL: pfetch failure\n";;
#	}

#	warn "INFO: seen $seen_count sequences\n";
#	if( $seen_count != $nse_count ) {
#	    die "FATAL: failed to pfetch some sequences\n";
#	}

    }
    close FA;
}
undef( $seqlist );             # free up memory

my $command = "/pfam/db/Rfam/bin/linux/cmsearch";
my $options = "";
$options .= "--local " if( $local );
$options .= "-W $window";

$name = "cm$$" if( not $name );
system "cp CM $$.CM" and die;

&printlog( "Queueing cmsearch jobs" );

my $fh = IO::File->new();
# preexec script copies files across and then tests for their presence
# if this fails then the job should reschedule for another go
$fh -> open( "| bsub -q $queue -o /pfam/db/Rfam/tmp/log/$$/$$.err.\%I -E '/pfam/db/Rfam/scripts/make/rfsearch_preexec.pl $phost $pwd $$.minidb.\$\{LSB_JOBINDEX\} $$.CM' -J$name\"[1-$k]\"" ) or die "$!";
$fh -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find lsrcp
$fh -> print( "$command $options /tmp/$$.CM /tmp/$$.minidb.\$\{LSB_JOBINDEX\} > /tmp/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n" );
$fh -> print( "lsrcp /tmp/$$.OUTPUT.\$\{LSB_JOBINDEX\} $phost:$pwd/OUTPUT.\$\{LSB_JOBINDEX\}\n" );
$fh -> print( "rm -f /tmp/$$.minidb.\$\{LSB_JOBINDEX\} /tmp/$$.OUTPUT.\$\{LSB_JOBINDEX\} /tmp/$$.CM\n" );
$fh -> close;


# send something to clean up
$fh = new IO::File;
$fh -> open("| bsub -q $queue2 -w\'done($name)\'") or die "$!";
$fh -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find lsrcp
$fh -> print("date >> /tmp/$$.cmerr\n");
$fh -> print("lsrcp /tmp/$$.cmerr $phost:$pwd/CMSEARCH_JOBS_COMPLETE\n");
$fh -> print("rm -f /tmp/$$.cmerr\n");
$fh -> close;

&update_desc( $buildopts, $options ) unless( !-e "DESC" );


##############

sub printlog {
    my $m = join( '', @_ );
    my $time = `date`;
    chomp $time;
    open( LOG, ">>rfsearch.log" ) or die;
    if( $m ) {
        printf LOG "%-35s [%s]\n", $m, $time;
        printf STDERR "%-35s [%s]\n", $m, $time;
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
    my $buildopts = shift;
    my $searchopts = shift;
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
