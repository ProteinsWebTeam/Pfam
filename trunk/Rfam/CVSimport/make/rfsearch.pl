#!/usr/local/bin/perl -w

# This is mostly pretty NFS friendly now.  Should run rfsearch itself on 
# something with local storage, but the jobs all get sent off to blades
# using nice socket things, pfetch etc.  Blast searches are still bad, 
# but not sure how to fix at the moment.

use strict;
use Getopt::Long;
use IO::File;

use Bio::Index::Fasta;
use Rfam;

my( $quiet, 
    $nobuild, 
    $queue, 
    $bqueue,
    $window,
    $blast_eval,
    $local,
    $global,
    $name,
    $cpus,
    $blast,
    $help,
    $update );


sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches covariance model against sequence database

Usage:   rfsearch.pl <options>
Options:       -h              show this help
	       -e <n>          use blast evalue of <n>
               -q <queue>      use lsf queue <queue> for the cmsearch step
               -bq <queue>     use lsf queue <queue> for the blast jobs (DISABLED!)
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

if( $bqueue ) {
    warn "The --bq option has been disabled for now - its use\nwill do more harm than good now!\n";
}

my $buildopts;
if( -s "DESC" ) {
    open( D, "DESC" ) or die "DESC exists but I can't open it";
    while( <D> ) {
	/^BM\s+cmbuild\s+(.*)\s*/ and do {
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

$buildopts = "CM SEED" unless $buildopts;
open( S, "SEED" ) or die;
while(<S>) {
    if( /^\#=GC RF/ and $buildopts !~ /--rf/ ) {
	$buildopts = "--rf $buildopts";
    }
}   

# defaults
my $blastdbdir  = $Rfam::rfamseq_current_dir;  # glob files from here
my $blastdbdir2 = "/data/blastdb/Rfam/Large";  # but run things from here
$blast_eval = 10  unless $blast_eval;
$window     = 100 unless $window;
$cpus       = 20  unless $cpus;
$queue      = "pfam_slow -Rlinux" unless $queue;
$bqueue     = "pfam_slow -R 'select[largedata]'"; # unless $bqueue;
my $fafile = "FA";

system "sreformat fasta SEED > $fafile" and die "can't convert SEED to $fafile";
unless( $nobuild ) {
    print STDERR "building model ... ";
    system "/pfam/db/Rfam/bin/cmbuild -F $buildopts" and die "can't build CM from SEED";
    print STDERR "done\n";
}

my $i = 0;
my $pwd   = `pwd`;
my $phost = `uname -n`;
chomp $pwd;
chomp $phost;

my @blastdb;
if( $update ) {      # run over the new.fa.* databases
    @blastdb = glob( "$blastdbdir/new.fa.*[0-9]" );
    &update_output( "OUTPUT", "OUTPUT.0" );
}
else {               # run over *.fa databases
    @blastdb = glob( "$blastdbdir/*.fa" );
}    

unless( $blast ) {
    print STDERR "Queuing up blast jobs ...\n";
    foreach my $blastdb ( @blastdb ) {
	$i ++;
	my( $div ) = $blastdb =~ /$blastdbdir\/(\S+)$/;
	my $fh = new IO::File;
	$fh -> open("| bsub -q $bqueue -o $div.berr -J\"rf$$\"") or die "$!";
	$fh -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find lsrcp
	$fh -> print("rfamseq_blast.pl -e $blast_eval --db $blastdbdir2/$div -l $fafile > /tmp/$$.blastlist.$i\n");
	$fh -> print("lsrcp /tmp/$$.blastlist.$i $phost:$pwd/$$.blastlist.$i\n");
	$fh -> print("rm -f /tmp/$$.blastlist.$i\n");
	$fh -> close;
    }

    print STDERR "Waiting for blast jobs ...\n";
    my $fh = new IO::File;
    $fh -> open("| bsub -I -q pfam_fast -w\'done(rf$$)\'") or die "$!";
    $fh -> print("echo \"blast jobs finished at:\" > $$.berr\n");
    $fh -> print("date >> $$.berr\n");
    $fh -> close;
}

print STDERR "parsing blast list ... ";
my $seqlist = {};
if( $blast ) {
    $seqlist = &parse_list( $seqlist, "$blast" );
}
else {
    for( my $j=1; $j<=$i; $j++ ) {
	$seqlist = &parse_list( $seqlist, "$$.blastlist.$j" );
    }
}
print STDERR "done\n";

print STDERR "building mini database ... ";
my $numseqs = scalar( keys %{ $seqlist } );
my $count = int( $numseqs/$cpus ) + 1;
my $k = 0;
my @seqids = keys %{ $seqlist };

#open( T, ">tmp.sam" );

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

    open( FA, "> $$.minidb.$k" ) or die;
    while( @nses ) {
	my @pfetchids = ( [ splice( @nses, 0, 1000 ) ],   # first round of fetches
			  [],                             # end > length failures
			  [] );                           # version failures

	my $seen_count = 0;

	for( my $p=0; $p<@pfetchids; $p++ ) {
	    my $listref = $pfetchids[$p];
	    next unless( @{$listref} );

	    my $options = "";
	    if( $p == (@pfetchids-1) ) {
		# we're retreiving the version failures
		$options .= "-a";
	    }

	    my $str = join( ' ', @{$listref} );
	    my $fh = IO::File->new();
	    $fh -> open( "pfetch $options $str |" );
#	    print "pfetch $options $str\n";

	    my $i = 0;
	    my( $endmismatch );

	    while(<$fh>) {
#		print;
		if( my( $pfetchid ) = /^\>\S+\s+(\S+)/ ) {
		    my $nse = $listref->[$i];
		    $nse =~ s/:/\//g;
		    my( $tmpid ) = $nse =~ /(\S+\.\d+)\/\d+-\d+/;
		    if( $pfetchid ne $tmpid ) {
			# catch stupid problems
			die "\nFATAL: pfetch id problem - [$nse] mismatch [$pfetchid]\nReport this!\n";
		    }
		    print FA ">$nse\n";
		    $i ++;
		    $seen_count ++;
		    $endmismatch = 0;
		}
		elsif( my( $end ) = /end is greater than sequence length.*?(\d+)/ ) {
		    my $nse = $listref->[$i];
		    if( $p == 1 ) {
			# this has failed before, something wrong
			die "\nFATAL: failed to pfetch [$nse]\n";
		    }
		    my( $tmp ) = $nse =~ /(\S+\.\d+\:\d+-)\d+/;
		    push( @{$pfetchids[1]}, "$tmp$end" );
		    $i++;
		    $endmismatch = 1;
		}
		elsif( /no match/ ) {
		    if( $endmismatch ) {
			# we've already dealt with this sequence
			$endmismatch = 0;
		    }
		    else {
			my $nse = $listref->[$i];
			if( $p == 2 ) {
			    # this has failed before, something wrong
			    die "\nFATAL: failed to pfetch [$nse]\n";
			}
			push( @{$pfetchids[2]}, $listref->[$i] );
			$i++;
		    }
		}
		else {
		    print FA "$_";
		}
	    }
	    $fh -> close;
	}

#	print "$seen_count\n";
#	if( $seen_count != @{$pfetchids[0]} ) {
#	    die "FATAL: failed to pfetch some sequences\n";
#	}

    }
    close FA;
}
print STDERR "done\n";
undef( $seqlist );             # free up memory

my $command = "/pfam/db/Rfam/bin/linux/cmsearch";
my $options = "";
$options .= "--local " if( $local );
$options .= "-W $window";

$name = "" if( not $name );
print STDERR "Queueing cmsearch jobs ...\n";
my $fh = IO::File->new();
$fh -> open( "| bsub -q $queue -o $$.err.\%I -J$name\"[1-$k]\"" ) or die "$!";
$fh -> print(". /usr/local/lsf/conf/profile.lsf\n");   # so we can find lsrcp
$fh -> print( "lsrcp $phost:$pwd/$$.minidb.\$\{LSB_JOBINDEX\} /tmp/$$.minidb.\$\{LSB_JOBINDEX\}\n" );
$fh -> print( "$command $options CM /tmp/$$.minidb.\$\{LSB_JOBINDEX\} > /tmp/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n" );
$fh -> print( "lsrcp /tmp/$$.OUTPUT.\$\{LSB_JOBINDEX\} $phost:$pwd/OUTPUT.\$\{LSB_JOBINDEX\}\n" );
$fh -> print( "rm -f /tmp/$$.minidb.\$\{LSB_JOBINDEX\} /tmp/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n" );
$fh -> close;

&update_desc( $options ) unless( !-e "DESC" );


##############

sub parse_list {
    my $list       = shift;
    my $blastfile  = shift;
    open( BL, $blastfile ) or die;
    while( <BL> ) {
	if( my( $name, $start, $end ) = /^(\S+)\s+(\d+)\s+(\d+)/ ) {
	    # add window length onto each end
	    $start = $start - $window;
	    $end   = $end   + $window;
	    $start = 1 if( $start < 1 );

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

	    if( $already ) {
		print "SKIP\n";
	    }
	    else {
		push( @{ $list->{$name} }, { 'start' => $start,
					     'end'   => $end } );
		print "KEEP\n";
	    }
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
    my $options = shift;
    open( DNEW, ">DESC.new" ) or die;
    open( DESC, "DESC" ) or die;
    while(<DESC>) {
	if( /^BM   cmsearch\s+/ ) {
	    print DNEW "BM   cmsearch $options CM SEQDB\n";
	    next;
	}
	print DNEW $_;
    }
    close DESC;
    close DNEW;
    rename( "DESC", "DESC.old" ) or die;
    rename( "DESC.new", "DESC" ) or die;
}
