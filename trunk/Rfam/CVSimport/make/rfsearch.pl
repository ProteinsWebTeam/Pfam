#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use lib '/nfs/disk100/pubseq/Pfam/bioperl';
use Bio::Tools::BPlite;
use Bio::Index::Fasta;
use lib '/pfam/db/Rfam/scripts/Modules';
use Rfam;

my( $quiet, 
    $blastdb, 
    $blast_outfile, 
    $nobuild, 
    $noblast,
    $cpus, 
    $queue, 
    $cove,
    $bqueue,
    $window,
    $blast_eval,
    $inxfile,
    $local,
    $global,
    $help );

# -queue must be linux machines at the moment

#my $arch = `uname`;
#if( $arch =~ /linux/i ) {
    $ENV{'PATH'} = "/pfam/db/Rfam/bin/linux:$ENV{'PATH'}"; # push linux binaries onto front of path
#}

sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches covariance model against sequence database

Usage:   rfsearch.pl <options>
Options:       -h              show this help
               -q              don't tell me what you're doing
               -db <blastdb>   sequence database, else use rfamseq
	       -inx <bpindex>  bioperl index of seq db
	       -blast <file>   use precomputed blast output
	       -evalue <n>     use blast evalue of <n>
               -noblast        do full cmsearch search, no blast preproc (not implemented)
               -cpu <n>        use <n> cpus for the cmsearch step
               -queue <queue>  use lsf queue <queue> for the cmsearch step
               -bqueue <queue> use lsf queue <queue> for the blast jobs
	       -local          run cmsearch with --local option
	       -global         run cmsearch in global mode (override DESC cmsearch command)
	       -window <n>     window size <n> basepairs
	       -nobuild        skip cmbuild step

EOF
}

&GetOptions( "q"        => \$quiet,
	     "db=s"     => \$blastdb,  # broken I think
	     "inx=s"    => \$inxfile,
	     "cove"     => \$cove,
	     "blast=s"  => \$blast_outfile,
	     "evalue=s" => \$blast_eval,
	     "noblast"  => \$noblast,
	     "cpu=s"    => \$cpus,
	     "queue=s"  => \$queue,
	     "bqueue=s" => \$bqueue,
	     "local"    => \$local,
	     "global"   => \$global,
             "window=s" => \$window,
	     "nobuild"  => \$nobuild,
	     "h"        => \$help );


$inxfile    = $rfamseq_current_inx unless $inxfile;
my $blastdbdir = $rfamseq_current_dir;

my $seqinx  = Bio::Index::Fasta->new( $inxfile ); 

END {
    # random wierdness - this stops core dump on exit
    undef $seqinx;
}

if( $help or not -e "SEED" ) {
    &help();
    exit(1);
}
if( $cpus > 1 and not $queue ) {
    &help();
    print STDERR "If you specify -cpu then you also need the -queue option\n";
    exit(1);
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

# defaults
$blast_eval = 10  unless $blast_eval;
$window     = 100 unless $window;
$cpus       = 1   unless $cpus;
$buildopts  = "--rf CM SEED" unless $buildopts;

my $fafile = "FA";

unless( $blast_outfile ) {
    print STDERR "making fasta file ... \n" unless $quiet;
    system "sreformat fasta SEED > $fafile" and die "can't convert SEED to FA";
    unless( $nobuild ) {
	print STDERR "build covariance model ... \n" unless $quiet;
	system "/pfam/db/Rfam/bin/cmbuild -F $buildopts" and die "can't build CM from SEED";
    }

    my @blastdbs;
    if( $blastdb ) {
	push( @blastdbs, $blastdb );
    }
    else {
	@blastdbs = glob( "$rfamseq_current_dir/*.fa" );
    }

    print STDERR "running blast search ... \n" unless $quiet;
    if( $bqueue ) {
	my( @children, @blastfiles );
	foreach my $blastdb ( @blastdbs ) {
	    my $pid;
	  FORK: {
	      if( $pid = fork ) {
		  # parent
		  push( @children, $pid );
	      }
	      elsif( defined $pid ) {
		  # child
		  my $blastfile = &run_blast_search( $fafile, $blastdb, $blast_eval );
		  push( @blastfiles, $blastfile );
		  exit(0);
	      }
	      elsif( $! =~ /No more processes/ ) {
		  sleep 5;
		  redo FORK;
	      }
	      else {
		  die "Cannot fork: $!\n";
	      }
	  }
	}
	$blast_outfile = "$$.blast";
	foreach my $pid ( @children ) {
	    waitpid $pid, 0;
	    system "cat $pid.blast >> $blast_outfile" and die;
	}
    }    
    else {
	foreach my $blastdb ( @blastdbs ) {
	    $blast_outfile = &run_blast_search( $fafile, $blastdb, $blast_eval );
	}
    }
    print STDERR "done\n" unless $quiet;
}

print STDERR "parsing blast output ... " unless $quiet;
my %seqlist = %{ &parse_blast( $blast_outfile ) };
print STDERR "done\n" unless $quiet;

print STDERR "building mini database ... " unless $quiet;
my $numseqs = scalar( keys %seqlist );
my $count = int( $numseqs/$cpus ) + 1;
my $i = 1;
my @seqids = sort keys %seqlist;
while( @seqids ) {
    my @tmpids = splice( @seqids, 0, $count ); 
    open( FA, "> $$.minidb.$i" ) or die;
    foreach my $seqid ( @tmpids ) {
	foreach my $reg ( @{ $seqlist{ $seqid } } ) {
#	    print $seqid, " ", $reg->{'start'}, " ", $reg->{'end'}, "\n";
#	    my $start = $reg->{'start'} - $window;
#	    my $end   = $reg->{'end'}   + $window;
#	    $start = 1 if( $start < 1 );
	    my $seq = &get_seq( $seqid, $reg->{'start'}, $reg->{'end'} );
	    next if not $seq;
	    my $seqstr = $seq->seq();
	    $seqstr =~ s/(.{1,60})/$1\n/g;
	    print FA ">", $seq->id(), "\n$seqstr";
#	    print $seq->id(), "\n";
	}
    }
    $i++;
    close FA;
}
print STDERR "done\n" unless $quiet;
undef( %seqlist );             # free up memory

my $command;
if( $cove ) {
    $command = "covels -c -w $window -t -100";
}
elsif( $local ) {
    $command = "cmsearch --local -W $window";
}
else {
    $command = "cmsearch -W $window";
}

print STDERR "searching minidb with model ... " unless $quiet;
if( $queue ) {
    system "echo \'$command CM $$.minidb.\$\{LSB_JOBINDEX\} > OUTPUT.\$\{LSB_JOBINDEX\}\' | bsub -q $queue -o $$.err.\%I -J\"[1-$i]\"" and die;
}
else {
    for( my $j=1; $j<=$i; $j++ ) {
	system "$command CM $$.minidb.$j > OUTPUT.$j" and die;
    }
}
print "done\n";

#&update_desc( $options );


##############

sub run_blast_search {
    my $file = shift;
    my $blastdb = shift;
    my $evalue = shift;

    eval {
	if( $bqueue ) {
#	    print STDERR "bsub -I -q $bqueue -e $$.berr blastall -i $file -p blastn -e $evalue -d $blastdb > $$.blast\n";
	    system "bsub -I -q $bqueue -e $$.berr 'echo \"running on \${LSB_HOSTS}\" > $$.berr; blastall -i $file -p blastn -e $evalue -d $blastdb > $$.blast'" and die;
	}
	else {
#	    print STDERR "blastall -i $file -p blastn -e $evalue -d $blastdb >> $$.blast\n";
	    system "blastall -i $file -p blastn -e $evalue -d $blastdb >> $$.blast" and die;
	}
    };
    if( $@ ) {
	die "blast job failed\n[blastall -i $file -p blastn -e $evalue -d $blastdb > $$.blast]\n[$@]\n";
    }

    return "$$.blast";
}

sub parse_blast {

    ##########################################################
    # mmm - do we catch blast hits with end < start properly #
    # seems we still get multiple copies of such things      #
    ##########################################################

    my $blastfile = shift;
    open( BL, $blastfile ) or die;
    my $report = new Bio::Tools::BPlite( -fh => \*BL );
    my %list;
    {
	while( my $sbjct = $report -> nextSbjct ) {
	    my $name = $sbjct -> name();
	    $name =~ /^(\S+)\s+/;
	    $name = $1;
	    while( my $hsp = $sbjct->nextHSP ) {
		my( $start, $end ) = ( $hsp->subject->start, $hsp->subject->end );
#		print "PARSE $name $start $end\n";
		# if reverse strand then swap start and end
		if( $end < $start ) {
		    ( $start, $end ) = ( $end, $start );
		}
		# add window length onto each end
		$start = $start - $window;
		$end   = $end   + $window;
		$start = 1 if( $start < 1 );
		# avoid having multiple copies of one region in minidb
		my $already;
		if( exists $list{ $name } ) {
		    foreach my $se ( sort @{ $list{ $name } } ) {
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
#		print STDERR "$name ".$start." ".$end;
#		print STDERR " $already" if $already;
#		print STDERR "\n";

		unless( $already ) {
		    push( @{ $list{ $name } }, { 'start' => $start,
						 'end'   => $end } );
		}
	    }
	}
	last if ( $report -> _parseHeader == -1 );
	redo;
    }
    return \%list;
}


sub update_desc {
    my $options = shift;
    open( DNEW, ">DESC.new" ) or die;
    open( DESC, "DESC" ) or die;
    while(<DESC>) {
	if( /^SM\s+/ ) {
	    print DNEW "SM   cmsearch $options\n";
	    next;
	}
	print DNEW $_;
    }
    close DESC;
    close DNEW;
    rename( "DESC", "DESC.old" ) or die;
    rename( "DESC.new", "DESC" ) or die;
}



sub get_seq {
    # fixes start < 1 and end > length
    my $id    = shift;
    my $start = shift;
    my $end   = shift;

    my $seq = new Bio::Seq;
    eval {
	$seq = $seqinx -> fetch( $id );
    };
    if( not $seq or $@ ) {
	warn "$id not found in your seq db\n";
	return 0;       # failure
    }
    my $length = $seq -> length();
    if( $start < 1 ) {
	$start = 1;
    }
    if( $end > $length ) {
	$end = $length;
    }
    my $truncseq = $seq -> trunc( $start, $end );
    $truncseq -> desc( "" );
    $truncseq -> id( "$id/$start-$end" );
#    my $str = $truncseq->seq();
#    $str =~ s/(.{1,60})/$1\n/g;

#    print $fh ">", $truncseq->id(), "\n$str";
    return $truncseq;
}


