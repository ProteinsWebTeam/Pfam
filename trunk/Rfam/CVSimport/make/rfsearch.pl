#!/usr/local/bin/perl -w

use strict;
use Getopt::Long;
use lib '/nfs/disk100/pubseq/Pfam/bioperl';
use Bio::Tools::BPlite;
use Bio::Index::Fasta;
use lib '/nfs/disk100/pubseq/Pfam/scripts/Modules';
use Bio::SimpleAlign;

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
    $help );

sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches covariance model against sequence database

Usage:   rfsearch.pl <options> <fasta file>
Options:       -h              show this help
               -q              don't tell me what you're doing
               -db <blastdb>   sequence database, else use rfamseq
	       -inx <bpindex>  bioperl index of seq db
	       -blast <file>   use precomputed blast output
	       -evalue <n>     use blast evalue of <n>
               -noblast        do full covels search, no blast preproc (not implemented)
               -cpu <n>        use <n> cpus for the covels step
               -queue <queue>  use lsf queue <queue> for the covels step
               -bqueue <queue> use lsf queue <queue> for the blast jobs
	       -local          run cmsearch with --local option
	       -window <n>     window size <n> basepairs

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
             "window=s" => \$window,
	     "h"        => \$help );


# defaults
$blast_eval = 10  unless $blast_eval;
$window     = 100 unless $window;
$cpus       = 1   unless $cpus;
$inxfile    = '/pfam/db/rfamseq/rfamseq.fa.bpi' unless $inxfile;
my $blastdbdir = '/pfam/db/rfamseq';
my $pathtocove = '/nfs/farm/Pfam/bin';

my $fafile  = shift;
my $seqinx  = Bio::Index::Fasta->new( $inxfile ); 

END {
    # random wierdness - this stops core dump on exit
    undef $seqinx;
}

if( $help or not $fafile ) {
    &help();
    exit(1);
}
if( $cpus > 1 and not $queue ) {
    &help();
    print STDERR "If you specify -cpu then you also need the -queue option\n";
    exit(1);
}
	
unless( $blast_outfile ) {
    my @blastdbs;
    if( $blastdb ) {
	push( @blastdbs, $blastdb );
    }
    else {
	if( not $blastdbdir ) {
	    &help();
	    print STDERR "You must set your BLASTDB environment variable to something sensible\n\n";
	    exit(1);
	}
	@blastdbs = ( "$blastdbdir/fun.fa",
		      "$blastdbdir/hum01.fa",
		      "$blastdbdir/hum02.fa",
		      "$blastdbdir/hum03.fa",
		      "$blastdbdir/hum04.fa",
		      "$blastdbdir/hum05.fa",
		      "$blastdbdir/hum06.fa",
		      "$blastdbdir/hum07.fa",
		      "$blastdbdir/hum08.fa",
		      "$blastdbdir/hum09.fa",
		      "$blastdbdir/hum10.fa",
		      "$blastdbdir/hum11.fa",
		      "$blastdbdir/inv01.fa",
		      "$blastdbdir/inv02.fa",
		      "$blastdbdir/mam.fa",
		      "$blastdbdir/mus.fa",
		      "$blastdbdir/org.fa",
		      "$blastdbdir/phg.fa",
		      "$blastdbdir/pln.fa",
		      "$blastdbdir/pro01.fa",
		      "$blastdbdir/pro02.fa",
		      "$blastdbdir/pro03.fa",
		      "$blastdbdir/rod.fa",
		      "$blastdbdir/vrl.fa",
		      "$blastdbdir/vrt.fa" );
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

print STDERR "searching minidb with cove model ... " unless $quiet;
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
	    print DNEW "SM   covels $options\n";
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


