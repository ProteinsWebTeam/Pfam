#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $rfam_mod_dir;
use lib $bioperl_dir;

use strict;
use Getopt::Long;
use IO::File;

use Bio::Tools::BPlite;
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
    $cpus,
    $blast,
    $help );


sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches covariance model against sequence database

Usage:   rfsearch.pl <options>
Options:       -h              show this help
	       -e <n>          use blast evalue of <n>
               -q <queue>      use lsf queue <queue> for the cmsearch step
               -bq <queue>     use lsf queue <queue> for the blast jobs
	       -w <n>          window size <n> basepairs
	       --local         run cmsearch with --local option
	       --global        run cmsearch in global mode (override DESC cmsearch command)
	       --cpu           number of cpus to run cmsearch job over
	       --nobuild       skip cmbuild step

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
	     "blast=s"  => \$blast,
	     "h"        => \$help );

if( $help or not -e "SEED" ) {
    &help();
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
my $blastdbdir = $Rfam::rfamseq_current_dir;
my $inxfile    = $Rfam::rfamseq_current_inx;
$blast_eval = 10  unless $blast_eval;
$window     = 100 unless $window;
$cpus       = 20  unless $cpus;
$queue      = "pfam_slow -Rlinux" unless $queue;
$bqueue     = "pfam_slow -Rlinux" unless $bqueue;
$buildopts  = "--rf CM SEED" unless $buildopts;
my $fafile = "FA";

my $seqinx  = Bio::Index::Fasta->new( '-filename'    => $inxfile,
                                      '-dbm_package' => 'DB_File' ); 
END { undef $seqinx; }   # stop bizarre seg faults

print STDERR "building model ... ";
system "sreformat fasta SEED > $fafile" and die "can't convert SEED to $fafile";
unless( $nobuild ) {
    system "/pfam/db/Rfam/bin/cmbuild -F $buildopts" and die "can't build CM from SEED";
}
print STDERR "done\n";

print STDERR "Queuing up blast jobs ...\n";
my $i = 0;
my $fh = new IO::File;

unless( $blast ) {
    foreach my $blastdb ( glob( "$blastdbdir/*.fa" ) ) {
	$i ++;
	my( $div ) = $blastdb =~ /\/([a-z0-9]+)\.fa$/;
	$fh -> open("| bsub -q $bqueue -o $div.berr -J\"rf$$\" -f \"$$.blast.$i < /tmp/$$.blast.$i\"") or die "$!";
	$fh -> print("blastall -b 100000 -v 100000 -p blastn -i $fafile -e $blast_eval -F F -W 7 -d $blastdb > /tmp/$$.blast.$i\n");
	$fh -> close;
    }

    print STDERR "Waiting for blast jobs ...\n";
    my $fh = new IO::File;
    $fh -> open("| bsub -I -q pfam_fast -Ralpha -w\'done(rf$$)\'") or die "$!";
    $fh -> print("cat $$.blast.* >> $$.blastall\n");
    $fh -> close;
}

$blast = "$$.blastall" if( not $blast );

print STDERR "parsing blast output ... ";
my %seqlist = %{ &parse_blast( $blast ) };
print STDERR "done\n";

print STDERR "building mini database ... ";
my $numseqs = scalar( keys %seqlist );
my $count = int( $numseqs/$cpus ) + 1;
my $k = 1;
my @seqids = sort keys %seqlist;
while( @seqids ) {
    my @tmpids = splice( @seqids, 0, $count ); 
    open( FA, "> $$.minidb.$k" ) or die;
    foreach my $seqid ( @tmpids ) {
        foreach my $reg ( @{ $seqlist{ $seqid } } ) {
            my $seq = &get_seq( $seqid, $reg->{'start'}, $reg->{'end'} );
            next if not $seq;
            my $seqstr = $seq->seq();
            $seqstr =~ s/(.{1,60})/$1\n/g;
            print FA ">", $seq->id(), "\n$seqstr";
        }
    }
    $k++;
    close FA;
}
print STDERR "done\n";
undef( %seqlist );             # free up memory

my $command = "/pfam/db/Rfam/bin/linux/cmsearch";
my $options = "";
if( $local ) {
    $options .= "--local";
}
$options .= " -W $window";

print STDERR "Queueing cmsearch jobs ...\n";
$fh -> open("| bsub -q $queue -o $$.err.\%I -J\"[1-$k]\" -f \"$$.minidb.\%I > /tmp/$$.minidb.\%I\" -f \"OUTPUT.\%I < /tmp/$$.OUTPUT.\%I\"") or die "$!";
$fh -> print("$command $options CM /tmp/$$.minidb.\$\{LSB_JOBINDEX\} > /tmp/$$.OUTPUT.\$\{LSB_JOBINDEX\}\n");
$fh -> close;

&update_desc( $options ) unless( !-e "DESC" );


##############

sub parse_blast {
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
    return $truncseq;
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
