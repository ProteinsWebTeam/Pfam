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
    $help );


sub help {
    print STDERR <<EOF;

rfsearch.pl: builds and searches covariance model against sequence database

Usage:   rfsearch.pl <options>
Options:       -h              show this help
               -q              don't tell me what you're doing
	       -e <n>          use blast evalue of <n>
               -q <queue>      use lsf queue <queue> for the cmsearch step
               -bq <queue>     use lsf queue <queue> for the blast jobs
	       -w <n>          window size <n> basepairs
	       --local         run cmsearch with --local option
	       --global        run cmsearch in global mode (override DESC cmsearch command)
	       --nobuild       skip cmbuild step

EOF
}

&GetOptions( "q"        => \$quiet,
	     "e=s"      => \$blast_eval,
	     "q=s"      => \$queue,
	     "bq=s"     => \$bqueue,
	     "local"    => \$local,
	     "global"   => \$global,
             "w=s"      => \$window,
	     "nobuild"  => \$nobuild,
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
my $blastdbdir = $rfamseq_current_dir;
$blast_eval = 10  unless $blast_eval;
$window     = 100 unless $window;
$queue      = "pfam_slow -Rlinux" unless $queue;
$bqueue     = "pfam_slow -Rlinux" unless $bqueue;
$buildopts  = "--rf CM SEED" unless $buildopts;
my $fafile = "FA";

system "sreformat fasta SEED > $fafile" and die "can't convert SEED to FA";
unless( $nobuild ) {
    system "/pfam/db/Rfam/bin/cmbuild -F $buildopts" and die "can't build CM from SEED";
}

my @blastdbs = glob( "$rfamseq_current_dir/*.fa" );

my $i = 0;
foreach my $blastdb ( @blastdbs ) {
    $i ++;
    my( $div ) = $blastdb =~ /\/([a-z0-9]+)\.fa$/;
    system "bsub -q $bqueue -o $div.berr -J\"rf$$\" \'rfamseq_blast.pl -d $div -e $blast_eval --minidb FA > $$.minidb.$i\'" and die;
}

my $command = "/pfam/db/Rfam/bin/linux/cmsearch";
my $options = "";
if( $local ) {
    $options .= "--local";
}
$options .= " -W $window";

system "echo \'$command CM $$.minidb.\$\{LSB_JOBINDEX\} > OUTPUT.\$\{LSB_JOBINDEX\}\' | bsub -q $queue -w\'done(rf$$)\' -o $$.err.\%I -J\"[1-$i]\"" and die;

&update_desc( $options ) unless( !-e "DESC" );


##############

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
