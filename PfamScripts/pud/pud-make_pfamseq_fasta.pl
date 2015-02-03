#!/usr/bin/env perl

#make the pfamseq fasta file and SSI index

use strict;
use warnings;
use Cwd;
use LWP::UserAgent;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use Archive::Tar;
use File::Touch;
use File::Copy;
use Bio::Pfam::Config;

my ( $statusdir, $pfamseq_dir );

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

#Database connection stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

&GetOptions(
  "status_dir=s"  => \$statusdir,
  "pfamseq_dir=s" => \$pfamseq_dir
  )
  or $logger->logdie("Invalid option!\n");

unless ( $statusdir and -e $statusdir ) {
  help();
}
unless ( $pfamseq_dir and -e $pfamseq_dir ) {
  help();
}

my $pwd = cwd;
chdir($pfamseq_dir)
  or $logger->logdie("Couldn't change directory into $pfamseq_dir $!");


#get DBSIZE file
open(DB, "DBSIZE") or $logger->logdie("Could not open DBSIZE:[$!]");
my $dbsize;
while(<DB>){
    ($dbsize) = $_ =~/(\S+)/;
    last;
}

#get sequences from pfamseq
my $offset = 0;
my $n = 1;
my $total = 0;

while ($total < $dbsize){
    $logger->debug("Querying database.... chunk $n offset $offset ");
    my $st = $dbh->prepare("select pfamseq_acc, seq_version, pfamseq_id, description, sequence from pfamseq limit 1000000 offset $offset") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
    $st->execute() or $logger->logdie("Couldn't execute statement ".$st->errstr);
    my $rowno = $st->rows;

    my $array_ref = $st->fetchall_arrayref();
    open (FA, ">pfamseq") or $logger->logdie("Cannot open pfamseq file to write");
    $logger->debug("Fetching results...\n");
    foreach my $row (@$array_ref) {

	print FA ">" . $row->[0] . "." . $row->[1] . " " . $row->[2] . " " . $row->[3] . "\n" . $row->[4] . "\n";
    }
    close FA;
    $offset = $offset + 1000000;
    $total = $total + $rowno;
    $n++;
    $logger->debug("$total rows retrieved\n");
}
$dbh->disconnect;

#make easel indexes
if(-e "$statusdir/esl-indexes") { 
    $logger->debug("Already make easel indexes");
} else {
    $logger->debug("Making easel indices for pfamseq");
    system ("esl-sfetch --index pfamseq") and $logger->logdie("Couldn't make easel indices for pfamseq:[$!]");
    $logger->debug("Making NCBI indices for pfamseq");
    system ("formatdb -p T -i pfamseq") and $logger->logdie("Couldn't make NCBI-blast indices for pfamseq:[$!]");
    system("touch $statusdir/esl-indexes") and $logger->logdie("Couldn't touch $statusdir/esl-indexes:[$!]\n");
}

#copy pfamseq to nfs
if ("$statusdir/moved_pfamseq"){
    $logger->debug("Already moved pfamseq to nfs");
} else {
    $logger->info("Copying pfamseq to nfs directory\n");
    my $pfamseq_nfs = $config->{pfamseq}->{location};

    unlink glob("$pfamseq_nfs/*") or $logger->logdie("Problem deleting files in $pfamseq_nfs");
    my @pfamseq_files = qw(pfamseq pfamseq.pal pfamseq.ssi);
    open(PAL, "pfamseq.pal") or $logger->logdie("Could not open pfamseq.pal:[$!]");
    while(<PAL>){
	if(/^DBLIST\s+(.*)/){
	    my $l = $1; #$l contains the list of virtual databases;
	    foreach my $bit (split(/\s+/, $l)){
		foreach my $ext (qw(phr pin psq)){
		    push(@pfamseq_files, $bit.".".$ext);
		}
	    }
	}
    }
    close(PAL);


    foreach my $f (@pfamseq_files) {
	$logger->debug("Copying $f");
	copy($f, $pfamseq_nfs."/".$f) or $logger->logdie("Copy $f to $pfamseq_nfs failed: $!");
    }
    system("touch $statusdir/moved_pfamseq") and $logger->logdie("Couldn't touch $statusdir/moved_pfmaseq:[$!]\n");
}

sub help{

print STDERR << "EOF";

This script extracts sequences from pfamseq, writes a fasta file and easel indexes, and moves to nfs.
Usage:

  $0 -status_dir <status_dir> -pfamseq_dir <pfamseq_dir>

Both the status directory and pfamseq_directory must already exist.
pfamseq_dir is the directory where the uniprot data will be downloaded
to.  The status directory is where a log of the progress of the script
is recorded.

EOF
}
