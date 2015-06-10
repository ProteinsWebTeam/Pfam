#!/usr/bin/env perl

#make the pfamseq fasta file and SSI index

use strict;
use warnings;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use File::Copy;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

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
  or $logger->logdie("Invalid option!");

unless ( $statusdir and -e $statusdir ) {
  help();
}
unless ( $pfamseq_dir and -e $pfamseq_dir ) {
  help();
}

#get DBSIZE file
open(DB, "$pfamseq_dir/DBSIZE") or $logger->logdie("Could not open $pfamseq_dir/DBSIZE:[$!]");
my $dbsize;
while(<DB>){
    ($dbsize) = $_ =~/(\S+)/;
    last;
}
close DB;

#get sequences from pfamseq
#if this is too slow/fails it could be worth trying a smaller offset or sending it to the farm with plenty of memory
my $offset = 0;
my $n = 1;
my $total = 0;
open (FA, ">$pfamseq_dir/pfamseq") or $logger->logdie("Cannot open pfamseq file to write");
while ($total < $dbsize){
    $logger->debug("Querying database.... chunk $n offset $offset ");
    my $st = $dbh->prepare("select pfamseq_acc, seq_version, pfamseq_id, description, sequence from pfamseq limit 1000000 offset $offset") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
    $st->execute() or $logger->logdie("Couldn't execute statement ".$st->errstr);
    my $rowno = $st->rows;

    my $array_ref = $st->fetchall_arrayref();
    $logger->debug("Fetching results...");
    foreach my $row (@$array_ref) {
      print FA ">" . $row->[0] . "." . $row->[1] . " " . $row->[2] . " " . $row->[3] . "\n" . $row->[4] . "\n";
    }
    $offset = $offset + 1000000;
    $total = $total + $rowno;
    $n++;
    $logger->debug("$total rows retrieved");
}
close FA;
$dbh->disconnect;

#make easel indexes
if(-e "$statusdir/esl-indexes") { 
    $logger->debug("Already make easel indexes");
} else {
    $logger->debug("Making easel indices for pfamseq");
    chdir($pfamseq_dir) or $logger->logdie("Couldn't chdir into $pfamseq_dir:[$!]");
    system ("esl-sfetch --index pfamseq") and $logger->logdie("Couldn't make easel indices for pfamseq:[$!]");
    $logger->debug("Making NCBI indices for pfamseq");
    system ("formatdb -p T -i pfamseq") and $logger->logdie("Couldn't make NCBI-blast indices for pfamseq:[$!]");
    chdir("../") or $logger->logdie("Couldn't chdir up from $pfamseq_dir:[$!]");
    system("touch $statusdir/esl-indexes") and $logger->logdie("Couldn't touch $statusdir/esl-indexes:[$!]\n");
}

##copy pfamseq to nfs
#if ("$statusdir/moved_pfamseq"){
#    $logger->debug("Already moved pfamseq to nfs");
#} else {
#    $logger->info("Copying pfamseq to nfs directory");
#    my $pfamseq_nfs = $config->{pfamseq}->{location};
# 
#    unlink glob("$pfamseq_nfs/*") or $logger->logdie("Problem deleting files in $pfamseq_nfs");
#    my @pfamseq_files = qw(pfamseq pfamseq.pal pfamseq.ssi);
#    open(PAL, "$pfamseq_dir/pfamseq.pal") or $logger->logdie("Could not open $pfamseq_dir/pfamseq.pal:[$!]");
#    while(<PAL>){
#	if(/^DBLIST\s+(.*)/){
#	    my $l = $1; #$l contains the list of virtual databases;
#	    foreach my $bit (split(/\s+/, $l)){
#		foreach my $ext (qw(phr pin psq)){
#		    push(@pfamseq_files, $bit.".".$ext);
#		}
#	    }
#	}
#    }
#    close(PAL);
#
#
#    foreach my $f (@pfamseq_files) {i
#	$logger->debug("Copying $f");
#	copy("$pfamseq_dir/$f", "$pfamseq_nfs/$f") or $logger->logdie("Copy $pfamseq_dir/$f to $pfamseq_nfs failed: $!");
#    }
#    system("touch $statusdir/moved_pfamseq") and $logger->logdie("Couldn't touch $statusdir/moved_pfamseq:[$!]\n");
#}
#
##update DBSIZE file
#move ("$pfamseq_dir/DSIZE", "$pfamseq_dir/DBSIZE_preantifam") or die "Cannot move $pfamseq_dir/DBSIZE to $pfamseq_dir/DBSIZE_preantifam\n";
#open (DBSIZE, ">$pfamseq_dir/DBSIZENEW") or die "Cannot open new $pfamseq_dir/DBSIZE file to write\n";
#print DBSIZENEW "$total\n";
#close DBSIZENEW;



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
