#!/usr/bin/env perl

#make the uniprot fasta file and SSI index
use strict;
use warnings;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use File::Copy;
use Cwd;
use Config::General qw(SaveConfig);
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

#Database connection stuff
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

#User options
my ($status_dir, $pfamseq_dir, $move);
&GetOptions(
  "status_dir=s"  => \$status_dir,
  "pfamseq_dir=s" => \$pfamseq_dir);

unless ( $status_dir and -e $status_dir ) {
  help();
}
unless ( $pfamseq_dir and -e $pfamseq_dir ) {
  help();
}
my $cwd = getcwd();


#Create fasta file from uniprot table
#if this is too slow/fails it could be worth trying a smaller offset or sending it to the farm with plenty of memory
my $total=0;
if(-s "$pfamseq_dir/uniprot") {
  $logger->debug("Already made uniprot fasta file");
  my $sth=$dbh->prepare("select count(*) from uniprot") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
  $sth->execute() or $logger->logdie("Couldn't execute statement ".$sth->errstr);
  $total=$sth->fetchrow();
}
else {
  $logger->debug("Going to make fasta file from uniprot table");
  my $offset = 0;
  my $n = 1;
  open (FA, ">$pfamseq_dir/uniprot") or $logger->logdie("Cannot open $pfamseq_dir/uniprot file to write");
  while (1){
    $logger->debug("Querying uniprot table in database.... chunk $n offset $offset ");
    my $st = $dbh->prepare("select uniprot_acc, seq_version, uniprot_id, description, sequence from uniprot limit 1000000 offset $offset") or $logger->logdie("Failed to prepare statement:".$dbh->errstr);
    $st->execute() or $logger->logdie("Couldn't execute statement ".$st->errstr);
    my $rowno = $st->rows;
    last unless($rowno);
    my $array_ref = $st->fetchall_arrayref();
    $logger->debug("Fetching results...");
    foreach my $row (@$array_ref) {
      print FA ">" . $row->[0] . "." . $row->[1] . " " . $row->[2] . " " . $row->[3] . "\n" . $row->[4] . "\n";
    }
    $offset += 1000000;
    $total += $rowno;
    $n++;
    $logger->debug("$total rows retrieved");
  }
  close FA;
  $dbh->disconnect;
}


#Make easel indexes
if(-e "$status_dir/esl-indexes_uniprot") { 
  $logger->debug("Already make easel indexes");
} else {
  $logger->debug("Making easel indices for uniprot");
  chdir($pfamseq_dir) or $logger->logdie("Couldn't chdir into $pfamseq_dir:[$!]");
  system ("esl-sfetch --index uniprot") and $logger->logdie("Couldn't make easel indices for uniprot:[$!]");
  chdir($cwd) or $logger->logdie("Couldn't chdir to $cwd:[$!]");
  system("touch $status_dir/esl-indexes_uniprot") and $logger->logdie("Couldn't touch $status_dir/esl-indexes_uniprot:[$!]\n");
}


sub help{

  print STDERR << "EOF";

This script creates a fasta file from the sequences in the uniprot
table in the database.

Usage:

  $0 -status_dir <status_dir> -pfamseq_dir <pfamseq_dir>

Options
  -help  :Prints this help message

Both the status directory and pfamseq_directory must already exist.
pfamseq_dir is the directory where the pfamseq fasta file will be
generated. The status directory is where a log of the progress of 
the script is recorded.

EOF
}
