#! /usr/bin/env perl 

#Script to run jackhmmer on all proteins in a particular
#proteome that do not have a match to Pfam-A

use strict;
use warnings;
use Getopt::Long;
use Log::Log4perl qw(:easy);

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


my ($ncbi_tax, $max_jobs, $help);


&GetOptions( "ncbi_tax=s" => \$ncbi_tax,
  "max_jobs=i" => \$max_jobs,
  "help" => \$help);


unless($ncbi_tax) {
  print STDERR "Need to specify tax id ($0 -ncbi_tax <ncbi_taxid>)\n";
  &help();

}

my $config = Bio::Pfam::Config->new;

#If running outside of EBI, use the cgi script
unless($config->location eq 'EBI') {
  warn "This script will not work outside of EBI\n";
  exit;
}

if($max_jobs) {
  unless($max_jobs >0) {
    $logger->logdie("max_jobs needs to be a number greater than 0");
  }
}
else {
  $max_jobs = 200;
}

open(SUMMARY, ">$ncbi_tax.txt") or die "Couldn't open fh to $ncbi_tax.txt, $!";


my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

my $dbh = $pfamDB->getSchema->storage->dbh;


#Get species and auto_proteome number
my $st_tax = $dbh->prepare("select species from complete_proteomes where ncbi_taxid = '$ncbi_tax'");

$st_tax->execute or $logger->logdie("Couldn't execute statement ".$st_tax->errstr."\n");

my ($species) = $st_tax->fetchrow;

unless($species) {
  $logger->logdie("Couldn't find the ncbi taxid |$ncbi_tax| in the database"); 
}

$logger->info("Species name for NCBI taxid $ncbi_tax is '$species'");

print SUMMARY "Species name for NCBI taxid $ncbi_tax is '$species'\n";


#Find out which ones have a Pfam-A match
my $st2 = $dbh->prepare("select p.pfamseq_acc from pfamseq p, pfamA_reg_full_significant r where p.pfamseq_acc=r.pfamseq_acc and in_full=1 and ncbi_taxid=$ncbi_tax");
$st2->execute() or $logger->logdie("Couldn't execute statement ". $st2->errstr."\n");

my $pfamA_data = $st2->fetchall_arrayref;
my %pfamA_proteins;
foreach my $row(@$pfamA_data) { 
  $pfamA_proteins{$row->[0]}=1;
}
my $pfamA_count = keys %pfamA_proteins;


#Get total number of proteins in proteome
my $st3 = $dbh->prepare("select pfamseq_acc, seq_version, description, sequence from pfamseq where ncbi_taxid=$ncbi_tax");
$st3->execute() or die $logger->logdie("Couldn't execute statement ".$st3->errstr."\n");

my $all_data = $st3->fetchall_arrayref;
my $protein_count=0;
my %fasta;
foreach my $row(@$all_data) { 
  my ($pfamseq_acc, $seq_version, $desc, $sequence) = ($row->[0], $row->[1], $row->[2], $row->[3]);

  $protein_count++;

  unless(exists($pfamA_proteins{$pfamseq_acc})) {
    $fasta{$pfamseq_acc}= ">$pfamseq_acc" . "." . "$seq_version $desc\n$sequence";
  }
}


$logger->info("$protein_count proteins in proteome");
print SUMMARY "$protein_count proteins in proteome\n";
my $percentage1 = ($pfamA_count/$protein_count)*100;
$percentage1 = sprintf("%.1f", $percentage1);

my $no_pfamA = $protein_count - $pfamA_count;
my $percentage2 = ($no_pfamA/$protein_count)*100;
$percentage2 = sprintf("%.1f", $percentage2);

$logger->info("$pfamA_count/$protein_count ($percentage1\%) proteins have a match to a Pfam-A domain");
print SUMMARY "$pfamA_count/$protein_count ($percentage1\%) proteins have a match to a Pfam-A domain\n";
$logger->info("$no_pfamA/$protein_count ($percentage2\%) proteins do not match a Pfam-A domain");
print SUMMARY "$no_pfamA/$protein_count ($percentage2\%) proteins do not match a Pfam-A domain\n";

$logger->info("Going to run jackhmmer (N=3) on each of the proteins that do not match a Pfam-A");
print SUMMARY "Going to run jackhmmer (N=3) on each of the proteins that do not match a Pfam-A\n";
close SUMMARY;

$logger->info("The maximum number of jobs submitted to the farm at any one time will be $max_jobs\n");

unless(-d "Jackhmmer") {
  mkdir("Jackhmmer", 0755) or $logger->logdie("Couldn't mkdir Jackhmmer, $!");
}
chdir("Jackhmmer") or $logger->logdie("Couldn't chdir into 'Jackhmmer', $!");

my $n = $max_jobs;
my $jobs_submitted=0;

foreach my $acc (keys %fasta) {

  mkdir($acc, 0755) or $logger->logdie("Couldn't mkdir $acc, $!");
  chdir($acc) or $logger->logdie("Couldn't chdir into $acc, $!");

  open(FH, ">seq.fasta") or $logger->die("Couldn't open fh to seq.fasta, $!");
  print FH $fasta{$acc};
  close FH;

  system("pfjbuild -N 3 -fa seq.fasta -chk chk -gzip") and $logger->logdie("Failed to run jackhmmer on $acc, $!");
  $jobs_submitted++;
  chdir("../") or $logger->logdie("Couldn't chdir up a dir from $acc, $!");

  if($jobs_submitted == $no_pfamA) { #All jobs are submitted, exit loop
    $logger->info("All jobs submitted to the farm");
    last;
  }

  $n--;
  while($n<=0) { #See how many jobs are running/pending, and how many more we should submit
    sleep 30; #Give time for last submitted job to appear on 'bjobs'

    my $farm_jobs = count_bjobs();

    $n = $max_jobs - $farm_jobs;
    if($n<=0) { 
      $logger->info("You have $farm_jobs jobs on the farm, waiting for some of the jobs to finish before submitting more");
      sleep 120;
    }
    else {
      $logger->info("You have $farm_jobs jobs on the farm, submitting another $n job(s)");
    }
  }
}



sub count_bjobs {
  my $run=0;

  my $flag;

  open(BJOBS, "bjobs |") or $logger->logdie("Couldn't open fh to bjobs $!");
  while(<BJOBS>) {
    next if(/^No unfinished job found/);
    if(/^JOBID/) {
      $flag=1;
    }

    $run++;
  }
  close BJOBS;

  if($run and !$flag) {
    $logger->info("LSF looks like it is not working, going to wait before submitting more jobs");
    $run = 100000; #Big number prevents more jobs being submitted
  }

  return($run);
}




sub help {

  print STDERR << "EOF";

This program runs jackhmmer on all proteins in a proteome that do not
have a match to a Pfam-A family.  It outputs a file in the cwd
(<ncbi_tax>.txt), stating how many proteins are in the proteome, and
how many of the proteins have a match to a Pfam-A family.  It runs
jackhmmer (N=3) on the proteins that do not have a match to Pfam-A
family.  The jackhmmer searches will be written to a directory called
Jackhmmer in the cwd.  You need to specify the ncbi tax id of the
organism you wish to run the script on on the command line.

Usage:
  $0 -ncbi_tax <ncbi_tax>

Options:
 -max_jobs <n>:Maximum number of jobs to run on the farm at any one time
               (Default 200)


After this script has been run, you should run jackhmmer_summary.pl to
get a summary of how many new sequences will be added to Pfam, and how
many overlaps there are to Pfam-A families.

EOF

  exit (0);

}
