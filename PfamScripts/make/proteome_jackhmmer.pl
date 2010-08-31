#! /software/bin/perl -w

#Script to run jackhmmer on all proteins in a particular
#proteome that do not have a match to Pfam-A

use strict;
use Getopt::Long;
use Log::Log4perl qw(:easy);

use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;


#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


my ($ncbi_tax, $help);


&GetOptions( "ncbi_tax=s" => \$ncbi_tax,
	           "help" => \$help);


unless($ncbi_tax) {
    print STDERR "Need to specify tax id ($0 -ncbi_tax <ncbi_taxid>)\n";
    &help();

}

open(SUMMARY, ">$ncbi_tax.txt") or die "Couldn't open fh to $ncbi_tax.txt, $!";


my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );

my $dbh = $pfamDB->getSchema->storage->dbh;

my $st_tax = $dbh->prepare("select species from taxonomy where ncbi_taxid = '$ncbi_tax'");

$st_tax->execute or $logger->logdie("Couldn't select species from taxonomy where ncbi_taxid = '$ncbi_tax' "..$st_tax->errstr."\n");

my $species = $st_tax->fetchrow;

unless($species) {
    $logger->logdie("Couldn't find the ncbi taxid |$ncbi_tax| in the databasexs"); 
}

$logger->info("Species name for NCBI taxid $ncbi_tax is '$species'");

print SUMMARY "Species name for NCBI taxid $ncbi_tax is '$species'\n";


#Get auto_proteome number
my $st1 = $dbh->prepare("select auto_proteome from complete_proteomes where ncbi_taxid = '$ncbi_tax'");
$st1->execute() or $logger->logdie("Couldn't select auto_proteome from complete_proteomes where ncbi_taxid = ncbi_tax " . $st1->errstr."\n");

my $auto_proteome =  $st1->fetchrow;

unless($auto_proteome) {
    $logger->logdie("Couldn't find the auto_proteome for the ncbi taxid $ncbi_tax in the database");
}


#Find out which ones have a Pfam-A match
my $st2 = $dbh->prepare("select auto_pfamseq from proteome_regions where auto_proteome = $auto_proteome");
$st2->execute() or $logger->logdie("Couldn't select auto_pfamseq from proteome_regions where auto_proteome = $auto_proteome ". $st2->errstr."\n");

my $pfamA_data = $st2->fetchall_arrayref;
my %pfamA_proteins;
foreach my $row(@$pfamA_data) { 
    $pfamA_proteins{$row->[0]}=1;
}
my $pfamA_count = keys %pfamA_proteins;




#Get total number of proteins in proteome
my $st3 = $dbh->prepare("select proteome_pfamseq.auto_pfamseq, pfamseq_acc, seq_version, description, sequence from pfamseq, proteome_pfamseq where pfamseq.auto_pfamseq = proteome_pfamseq.auto_pfamseq and auto_proteome = $auto_proteome");
$st3->execute() or die $logger->logdie("Couldn't select proteome_pfamseq.auto_pfamseq, pfamseq_acc, seq_version, description, sequence from pfamseq, proteome_pfamseq where pfamseq.auto_pfamseq = proteome_pfamseq.auto_pfamseq and auto_proteome = $auto_proteome " . $st3->errstr."\n");

my $all_data = $st3->fetchall_arrayref;
my %all_proteins;
my %no_pfamA;
my %fasta;
foreach my $row(@$all_data) { 
    my ($auto_pfamseq, $pfamseq_acc, $seq_version, $desc, $sequence) = ($row->[0], $row->[1], $row->[2], $row->[3], $row->[4]);

    $all_proteins{$auto_pfamseq}=1;

    unless(exists($pfamA_proteins{$auto_pfamseq})) {
	$no_pfamA{$auto_pfamseq}= $pfamseq_acc;
	$fasta{$auto_pfamseq}= ">$pfamseq_acc" . "." . "$seq_version $desc\n$sequence";
    }
}


my $count = keys %all_proteins;

$logger->info("$count proteins in proteome");
print SUMMARY "$count proteins in proteome\n";
my $percentage1 = ($pfamA_count/$count)*100;
$percentage1 = sprintf("%.2f", $percentage1);

my $no_pfamA = $count - $pfamA_count;
my $percentage2 = ($no_pfamA/$count)*100;
$percentage2 = sprintf("%.2f", $percentage2);

$logger->info("$pfamA_count ($percentage1\%) proteins match a Pfam-A domain");
print SUMMARY "$pfamA_count ($percentage1\%) proteins match a Pfam-A domain\n";
$logger->info("$no_pfamA ($percentage2\%) proteins do not match a Pfam-A domain");
print SUMMARY "$no_pfamA ($percentage2\%) proteins do not match a Pfam-A domain\n";

$logger->info("Going to run jackhmmer (N=3) on each of the proteins that do not match a Pfam-A");
print SUMMARY "Going to run jackhmmer (N=3) on each of the proteins that do not match a Pfam-A\n";
close SUMMARY;


unless(-d "Jackhmmer") {
    mkdir("Jackhmmer", 0755) or $logger->logdie("Couldn't mkdir Jackhmmer, $!");
}
chdir("Jackhmmer") or $logger->logdie("Couldn't chdir into 'Jackhmmer', $!");


foreach my $auto (keys %no_pfamA) {
    my $acc = $no_pfamA{$auto};

    mkdir($acc, 0755) or $logger->logdie("Couldn't mkdir $acc, $!");
    chdir($acc) or $logger->logdie("Couldn't chdir into $acc, $!");

    open(FH, ">seq.fasta") or $logger->die("Couldn't open fh to seq.fasta, $!");
    print FH $fasta{$auto};
    close FH;

    system("pfjbuild -N 3 -fa seq.fasta -gzip") and $logger->logdie("Failed to run jackhmmer on $acc, $!");
    chdir("../") or $logger->logdie("Couldn't chdir up a dir from $acc, $!");
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


After this script has been run, you should run proteome_summary.pl to
get a summary of how many new sequences will be added to Pfam, and how
many overlaps there are to Pfam-A families.

EOF

exit (0);

}
