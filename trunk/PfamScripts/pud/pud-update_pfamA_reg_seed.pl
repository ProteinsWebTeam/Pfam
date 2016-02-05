#!/usr/bin/env perl

use strict;
use warnings;
use DBI;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;
use Log::Log4perl qw(:easy);

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();


#Db connection
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $dbh = $pfamDB->getSchema->storage->dbh;


#Make a file of md5s from pfamA_reg_seed
$logger->info("Making a file of accessions and md5s from pfamA_reg_seed, this will be used during seed surgery\n");
my $md5_sth = $dbh->prepare("select pfamseq_acc, md5 from pfamA_reg_seed");
$md5_sth->execute() or $logger->logdie("Couldn't select pfamseq_acc and md5 from pfamA_reg_seed table ".$md5_sth->errstr);

my ($accession, $md5);
$md5_sth->bind_columns(\$accession, \$md5);

my $outfile="pfamA_reg_seed.md5";
open(MD5, ">$outfile") or $logger->logdie("Couldn't open fh to $outfile, $!");
while ($md5_sth->fetch()) {
  print MD5 "$accession\t$md5\n";
}
close MD5;


#See which sequences from pfamA_reg_seed are not in the pfamseq table
$logger->info("Seeing which sequences from pfamA_reg_seed have been deleted from the pfamseq table");
my $pfamseq_sth=$dbh->prepare("select s.pfamseq_acc from pfamA_reg_seed s left join pfamseq p on p.pfamseq_acc=s.pfamseq_acc and p.md5=s.md5 and p.seq_version=s.seq_version where source='pfamseq' and p.pfamseq_acc is null");
$pfamseq_sth->execute() or $logger->logdie("Couldn't execute statement ".$pfamseq_sth->errstr);
my ($pfamseq_acc);
$pfamseq_sth->bind_columns(\$pfamseq_acc);

my %delete;
while ($pfamseq_sth->fetch()) {
  $delete{$pfamseq_acc}=1;
}


#See which sequences from pfamA_reg_seed are not in the uniprot table
$logger->info("Seeing which sequences from pfamA_reg_seed have been deleted from the uniprot table");
my $uniprot_sth=$dbh->prepare("select s.pfamseq_acc from pfamA_reg_seed s left join uniprot u on u.uniprot_acc=s.pfamseq_acc and u.md5=s.md5 and u.seq_version=s.seq_version where source='uniprot' and u.uniprot_acc is null");
$uniprot_sth->execute() or $logger->logdie("Couldn't execute statement ".$uniprot_sth->errstr);
my ($uniprot_acc);
$uniprot_sth->bind_columns(\$uniprot_acc);

while ($uniprot_sth->fetch()) {
  $delete{$uniprot_acc}=1; 
}

my $num_seq = keys %delete;
$num_seq=0 unless($num_seq);

#Delete sequences from pfamA_reg_seed that are not in the pfamseq or uniprot tables
$logger->info("Deleting the $num_seq accessions from pfamA_reg_seed that have been deleted from the pfamseq or uniprot tables");
my $delete_sth=$dbh->prepare("delete from pfamA_reg_seed where pfamseq_acc=?");
foreach my $acc (keys %delete) {
  $delete_sth->execute($acc) or $logger->logdie("Couldn't execute statement ".$delete_sth->errstr);
}
