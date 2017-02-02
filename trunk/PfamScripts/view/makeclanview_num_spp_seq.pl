#!/usr/bin/env perl

#calculates number of sequences and number of species in each clan
#and uploads to db
#run this after all clan and family view processes have run

use strict;
use warnings;
use Bio::Pfam::PfamLiveDBManager;
use Bio::Pfam::Config;
use DDP;
use Getopt::Long;
use File::Copy;
use Data::Dumper;
use Log::Log4perl qw(:easy);

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();


#Get clan membership
$logger->info("Fetching clan membership");
my @members = $pfamDB->getSchema->resultset('ClanMembership')->search({},
{ columns => [qw/ clan_acc pfama_acc /]});

my %membership;
my %fam_in_clan;
foreach my $row (@members){
  my $clan = $row->clan_acc->clan_acc;
  my $pfam = $row->pfama_acc->pfama_acc;
  $membership{$clan}{$pfam}=1;
  $fam_in_clan{$pfam}=$clan;
}

#Calculate number of sequences in each clan and upload to database
$logger->info("Updating number of sequences in each clan");
foreach my $clanacc (keys %membership){
  my $totalseqs = 0;
  foreach my $fam (keys %{$membership{$clanacc}}){
    my @family = $pfamDB->getSchema->resultset('PfamA')->search(
      { pfama_acc => $fam },
      {
        columns => [qw/ num_full /]
      }
    );
    $totalseqs += $family[0]->num_full;
  } #end of loop through families within the clan
  #insert total number of seqs into db
  $pfamDB->getSchema->resultset('Clan')->update_or_create({
      clan_acc => $clanacc,
      number_sequences => $totalseqs }
  );

} 

#Get the ncbi taxids for all seq in pfamseq
$logger->info("Getting ncbi taxids from pfamseq");
my $st_ncbi = $dbh->prepare("select pfamseq_acc, ncbi_taxid from pfamseq");
$st_ncbi->execute() or $logger->logdie("Couldn't execute statement ".$st_ncbi->errstr."\n");
my ($acc,$ncbi_tax);
$st_ncbi->bind_columns(\$acc, \$ncbi_tax);
my %ncbi;
while ($st_ncbi->fetch()) {
  $ncbi{$acc}=$ncbi_tax;
}

#Go through each clan and calculate how many species for each one
$logger->info("Calculating number of species for each clan");

#Get all regions from pfamA_reg_full_significant into a file
my $file = "reg_full.txt";
my $command = "mysql -h ".$pfamDB->{host}." -u ".$pfamDB->{user}." -p". $pfamDB->{password}." -P ".$pfamDB->{port}." ".$pfamDB->{database};
$command .= " --skip-column-names --quick -e \"select pfamA_acc, pfamseq_acc from pfamA_reg_full_significant where in_full=1\"";
system("$command > $file") and $logger->logdie("Problem running '$command', $!");

#Go through the regions and store the taxid for each one
my %clan_species;
my %seen;
open(FH, $file) or $logger->logdie("Couldn't open fh to $file, $!");
while(<FH>) {
  if(/^(\S+)\s+(\S+)/) {
    my ($pfamA_acc, $pfamseq_acc) = ($1, $2);

    if(exists($fam_in_clan{$pfamA_acc})) {
      my $clan=$fam_in_clan{$pfamA_acc};
      my $ncbi_taxid=$ncbi{$pfamseq_acc};
      $clan_species{$clan}{$ncbi_taxid}=1;
    }
  }
  else {
    $logger->warn("Unrecognised line in $file: $_");
  }
}
close FH;

#Upload the number of species for each clan to the db
$logger->info("Updating number_species for each clan in the clan table");
foreach my $clan (sort keys %clan_species) {
  my $num_species = keys %{$clan_species{$clan}};

  $pfamDB->getSchema->resultset('Clan')->update_or_create({
      clan_acc => $clan,
      number_species => $num_species }
  );
}

unlink($file);
