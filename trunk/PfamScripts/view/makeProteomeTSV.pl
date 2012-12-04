#!/usr/bin/env perl

#Script to print files containing the domains for each proteome in the completed_proteomes tables
#Output: one file foreach proteome in cwd
#Format of file is <seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc> <hmm name> <type> <hmm start> <hmm end> <hmm length> <bit score> <E-value> <clan>

use strict;
use warnings;
use Log::Log4perl qw(:easy);
use IO::Compress::Gzip qw(gzip $GzipError);
use DBI;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

#my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
#Set up database
my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new(  database => "pfam_26_0", password => "mafp1", user => "pfam", port => "3302", host => "pfamdb2a"  );
unless ($pfamDB) {
  $logger->die("Failed to connect to database");
}


my $dbh = $pfamDB->getSchema->storage->dbh;

#Get pfamA_acc, pfamA_id, family type and hmm_length to auto_pfamA mapping from pfamA table
$logger->info("Retrieving pfamA information from database");
my $pfamA = pfamA($pfamDB);

#Get clan to auto_pfamA mapping
$logger->info("Retrieving clan information from database");
my $clan = clan($pfamDB, $pfamA);

#Set up query for retrieving domain info from pfamA_reg_full_significant
my $st_regions = $dbh->prepare("select pfamseq_acc, ali_start, ali_end, seq_start, seq_end, auto_pfamA, model_start, model_end, domain_bits_score, domain_evalue_score from pfamseq left join pfamA_reg_full_significant reg on pfamseq.auto_pfamseq=reg.auto_pfamseq left join proteome_pfamseq on proteome_pfamseq.auto_pfamseq=pfamseq.auto_pfamseq where auto_proteome = ? and in_full=1") or die "Failed to prepare statement:".$dbh->errstr."\n";

#Get ncbi_taxids from completed_proteomes table
$logger->info("Retrieving list of completed proteomes from database");
my $st_proteomes = $dbh->prepare("select cp.auto_proteome, cp.ncbi_taxid, cp.species, cp.total_genome_proteins from complete_proteomes cp join ncbi_taxonomy on ncbi_taxonomy.ncbi_taxid = cp.ncbi_taxid") or die "Failed to prepare statement:".$dbh->errstr."\n";
$st_proteomes->execute() or die "Couldn't execute statement ".$st_proteomes->errstr."\n";
my $complete_proteomes = $st_proteomes->fetchall_arrayref();


#Go through each taxid and get Pfam-A regions
foreach my $proteome (@$complete_proteomes) {

  my ($auto_proteome, $ncbi_taxid, $species, $total_genome_proteins) = ($proteome->[0], $proteome->[1], $proteome->[2], $proteome->[3]);

  #Set up filehandle for printing
  my $outfile = $ncbi_taxid . ".tsv";
  if(-s "$outfile.gz") {
    $logger->info("Already done ncbi taxid $ncbi_taxid '$species'");
    next;
  }
  open(OUT, ">$outfile") or die "Couldn't open fh to $outfile, $!";
  
  #Retrieve domain info for proteome
  $logger->info("Retrieving domains for ncbi taxid $ncbi_taxid '$species'");
  $st_regions->execute($auto_proteome) or die "Couldn't execute statement ".$st_regions->errstr."\n";;
  my $pfamA_domains = $st_regions->fetchall_arrayref();

  #Print header
  print OUT "#Pfam-A regions from Pfam version 26.0 for ncbi taxid $ncbi_taxid '$species'\n";
  print OUT "#Total number of proteins in proteome: $total_genome_proteins\n";
  print OUT "#<seq id> <alignment start> <alignment end> <envelope start> <envelope end> <hmm acc> <hmm name> <type> <hmm start> <hmm end> <hmm length> <bit score> <E-value> <clan>\n";

  #Go through each domain and print info
  foreach my $domain (@$pfamA_domains) {
    my ($pfamseq_acc, $ali_start, $ali_end, $seq_start, $seq_end, $auto_pfamA, $model_start, $model_end, $domain_bits_score, $domain_evalue_score) = ($domain->[0], $domain->[1], $domain->[2], $domain->[3], $domain->[4], $domain->[5], $domain->[6], $domain->[7], $domain->[8], $domain->[9]);
    
    print OUT "$pfamseq_acc\t$ali_start\t$ali_end\t$seq_start\t$seq_end\t$pfamA->{$auto_pfamA}->{pfamA_acc}\t$pfamA->{$auto_pfamA}->{pfamA_id}\t$pfamA->{$auto_pfamA}->{type}\t$model_start\t$model_end\t$pfamA->{$auto_pfamA}->{model_length}\t$domain_bits_score\t$domain_evalue_score\t$clan->{$auto_pfamA}\n";

  } 
  close OUT;

  #gzip the file
  my $gzip = $outfile . ".gz";
  gzip $outfile => $gzip or die "Problem with gzip: $GzipError\n";
  unlink($outfile);

}



#Subroutine to store pfamA_acc, pfamA_id, model_length and type for all families
sub pfamA {
  my ($pfamDB) = @_;

  my @pfamA = $pfamDB->getSchema->resultset('Pfama')->search({},); 

  my $pfamA;
  foreach my $row (@pfamA) {
    $pfamA->{$row->auto_pfama}->{pfamA_acc}= $row->pfama_acc;
    $pfamA->{$row->auto_pfama}->{pfamA_id}= $row->pfama_id;
    $pfamA->{$row->auto_pfama}->{model_length}= $row->model_length;
    $pfamA->{$row->auto_pfama}->{type}= $row->type;
  }
  return($pfamA);
}

#Subroutine to store clan_acc for all families
sub clan {
  my ($pfamDB, $pfamA) = @_;

  my @clan = $pfamDB->getSchema->resultset("ClanMembership")->search({},
								     {  join     => [qw/auto_clan/],
									prefetch => [qw/auto_clan/]
								     });
  #Add clans to hash
  my $clan;
  foreach my $row (@clan) {
    $clan->{$row->auto_pfama->auto_pfama}=$row->auto_clan->clan_acc;
  }

  #Add No_clan to hash for pfamA with no clan
  foreach my $auto_pfamA (keys %$pfamA) {
    unless(exists($clan->{$auto_pfamA})) {
      $clan->{$auto_pfamA}="No_clan";
    }
  }
  return($clan);
}
