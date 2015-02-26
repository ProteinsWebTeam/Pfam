#!/usr/bin/env perl

#calculates number of sequences and number of species in each clan
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
  
$logger->debug("Calculating number of sequences present in each clan");
$logger->debug("Fetching clan membership");

my @members = $pfamDB->getSchema->resultset('ClanMembership')->search(
    {},
    {
	columns => [qw/ clan_acc pfama_acc /]
    }
    );
my %membership;
foreach my $row (@members){
    my $clan = $row->clan_acc->clan_acc;
    my $pfam = $row->pfama_acc->pfama_acc;
    $membership{$clan}{$pfam}=1;
}

$logger->debug("Calculating number of sequences in each clan");
foreach my $clanacc (keys %membership){
    my $totalseqs = 0;
#for each member get number of seqs and add to total
    foreach my $fam (keys %{$membership{$clanacc}}){
	my @family = $pfamDB->getSchema->resultset('PfamA')->search(
	    { pfama_acc => $fam },
	    {
		columns => [qw/ num_full /]
	    }
	    );
	$totalseqs = $totalseqs + $family[0]->num_full;
    } #end of loop through families within the clan
 #insert total number of seqs into db
    $pfamDB->getSchema->resultset('Clan')->update_or_create({
	clan_acc => $clanacc,
	number_sequences => $totalseqs }
	);

} 

$logger->debug("Calculating number of species present in each clan");
$logger->debug("Fetching clan/tax id data");

#note - the following query may take a long time to run. If it takes too long an alternative strategy is to use the following query as a system command, and then parse to file into the %clan_spp hash
#mysql -h <host> -u <user> -p<passowrd> -P <port> <database> --quick -e "SELECT p.ncbi_taxid, c.clan_acc FROM pfamseq p, pfamA_reg_full_significant r, clan_membership c WHERE r.pfamseq_acc = p.pfamseq_acc AND c.pfama_acc = r.pfama_acc" > clan_spp

my $st = $dbh->prepare("SELECT p.ncbi_taxid, c.clan_acc FROM pfamseq p, pfamA_reg_full_significant r, clan_membership c WHERE r.pfamseq_acc = p.pfamseq_acc AND c.pfama_acc = r.pfama_acc") or die "Cannot prepare statement\n";
$st->execute();
my $arrayref = $st->fetchall_arrayref;
my %clan_spp;
foreach my $row (@$arrayref){
    $clan_spp{$row->[1]}{$row->[0]}=1;
}

$logger->debug("Calculating the number of species for each clan");
foreach my $clan (keys %clan_spp){
    my $sppcount = keys %{$clan_spp{$clan}};
    print "$clan $sppcount\n";
#update column
    $pfamDB->getSchema->resultset('Clan')->update_or_create({
	clan_acc => $clan,
	number_species => $sppcount }
	);
}
