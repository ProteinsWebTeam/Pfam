#!/usr/bin/env perl

#Obtain current (live) reference proteome set from UniPort
#update pfamseq table to flag which entries are in this set (_live_ref_proteome)
#make a fasta file and easel indices of sequences from pfamseq which are in this set
#copy the live reference proteome set to /nfs/production - this will over-write previous versions of this live_reference_proteome set

use strict;
use warnings;
use DDP;
use LWP::UserAgent;
use Log::Log4perl qw(:easy);
use File::Copy;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my $ua = LWP::UserAgent->new;
$ua->env_proxy;

$logger->debug("Obtaining data from UniProt");

my $res = $ua->get( 'http://www.uniprot.org/uniprot/?query=keyword:KW-1185&format=tab&columns=id,version%28sequence%29' );

if ( $res->is_success ) {
  print "Data successfully obtained from UniProtKB\n";
}
else {
  print STDERR $res->status_line, "\n";
  $logger->logdie("Data not obtained from UniProt");
}

my $data = $res->content;
my @list = split(/\n/, $data);

#set all current_ref_prot to 0 (0 should be default, not null)
$logger->debug("Setting all current_ref_prot to 0");
my $stup = $dbh->prepare("update pfamseq set _live_ref_proteome = 0") or $logger->logdie("Can't prepare statement: $dbh->errstr");
$stup->execute() or die "Couldn't update _live_ref_proteome to 0\n";

#loop through @list and for each accession in the list that is present in pfamseq change current_ref_prot to 1

$logger->debug("Updating current_ref_prot column");
my $stacc = $dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_acc = ? and seq_version = ?") or $logger->logdie("Can't prepare statement: $dbh->errstr");
my $strp = $dbh->prepare("update pfamseq set _live_ref_proteome = 1 where pfamseq_acc = ?") or $logger->logdie("Can't prepare statement: $dbh->errstr");
my $count=0;
foreach my $entry (@list){
    #skip over anything that is not a uniprot accn
    if ($entry =~ /^[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}/){
        my @data = split(/\s+/, $entry);
        my $accn = $data[0];
        my $version = $data[1];
    #if this accn is in pfamseq, check version number update current_ref_prot to 1
        $stacc->execute($accn, $version);
        my $rows = $stacc->rows;
        if ($rows > 0){
               $strp->execute($accn);
            $count++;
        }    
    }
}

$logger->debug("Updated $count records");

#take a database dump of a new refprot set
$logger->debug("Creating fasta file");
my $stfetch = $dbh->prepare("select pfamseq_acc, seq_version, pfamseq_id, description, sequence from pfamseq where _live_ref_proteome='1'") or $logger->logdie("Can't prepare statement: $dbh->errstr");
$stfetch->execute() or $logger->logdie("Can't execute statement: $dbh->errstr");
my $rowfetch = $stfetch->rows;
my $outfile = "live_reference_proteome.fa";
open (FA, ">$outfile") or $logger->logdie("Can't open file to write");
$logger->debug("Fetching records...");
my $array_ref = $stfetch->fetchall_arrayref();
foreach my $row (@$array_ref) {
	print FA ">" . $row->[0] . "." . $row->[1] . " " . $row->[2] . " " . $row->[3] . "\n" . $row->[4] . "\n";
}
close FA;

$logger->debug("Finished fetching sequences: $rowfetch sequences fetched");

#make SSI index
$logger->debug("Making easel indices for live reference proteome set");
system ("esl-sfetch --index $outfile") and $logger->logdie("Couldn't make easel indices for pfamseq:[$!]");

#move to nfs/production - this will over-write previous versions
my $nfsdir = "/nfs/production/xfam/pfam/data/live_reference_proteome";
$logger->debug("Moving files to $nfsdir");
my $ssifile = $outfile . ".ssi";
move($outfile, "$nfsdir/$outfile") or $logger->logdie("Could not copy $outfile to $nfsdir");
move($ssifile, "$nfsdir/$ssifile") or $logger->logdie("Could not copy $ssifile to $nfsdir");


$logger->debug("Finished - now please update the config file with the new database size ($rowfetch) for live reference proteome set");



