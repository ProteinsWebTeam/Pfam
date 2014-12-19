#!/usr/bin/env perl

#This script is designed to calcualted the Pfam coverage for each proteome in the list
#of proteomes downloaded at sequence update time from Integr8 database.  This used to be done
#in Perl, but now down by a mixture of SQL and Perl statements. This pouplates three tables
#in the MySQL database - genome_species, genome_seqs and genome_pfamseq. Names are some what
#screwed up, but hey, that is for another day.

use strict;
use warnings;
use Getopt::Long;
use Data::Printer;

use Log::Log4perl qw(:easy);

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my ( $relNum, $help );

GetOptions(
  "rel=s" => \$relNum,
  "h"     => \$help
);

if ( !$relNum ) {
  $logger->logdie("No database number specified\n");
}

#-------------------------------------------------------------------------------
#Initial set-up

#Can we connect to the databse?
# get the config;
my $config = Bio::Pfam::Config->new;

#get the dbh handle for the database specifed in the config file;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh    = $pfamDB->getSchema->storage->dbh;

#Still have not decieded how to do this! Return to later and fix

my $proteomeSpeciesSth = $dbh->prepare(
  "INSERT INTO complete_proteome (ncbi_code, species) 
                    VALUES (?, ?, ?)"
) or die $dbh->errstr;

#Can we see the file containing the proteome data list. This will have been downloaded at sequence
#update time to ensure maximum union of the sequence ids between uniprot complet proteiome and pfamseq.
my $dir = $config->localDbsLoc . "/proteome/Release" . $relNum;
open( _PROT, "$dir/list" ) || die "Could not open $dir/taxonomy_list:[$!]\n";

#-------------------------------------------------------------------------------
#Start off with empty tables
$logger->debug("Deleteing from complete_proteomes");
my $st = $dbh->prepare("delete from complete_proteomes")
  or $logger->logdie(
  "Failed to delete from proteomes, could not prepare:" . $dbh->errstr );
$st->execute()
  or $logger->logdie(
  "Failed to delete from proteomes, could not execute:" . $dbh->errstr );
$st->finish()
  or $logger->logdie(
  "Failed to delete from proteomes, could not finish:" . $dbh->errstr );

#Reset all genome_seqs to be zero.
$logger->debug("Reseting pfamseq genome seqs");
$dbh->do('update pfamseq s set genome_seq = 0');

#-------------------------------------------------------------------------------
#Get auto_pfamseq mapping
$logger->info("Getting pfamseq accessions/auto_pfamseq");

my $seqSth =
     $dbh->prepare("select pfamseq_acc from pfamseq where pfamseq_acc = ?") #mySQL statement updated for db schema change
  or $logger->logdie( 'select prepare failed:' . $dbh->errstr );

#-------------------------------------------------------------------------------
#Right, we have had some issues with the kingdom not being set in the website
#properly.....trying to fix here.....

my @king =
  $pfamDB->getSchema->resultset('Taxonomy')
  ->search( { 'rank' => 'superkingdom' } );

#Get the current list of complete proteomes
my (%ncbi_codes);
while (<_PROT>) {
  my @line = split( /\t/, $_ );
  next if $line[0] eq 'Taxon';

#Conent should be smething lile
#Taxon  Mnemonic  |Scientific name |Common name |Synonym |Other Names |Reviewed  Rank  Lineage Parent

  #If this taxon in the database?
  my $row =
    $pfamDB->getSchema->resultset('Taxonomy')
    ->search( { ncbi_taxid => $line[0] } )->first;

  next if ( !$row );

  my $king = undef;
  foreach my $k (@king) {
    if ( $row->lft >= $k->lft and $row->rgt <= $k->rgt ) {
      $king = $k->level;
    }
  }

  $ncbi_codes{ $line[0] }{"text"} = $line[2];
  $ncbi_codes{ $line[0] }{"code"} = $line[0];
  $ncbi_codes{ $line[0] }{"king"} = $line[0];
}
close(_PROT);

#For each proteome, get the list of sequences, then start working on combining with Pfam
foreach my $file ( keys %ncbi_codes ) {
  print "Working on $file\n";

  #Try and open up the file
  my ( $genomic_sequence_length, %store_acc );
  if ( -s "$dir/$file.fasta" ) {
    open( FILE, "<", "$dir/$file.fasta" )
      || die "Counld not open file, $dir/$file.fasta:[$!]\n";
    my ( $acc, $id );
    while (<FILE>) {
      if ( $_ =~ /\>\S{2}\|(\S{6})[\-\d]{0,2}\|(\S+)/ ) {
        $acc = $1;
        $store_acc{$acc}++;
      }
    }
    close(FILE);
    process_proteome(
      $ncbi_codes{$file}{"code"},
      $ncbi_codes{$file}{"text"},
      $ncbi_codes{$file}{"king"},
      \%store_acc, $seqSth
    );
  }
  else {
    warn "$dir/" . $ncbi_codes{$file}{"pref"} . ".$file.fasta.gz has no size\n";
  }
}

$dbh->do(
'update complete_proteomes c set total_aa_length = (select sum(length) from pfamseq s, proteome_pfamseq p where s.pfamseq_acc=p.pfamseq_acc and p.ncbi_taxid=c.ncbi_taxid)'   
);  #mySQL statement updated for new db schema - assumed ncbi_taxid is new primary key for complete_proteomes
$dbh->do(
'update pfamseq s, proteome_pfamseq p set genome_seq = 1 where  s.pfamseq_acc=p.pfamseq_acc'
); #mySQL statement updated for new schema

sub process_proteome {
  my ( $ncbi_code, $ncbi_species, $group, $storeAccRef, $seqSth ) = @_;

  my $r = $pfamDB->getSchema->resultset('CompleteProteome')->find_or_create(
    {
      ncbi_taxid => $ncbi_code,
      species    => $ncbi_species,
      grouping   => $group
    }
  );

  my $autoProteome = $r->auto_proteome;
  my $count        = 0;
  my @insertThis;
  foreach my $gs ( keys %{$storeAccRef} ) {
    $seqSth->execute($gs);
    my $rowRef = $seqSth->fetchrow_arrayref;
    if ( defined($rowRef) ) {
      $count++;
	push(@insertThis, $autoProteome);
	push (@insertThis, $rowRef->[0]);
    }
    else {
      $logger->debug( $gs . " is not in pfamseq" );
    }
  }
   if ($insertThis[0]){
 	 $dbh->do( "INSERT INTO proteome_pfamseq (auto_proteome, pfamseq_acc) VALUES ( $insertThis[0], \"$insertThis[1]\" )"); 
	 $r->update( { total_genome_proteins => $count } );
	 }

}

