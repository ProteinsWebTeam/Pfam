#!/software/bin/perl

#This script is designed to calcualted the Pfam coverage for each proteome in the list
#of proteomes downloaded at sequence update time from Integr8 database.  This used to be done
#in Perl, but now down by a mixture of SQL and Perl statements. This pouplates three tables
#in the MySQL database - genome_species, genome_seqs and genome_pfamseq. Names are some what
#screwed up, but hey, that is for another day.

use strict;
use warnings;
use Getopt::Long;

use Log::Log4perl qw(:easy);
#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my($relNum, $help);

GetOptions(
  "rel=s"    => \$relNum,
  "h"        => \$help
);


if(!$relNum){
  $logger->logdie("No database number specified\n"); 
}


#-------------------------------------------------------------------------------
#Initial set-up

#Can we connect to the databse?
# get the config;
my $config = Bio::Pfam::Config->new;

#get the dbh handle for the database specifed in the config file;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{  $config->pfamliveAdmin }
);
my $dbh = $pfamDB->getSchema->storage->dbh;
#Still have not decieded how to do this! Return to later and fix

my $proteomeSpeciesSth = $dbh->prepare("INSERT INTO complete_prote (ncbi_code, species) 
										VALUES (?, ?, ?)") or die $dbh->errstr;

#Can we see the file containing the proteome data list. This will have been downloaded at sequence
#update time to ensure maximum union of the sequence ids btween Integr8 and pfamseq.
my $dir = $config->localDbsLoc."/proteome/Release" . $relNum;
open(_PROT, "$dir/proteome_report.txt") || die "Could not open $dir/proteome_report.txt:[$!]\n";

#-------------------------------------------------------------------------------
#Start off with empty tables

my $st = $dbh->prepare("delete from complete_proteomes") or $logger->logdie( "Failed to delete from proteomes, could not prepare:".$dbh->errstr );
$st->execute() or $logger->logdie( "Failed to delete from proteomes, could not execute:".$dbh->errstr );
$st->finish() or $logger->logdie( "Failed to delete from proteomes, could not finish:".$dbh->errstr );

#-------------------------------------------------------------------------------
#Get auto_pfamseq mapping
$logger->info("Getting pfamseq accessions/auto_pfamseq");

my $newSeqSth =
  $dbh->prepare("select pfamseq_acc, auto_pfamseq from pfamseq")
  or $logger->logdie('select prepare failed:'. $dbh->errstr );
$newSeqSth->execute or $logger->logdie('select execution failed:'. $dbh->errstr );
my $res = $newSeqSth->fetchall_arrayref;
my $pfamseqAutos;
foreach my $r (@$res) {
  $pfamseqAutos->{ $r->[0] } = $r->[1];
}

#-------------------------------------------------------------------------------



#Get the current list of complete proteomes
my (%ncbi_codes);
while(<_PROT>) {
  if (/(\d+)\s+(\d+)\s+(.*)\s+(\S+)\s+(\d+)\s+$/) {
      my $fp = $1;
      my $text = $3; 
      my $code = $2; 
      my $filename = $4;
	  #print "$code, $text, $filename\n";
      $ncbi_codes{$filename}{"text"} = $text;
      $ncbi_codes{$filename}{"code"} = $code;
      $ncbi_codes{$filename}{"pref"} = $fp;
  }else{
  	warn "Unrecognised line:$_\n";
  }
}
close(_PROT);


#For each proteome, get the list of sequences, then start working on combining with Pfam   
foreach my $file (keys %ncbi_codes){
	print "Working on $file\n";
	#Try and open up the file
	my ($genomic_sequence_length, %store_acc);
	if(-s "$dir/".$ncbi_codes{$file}{"pref"}.".$file.fasta.gz"){
		open(FILE, "gunzip -c $dir/".$ncbi_codes{$file}{"pref"}.".$file.fasta.gz|") || die "Counld not gunzip file\n";
		my ($acc, $id);
    	while(<FILE>){
	    	if ($_ =~ /\>\S{2}\|(\S{6})[\-\d]{0,2}\|(\S+)/){
    		  $acc = $1;
    		  $store_acc{$acc}++;
    		}
    	}
		close(FILE);
		process_proteome($ncbi_codes{$file}{"code"}, $ncbi_codes{$file}{"text"},  \%store_acc, $pfamseqAutos);
	}else{
		warn "$dir/".$ncbi_codes{$file}{"pref"}.".$file.fasta.gz has no size\n";
	}
}

$dbh->do('update complete_proteomes c set total_aa_length = (select sum(length) from pfamseq s, proteome_pfamseq p where s.auto_pfamseq=p.auto_pfamseq and p.auto_proteome=c.auto_proteome)'); 
$dbh->do('update pfamseq s, proteome_pfamseq p set genome_seq = 1 where  s.auto_pfamseq=p.auto_pfamseq');

sub process_proteome {
  my($ncbi_code, $ncbi_species, $storeAccRef, $pfamseqAutos) = @_;
  
  my $r =  $pfamDB->getSchema->resultset('CompleteProteomes')->find_or_create({ ncbi_taxid => $ncbi_code,
                                                                                species    => $ncbi_species });
  
  my $autoProteome = $r->auto_proteome;
  my $count = 0;
  my @insertThis;
  foreach my $gs (keys %{$storeAccRef}) {
  	if($pfamseqAutos->{$gs}){
  	  $count++;
  	  push(@insertThis, '('.$autoProteome.','.$pfamseqAutos->{$gs}.')' );
  	}else{
  	   $logger->debug($gs." is not in pfamseq"); 
  	}
  }
  
  $dbh->do("INSERT INTO proteome_pfamseq (auto_proteome, auto_pfamseq) VALUES ".join(",", @insertThis));
  $r->update({total_genome_proteins => $count});
}



