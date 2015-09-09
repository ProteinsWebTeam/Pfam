#!/usr/bin/env perl

#Script to perform uniprot search against a family
#The script gets the HMM from the db and adds the GA to it
#It searches the HMM agaist the uniprot db
#It uploads the alignment to the alignment_and_tree table
#It uploads the regions to the uniprot_reg_full table
#If the family is not in a clan, it populates number_uniprot in the pfamA table

use strict;
use warnings;
use File::Path qw(remove_tree);
use Compress::Zlib;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;


my $pfamA_acc=shift;
die "No pfamA_acc specified" unless($pfamA_acc);

my $config = Bio::Pfam::Config->new;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh = $pfamDB->getSchema->storage->dbh;

if(-d $pfamA_acc) {
  die "$pfamA_acc directory already exists\n";
} 
else {
  mkdir($pfamA_acc, 0775)
}
my $db = $config->uniprotLoc . "/uniprot"; 

chdir($pfamA_acc) or die "Couldn't chdir into $pfamA_acc, $!";

#Get a copy of the HMM
my $hmmResult = $pfamDB->getSchema->resultset('PfamAHmm')->find({ pfama_acc => $pfamA_acc});
open(HMM, ">$pfamA_acc.hmm.noGA") or die "Couldn't open fh to $pfamA_acc.hmm.noGA, $!";
print HMM $hmmResult->hmm;
close HMM;

#Look up GA thresholds
my $sth=$dbh->prepare("select sequence_GA, domain_GA from pfamA where pfamA_acc='$pfamA_acc'");
$sth->execute() or die "Couldn't execute statement ".$sth->errstr."\n";
my ($ga_seq, $ga_dom)=$sth->fetchrow;

#Add GA to HMM
open(NEW, ">$pfamA_acc.hmm") or die "Couldn't open fh to $pfamA_acc.hmm, $!";
open(HMM, "$pfamA_acc.hmm.noGA") or die "Couldn't open fh to $pfamA_acc.hmm.noGA, $!";
while(<HMM>) {
  print NEW $_;
  if(/^CKSUM/) {
    print NEW "GA    $ga_seq $ga_dom\n";
  }
}
close HMM;
close NEW;

#Run hmmsearch
system("hmmsearch --cut_ga --cpu 4 --domtblout $pfamA_acc.doms -A $pfamA_acc.stockholm $pfamA_acc.hmm $db > /dev/null");

#Reformat the alignment (refomat to pfam style, then remove all blank lines and lines beginning '#'
system("esl-reformat --informat stockholm pfam $pfamA_acc.stockholm | grep -v \'^#\\|^\$\' > $pfamA_acc.pfam");

#Read in the alignment and upload to db
my $aln;
open(ALN, "$pfamA_acc.pfam") or die "Couldn't open fh to $pfamA_acc.pfam, $!"; 
while(<ALN>) {
  $aln.=$_;
}
close ALN;

$pfamDB->getSchema->resultset('AlignmentAndTree')->update_or_create({
    pfama_acc => $pfamA_acc,
    alignment  => Compress::Zlib::memGzip($aln),
    type       => 'uniprot'
  }
);

#Delete old regions, if any
$pfamDB->getSchema->resultset('UniprotRegFull')->search( { pfama_acc => $pfamA_acc} )->delete;

#Upload matches to db
my @upload;
my $num_full=0;
open(MATCHES, "$pfamA_acc.doms") or die "Couldn't open fh to $pfamA_acc.doms, $!";
while(<MATCHES>) {
  next if(/^#/);
  my @line = split(/\s+/, $_);

  #                                                                          full sequence --- -------------- this domain -------------   hmm coord   ali coord   env coord
  #   ## target name accession   tlen query name accession  qlen   E-value  score  bias   #  of  c-Evalue  i-Evalue  score  bias  from    to  from    to  from    to  acc description of target
  #   ##------------ ---------- ----- ---------- ---------- ----- --------- ------ ----- --- --- --------- --------- ------ ----- ----- ----- ----- ----- ----- ----- ---- ---------------------
  #   #A0A0D3H5P1.1  -            982 SEED       -            307  4.8e-137  467.6  33.3   1   3   7.4e-48   2.6e-44  163.0   6.1     1   239    77   389    77   397 0.77 A0A0D3H5P1_9ORYZ 
  #
  my ($uniprot_acc, $seq_evalue, $seq_bits, $dom_evalue, $dom_bits, $hmm_st, $hmm_en, $ali_st, $ali_en, $env_st, $env_en) = ($line[0], $line[6], $line[7], $line[12], $line[13], $line[15], $line[16], $line[17], $line[18], $line[19], $line[20]);

  #Remove sequence version from acc
  if($uniprot_acc =~ /^(\S+)\.\d+/) {
    $uniprot_acc=$1;
  }

  push(@upload,  
    { pfama_acc => $pfamA_acc,
      uniprot_acc => $uniprot_acc,
      seq_start => $env_st,
      seq_end => $env_en,
      ali_start => $ali_st,
      ali_end => $ali_en,
      model_start => $hmm_st,
      model_end => $hmm_en,
      domain_bits_score => $dom_bits,
      domain_evalue_score => $dom_evalue,
      sequence_bits_score => $seq_bits,
      sequence_evalue_score => $seq_evalue,
      in_full => 1 });

  $num_full++;
  if(scalar(@upload) > 1000) {
    $pfamDB->getSchema->resultset("UniprotRegFull")->populate(\@upload);
    @upload = ();
  }
}
close MATCHES;

if(scalar(@upload)) {
  $pfamDB->getSchema->resultset("UniprotRegFull")->populate(\@upload);
}

#Update number_uniprot in pfamA table if family is not in a clan
#If family is in clan, this step is done later after clan competition
my $st_clan_membership = $dbh->prepare("select clan_acc from clan_membership where pfamA_acc='$pfamA_acc'");
$st_clan_membership->execute() or die "Couldn't execute statement ".$st_clan_membership->errstr."\n";
my $clan=$st_clan_membership->fetchrow;
unless($clan) {
  my $st_update_pfamA = $dbh->prepare("update pfamA set number_uniprot='$num_full' where pfamA_acc='$pfamA_acc'");
  $st_update_pfamA->execute() or die "Failed to update number_uniprot in pfamA table ". $st_update_pfamA->errstr."\n";
}

chdir("../") or die "Couldn't chdir up from $pfamA_acc, $!";
remove_tree($pfamA_acc);
