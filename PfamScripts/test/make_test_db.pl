#!/usr/bin/env perl

use strict;
use warnings;
use Getopt::Long;
use Log::Log4perl qw(:easy);
use DateTime;
use Data::Printer;

use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

my ( @families, $dir, $number );

&GetOptions(
  "dir=s"  => \$dir,
  "fam=s"  => \@families,
  "num_families=i" => \$number,
  ) or $logger->logdie("Invalid option!");
 
$number = 10 unless($number);
$logger->info("Connecting to the databases.");
#Get the connection to the pfam database.
my $config = Bio::Pfam::Config->new;
my $connectParams = $config->pfamliveAdmin;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $connectParams } );
my $dbh    = $pfamDB->getSchema->storage->dbh;

my %newConnectParams = %{ $config->pfamliveAdmin };
my $newConnectParams = \%newConnectParams;
my $dt = DateTime->now;
$newConnectParams->{database} = 'pfam_test_'.$dt->ymd("_");

$logger->info("Generating Schema.");

#dump an empty database.
system( "mysqldump -u ".$connectParams->{adminuser}. 
                   " -p".$connectParams->{adminpassword}.
                   " -h ".$connectParams->{host}. 
                   " -P".$connectParams->{port}.
                   " -d ".$connectParams->{database}. " > /tmp/schema.dmp");


$dbh->do("drop database if exists $newConnectParams->{database}") or $dbh->errstr;
$dbh->do("create database $newConnectParams->{database}") or $dbh->errstr;
my $pfamDBNew = Bio::Pfam::PfamLiveDBManager->new( %{ $newConnectParams } );
my $dbhNew    = $pfamDBNew->getSchema->storage->dbh;

system( "mysql -u ".$newConnectParams->{adminuser}. 
                  " -p".$newConnectParams->{adminpassword}.
                  " -h ".$newConnectParams->{host}. 
                  " -P".$newConnectParams->{port}.
                  " ". $newConnectParams->{database} ." < /tmp/schema.dmp");

$logger->info("Building temporary table.");

$dbhNew->do("create temporary table _families ( pfamA_acc varchar(7) NOT NULL, ".
            "UNIQUE KEY _fam_pfamA_acc (pfamA_acc)) ENGINE=InnoDB");

my $sth = $dbhNew->prepare("INSERT INTO _families (pfamA_acc) values (?)");

#Now randomly add families in, to make a up the number to the desired number of
#families 
if($number and scalar(@families) < $number){
  my $extra = $number - scalar(@families);
  my @allFamilies = $pfamDB->getSchema->resultset('Pfama')->search({});
  my $max = scalar(@allFamilies);
  #Get a hash, keyed of accessions
  my %addedFamilies = map{ $_ => 1 } @families;
  while(scalar(@families < $number)){
    my $family = $allFamilies[ rand($max) ];
    next if($family->num_full > 10000);
    if(! exists($addedFamilies{$family->pfama_acc})){
      push(@families, $family->pfama_acc);  
    }
  }
}

$logger->info("Building pfam based on:".join(", ", @families));
for my $f (@families){
  $sth->execute($f);  
}
$sth->finish;

#We may want to add the nested domain data here, so that we can have complete pfamA info.

#Join against the temporary table
$dbhNew->do("insert into pfamA select t.* from ".$connectParams->{database}.
               ".pfamA t, _families f where t.pfamA_acc=f.pfamA_acc");

$logger->info("Inserting Pfam-A related data.");

#now go through all of the live pfamA related tables and do an insert
$dbhNew->do("SET FOREIGN_KEY_CHECKS = 0;");
foreach my $table (qw(pfamA_literature_references pfamA_wiki _pfamA_internal pfamA_reg_seed pfamA_reg_full_significant pfamA_reg_full_insignificant clan_membership edits interpro gene_ontology nested_domains nested_locations current_pfam_version released_pfam_version pdb_pfamA_reg pfamA_HMM)){
  $dbhNew->do("insert into $table select t.* from ".$connectParams->{database}.".$table t, pfamA f where t.auto_pfamA=f.auto_pfamA");
}

$dbhNew->do("insert into wikipedia select distinct t.* from ".$connectParams->{database}.".wikipedia t, pfamA_wiki f where t.auto_wiki=f.auto_wiki");
$dbhNew->do("insert into literature_references select distinct t.* from ".$connectParams->{database}.".literature_references t, pfamA_literature_references f where t.auto_lit=f.auto_lit");
$dbhNew->do("insert into clans select distinct t.* from ".$connectParams->{database}.".clans t, clan_membership f where t.auto_clan=f.auto_clan");

$logger->info("Inserting Clan related data.");
foreach my $table (qw(clan_database_links clan_lit_refs clan_wiki released_clan_version)){
  $dbhNew->do("insert into $table select t.* from ".$connectParams->{database}.".$table t, clans f where t.auto_clan=f.auto_clan");  
}        
$dbhNew->do("insert into wikipedia select distinct t.* from ".$connectParams->{database}.".wikipedia t, clan_wiki f where t.auto_wiki=f.auto_wiki");
$dbhNew->do("insert into literature_references select t.* from ".$connectParams->{database}.".literature_references t, clan_lit_refs f where t.auto_lit=f.auto_lit");

$logger->info("Inserting pfamseq and pdb related data.");
#Use replace to get round duplicates...
my $seqSth = $dbhNew->prepare("replace pfamseq select * from ".$connectParams->{database}.".pfamseq where auto_pfamseq=?");
my $pdbSth = $dbhNew->prepare("replace pdb_residue_data select * from ".$connectParams->{database}.".pdb_residue_data where auto_pfamseq=?");
my $othSth = $dbhNew->prepare("replace other_reg select * from ".$connectParams->{database}.".other_reg where auto_pfamseq=?");
my $disSth = $dbhNew->prepare("replace pfamseq_disulphide select * from ".$connectParams->{database}.".pfamseq_disulphide where auto_pfamseq=?");
my $marSth = $dbhNew->prepare("replace pfamseq_markup select * from ".$connectParams->{database}.".pfamseq_markup where auto_pfamseq=?");

my %autos;

my $commitPoint = 1000;
#This probably would benefit from auto_commit being turn off.
$dbhNew->begin_work;
foreach my $table (qw(pfamA_reg_seed pfamA_reg_full_significant pfamA_reg_full_insignificant edits)){
  my $autoSeq = $dbhNew->prepare("select auto_pfamseq from $table");
  $autoSeq->execute;
  my $data  = $autoSeq->fetchall_arrayref;
  foreach my $d (@$data){
    next if(exists($autos{$d->[0]}));
    $seqSth->execute($d->[0]);
    $pdbSth->execute($d->[0]);
    $othSth->execute($d->[0]);
    $disSth->execute($d->[0]);
    $marSth->execute($d->[0]);
    $autos{$d->[0]}++;
  }
  $autoSeq->finish;
  my @keys = keys(%autos);
  if (scalar(@keys) > $commitPoint){
    $dbhNew->commit;
    $dbhNew->begin_work;
    $commitPoint += 1000;
  }
}
$seqSth->finish;

$logger->info("Inserting taxonomy and more pdb related data.");
$dbhNew->do("insert into ncbi_taxonomy select distinct t.* from ".$connectParams->{database}.".ncbi_taxonomy t, pfamseq s where s.ncbi_taxid=t.ncbi_taxid");

$dbhNew->do("insert into pdb select distinct t.* from ".$connectParams->{database}.".pdb t, pdb_residue_data r where r.pdb_id=t.pdb_id");
foreach my $table (qw(pdb_author pdb_image)){
  $dbhNew->do("insert into $table select t.* from ".$connectParams->{database}.".$table t, pdb f where t.pdb_id=f.pdb_id");  
}
$dbhNew->do("SET FOREIGN_KEY_CHECKS = 1;");

foreach my $t (qw(taxonomy evidence dead_clans dead_families markup_key VERSION)){
  $dbhNew->do("insert into $t select * from ".$connectParams->{database}.".$t");
}