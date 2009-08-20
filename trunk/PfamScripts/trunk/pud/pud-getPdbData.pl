#!/usr/local/bin/perl
#
# This is the iPfam version of the script that retrieves data from the MSD database.
#

use strict;
use warnings;
use Data::Dumper;
use Log::Log4perl qw(:easy);


use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;


#-------------------------------------------------------------------------------
#Initail set up....
#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

$ENV {"ORACLE_HOME"} = "/software/oracle";

my $config  = Bio::Pfam::Config->new;
my $pfamDB  = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $pfamDBh = $pfamDB->getSchema->storage->dbh;

#-------------------------------------------------------------------------------
#Open up the files
my $output_dir = $config->localDbsLoc."/msd";

open(MSD, ">$output_dir/msd_data.dat") or
   $logger->logdie("Could not open $output_dir/entryResidueData.dat:[$!]");
open(MSD2, ">$output_dir/entryData.dat") or
  $logger->logdie("Could not open $output_dir/entryData.dat:[$!]");
open(MSD3, ">$output_dir/entryAuthor.dat") or
  $logger->logdie("Could not open $output_dir/entryAuthor.dat:[$!]");
  
#-------------------------------------------------------------------------------
#Okay - This is the set up for connecting to the msd database. This should go into the config!!!!

$logger->info("Connecting to the PDBe database");
my $host = "ocs16";
my $port = "1530";
my $db = "msd";
my $password = "pdbe_ro";
my $user = "pdbe_ro";

my $dbh = DBI->connect("dbi:Oracle:host=$host;sid=$db;port=$port", $user, $password)
 or  $logger->logdie("Couldn't connect to database: ".DBI->errstr);

# We should now have a connection string!

#-------------------------------------------------------------------------------
# Get the PDB entry data from MSD 

my @entryIds; # We will store all of the entry ids in this 
#array so that we can get the residue mapping entry by entry. 

$logger->info("Gettting pdb table information");

#Now get the data for the pdb table
my $sthEntryData = $dbh->prepare("
select
e.ID,
e.KEYWORDS, 
e.TITLE, 
e.PDB_REV_DATE_ORIGINAL, 
e.RESOLUTION, 
e.METHOD, 
pl.DATABASE_ID_PUBMED as PUBMEDID
FROM 
(
SELECT  p.database_id_pubmed, p.entry_id, p.id
from pdbe.citation p where nvl(p.id,0) =0 )pl,
pdbe.entry e
where
e.id = pl.entry_id (+)");

$sthEntryData->execute or $logger->logdie("Failed to execute entryDataSth:".$dbh->errstr);

while (my $hash = $sthEntryData->fetchrow_hashref){
    print MSD2 $$hash{ID}."\t";
    print MSD2 ($$hash{KEYWORDS} ? $$hash{KEYWORDS} : '\N')."\t";

    if($$hash{TITLE}){
      $$hash{TITLE} =~ s/\n//g; #erros with unitialised
      print MSD2 $$hash{TITLE};
    }
    print MSD2 "\t";
    if($$hash{PDB_REV_DATE_ORIGINAL} =~ /(\d{2}\-\w{3}\-\d{2})/){
	     print MSD2 "$1\t";
    }else{
	     print MSD2 '\N'."\t";
    }
    if($$hash{RESOLUTION}){
      print MSD2 $$hash{RESOLUTION}."\t"; #Put in sprintf
    }else{
      print MSD2 '\N'."\t"; 
    }
    print MSD2 $$hash{METHOD}."\t";
    if($$hash{PUBMEDID}){
      print MSD2 $$hash{PUBMEDID}."\n";
    }else{
      print MSD2 '\N'."\n";
    }
    push(@entryIds, $$hash{ID});
}
$sthEntryData->finish;
close(MSD2);

#-------------------------------------------------------------------------------
# Get the PDB author data #

$logger->info("Getting the PDB author data");
#Now get the data for the pdb_author table
my $sthAuthor = $dbh->prepare("SELECT ENTRY_ID, ORDINAL, NAME FROM PDBE.AUDIT_AUTHOR");
$sthAuthor->execute;

while (my $hash = $sthAuthor->fetchrow_hashref){
	print MSD3 $$hash{ENTRY_ID}."\t";
	print MSD3 ($$hash{ORDINAL} ? $$hash{ORDINAL} : '')."\t";
	print MSD3 ($$hash{NAME} ? $$hash{NAME}: '')."\n";
}
$sthAuthor->finish;
close(MSD3);

#-------------------------------------------------------------------------------
#Get the residue by residue mapping
$logger->info("Going to get the residue by residue mapping between PDB and UniProt");


#Now get the uniprot mapping
my $sthMapping =$dbh->prepare(
"SELECT 
S.ENTRY_ID,
S.STRUCT_ASYM_ID,
S.AUTH_ASYM_ID, 
S.RESIDUE_ID, 
S.CHEM_COMP_ID, 
S.AUTH_SEQ_ID, 
S.ACCESSION,
U.SEQ_VERSION, 
S.ONE_LETTER_CODE, 
S.UNP_SERIAL,
S.DSSP_SYMBOL
FROM 
PDBE.XREF_RESIDUE S,
PDBE.UNP_ENTITY U
WHERE S.ENTRY_ID=U.ENTRY_ID and S.ACCESSION=U.ACCESSION AND S.ENTRY_ID = ?
order by 
S.ENTRY_ID, 
S.STRUCT_ASYM_ID, 
S.RESIDUE_ID ASC"
);

my ($c, $total);
foreach my $entryId (@entryIds){
  $c++;
  if($c == 1000){
    $total += $c;
    $c = 0;
    $logger->info("Getting the $total entry out of ".scalar(@entryIds)." entries");
  }
  $sthMapping->execute($entryId);
  my $map = $sthMapping->fetchall_arrayref;
  foreach my $row (@{$map}){
    my $rowString;
    foreach my $e (@$row){
      $rowString .= (defined($e) ? $e : '\N')."\t";    
    }
    print MSD $rowString."\n";  
  }
}
$sthMapping->finish;
close(MSD);


#-------------------------------------------------------------------------------
#Delete the old data.
$logger->info("Deleting the contents of pdb");
$pfamDB->getSchema->Resultset('Pdb')->delete 
  or $logger->logdie('Failed to delete the contents of pdb table');

$logger->info("Uploading data......");

my %ftmap = ( 'msd_data.dat'    => 'pdb_residue-data',
              'entryData.dat'   => 'pdb',
              'entryAuthor.dat' => 'pdb_author' );

#Now copy to the instance and upload
foreach my $f (qw( entryData.dat entryAuthor.dat msd_data.dat )){
  system("scp  $output_dir/msd_data.dat ".$config->pfamliveAdmin->{host}.":/tmp/$f") 
    and $logger->logdie("Could not scp to instance:[$!]");
  my $sth = $pfamDBh->("load data infile '/tmp/$file' into table ".$ftmap{$f}) 
    or $logger->logdie("Failed to prepare upload statement for $f:".$pfamDBh->errstr);
  $sth->execute or $logger->logdie("Failed to upload $f:".$pfamDBh->errstr);;
}

