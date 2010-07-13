#!/usr/local/bin/perl
#
# This is the iPfam version of the script that retrieves data from the MSD database.
#

use strict;
use warnings;
use Data::Dumper;
use Log::Log4perl qw(:easy);
use DBI;
use Getopt::Long;
use Bio::iPfam::Config;
use Net::SCP;

use Config::General;

my ( $ipfam_config );

GetOptions(
    "ipfam_config=s"  =>  \$ipfam_config
);

#-------------------------------------------------------------------------------
#Initail set up....
#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

$ENV {"ORACLE_HOME"} = "/software/oracle";

my $config  = Bio::iPfam::Config->new;
my $statusdir = $config->statusDir;


unless($statusdir and -d $statusdir){
  $logger->logdie("You need to pass a statusdir in:[$!]");
}

unless ( $ipfam_config ) {
  $logger->logdie( 'You need to give config file containing connection params to ipfam database' );
}
#-------------------------------------------------------------------------------

$logger->info('the input ipfam_config is '.$ipfam_config );

$logger->info( 'writing files to the directory'.$config->localDbsLoc );

#Open up the files
my $output_dir = $config->localDbsLoc."/msd";

unless(-e "$statusdir/fetched_pdb_data"){
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

#-------------------------------------------------------------------------------

my @entryIds; # We will store all of the entry ids in this array so that we can get the residue mapping entry by entry. 

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
from pdbe_ro.citation p where nvl(p.id,0) =0 )pl,
pdbe_ro.entry e
where
e.id = pl.entry_id (+)");

$sthEntryData->execute() or $logger->logdie("Failed to execute entryDataSth:".$dbh->errstr);
$logger->info("Query execution completed".$sthEntryData->fetchrow_hashref );

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

###########################
# Get the PDB author data #
###########################

$logger->info("Getting the PDB author data");
#Now get the data for the pdb_author table
my $sthAuthor = $dbh->prepare("SELECT ENTRY_ID, ORDINAL, NAME FROM PDBE_RO.AUDIT_AUTHOR");
$sthAuthor->execute;

while (my $hash = $sthAuthor->fetchrow_hashref){
	print MSD3 $$hash{ENTRY_ID}."\t";
	print MSD3 ($$hash{ORDINAL} ? $$hash{ORDINAL} : '')."\t";
	print MSD3 ($$hash{NAME} ? $$hash{NAME}: '')."\n";
}
$sthAuthor->finish;
close(MSD3);

################################################################
# Finally get the residue by residue mapping and the DSSP code #
################################################################

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
S.PDB_INS_CODE,
S.ACCESSION,
U.SEQ_VERSION, 
S.ONE_LETTER_CODE, 
S.UNP_SERIAL,
S.DSSP_SYMBOL
FROM 
PDBE_RO.XREF_RESIDUE S,
PDBE_RO.UNP_ENTITY U
WHERE S.ENTRY_ID=U.ENTRY_ID and S.ACCESSION=U.ACCESSION AND S.ENTRY_ID = ?
order by 
S.ENTRY_ID, 
S.STRUCT_ASYM_ID, 
S.RESIDUE_ID ASC"
);

my ($c, $total);
RESIDUE: foreach my $entryId (@entryIds){
  $c++;
  if($c == 1000){
    $total += $c;
    $c = 0;
    $logger->info("Getting the $total entry out of ".scalar(@entryIds)." entries");
  }
  $sthMapping->execute($entryId);
  my $map = $sthMapping->fetchall_arrayref;
  foreach my $row (@{$map}){
    #Make sure the sequence is in pfamseq
    #next unless($acc2auto{$row->[7]});
    #Add the auto pfamseq reference to the accession.  
    #$row->[7] .= "\t".$acc2auto{$row->[7]};
    #$row->[7] .= "\tAUTO_PFAMSEQ";
    my $rowString;
    foreach my $e (@$row){
      $rowString .= (defined($e) ? $e : '\N')."\t";    
    }
    print MSD $rowString."\n";  
  }
}
$sthMapping->finish;
close(MSD);
system("touch $statusdir/fetched_pdb_data") and 
  $logger->logdie("Could not touch $statusdir/fetched_pdb_data");
}else{
  $logger->info("Already fetched the pdb data\n");
}

# Now populating the tables ( pdb_residue_data, pdb, pdb-author );
#  first get the dbh handle;

my $conf;
$conf = new Config::General( "$ipfam_config" ) || $logger->logdie('Cant parse the config file'.$conf) ;
my %ipfamdb_config = $conf->getall;

my $ipfamdb =  $ipfamdb_config{ iPfamDB };

my $ipfam_host      = $ipfamdb->{ host } ;
my $ipfam_database  = $ipfamdb->{ db };
my $ipfam_port      = $ipfamdb->{ port };
my $ipfam_user      = $ipfamdb->{ user };
my $ipfam_password  = $ipfamdb->{ password };

my $ipfam_dbh = DBI->connect("dbi:mysql:host=$ipfam_host;database=$ipfam_database;port=$ipfam_port", $ipfam_user, $ipfam_password) || $logger->logdie( 'Cant get dbh handle for iPfam database'.DBI->errstr );

$logger->info("Uploading data......");

my %ftmap = ( 'msd_data.dat'    => 'pdb_residue_data',
              'entryData.dat'   => 'pdb',
              'entryAuthor.dat' => 'pdb_author' );

#Now copy to the instance and upload

my $scp = Net::SCP->new( { "host"=> $ipfam_host } );
my $tmp = "/tmp/";

foreach my $f (qw( entryData.dat entryAuthor.dat msd_data.dat )){
  
    $scp->put("$output_dir/$f", "$tmp/$f") or die $logger->logdie("Could not scp $output_dir/msd_data.dat to $tmp/$f " . $scp->{errstr});

    my $sth = $ipfam_dbh->prepare("load data infile '$tmp/$f' into table ".$ftmap{$f}) or $logger->logdie("Failed to prepare upload statement for $f:".$ipfam_dbh->errstr);
    $sth->execute or $logger->logdie("Failed to upload $f:".$ipfam_dbh->errstr);
}
