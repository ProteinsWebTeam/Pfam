#!/software/bin/perl

use strict;
use warnings;
use DBI;
use Bio::Pfam::Config;
use Log::Log4perl qw( :easy );
use Bio::Pfam::PfamLiveDBManager;
use Getopt::Long;
use Net::SCP;

# get the realease number as command line argument
my $RELEASE;

GetOptions(
  'release=i' =>  \$RELEASE,
);

#Start up the logger
Log::Log4perl->easy_init();
my $logger = get_logger();

unless($RELEASE){
  $logger->logdie( "Usage: $0 -release <pfam RELEASE number>");  
}

# get the config;
my $config = Bio::Pfam::Config->new;

#get the dbh handle for the database specifed in the config file;
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( 
  %{  $config->pfamliveAdmin }
);
my $pfamdbh = $pfamDB->getSchema->storage->dbh;

# get the production location from the congfig file;
#my $production_loc = $config->productionLoc || "/lustre/pfam/pfam/Production";
my $production_loc = $config->productionLoc 
  || $logger->logdie(" cant get the production location from the config");

$logger->logdie( "Are you sure the RELEASE number [$RELEASE] is correct? Can not find the pfamseq dir")
  unless(-d $config->productionLoc.'/pfamseq'.$RELEASE );
  
open( R, $config->productionLoc.'/pfamseq'.$RELEASE."/reldate.txt") 
  or $logger->logdie("Could not open ".$config->productionLoc.'/pfamseq'.$RELEASE."/reldate.txt :[$!]");

my $uniprotVersion;
while(<R>){
  if(/UniProt Knowledgebase Release (\S+) consists/){
    my $tmpVersion = $1;
    $tmpVersion =~ s/\./_/g;
    $uniprotVersion = "uniprot_".$tmpVersion;
    last; 
  }
}
close(R);

if(!$uniprotVersion){
	$logger->logdie("Failed to get the uniprot version");
}
$logger->info("Proceeding to get mappings for UniProt:".$uniprotVersion);

my $dbh = DBI->connect("dbi:mysql:database=$uniprotVersion;host=cbi3:port=3306", "genero") 
  or $logger->logdie("Failed to get connection to mole:".$DBI::errstr);

my $mapSth = $dbh->prepare("SELECT DISTINCT accession_version, primary_id 
									FROM entry e, accession a, dbxref x 
									WHERE a.entry_id=e.entry_id and e.entry_id=x.entry_id
									AND database_id='GI' and a.qualifier='primary'") or dir $dbh->errstr;
									
$mapSth->execute;
my $res = $mapSth->fetchall_arrayref;

my $f1 = "$production_loc/pfamseq".$RELEASE."/uniprotNcbiMapping.dat";
open(OUT, ">$f1") or die "Could not open $f1:[$!]\n";

foreach my $r (@$res){
  my ($acc, $version) = $r->[0] =~ /(\S+)\.(\d+)/;
  print OUT "$acc\t$version\t$r->[1]\n";  
  
}
close(OUT);


# Save the above into a file.
# Copy to the database instance
# Create a temporary table in that instance and upload
# join against pfamseq and ncbi_seq and insert into mapping table auto_pfamseq, gi where left joins are not null
# left join gi on gi number/primary
# left join pfamseq on pfamseq and seq_version

# my script;

# now copy the uniportNcbiMapping.dat file to the pfamdb2a instance and load it;

my $scp = Net::SCP->new( { "host"=> $pfamDB->{host} } );
my $tmp = "/tmp";
$scp->put("$f1", "$tmp/uniprotNcbiMapping.dat") or $logger->logdie( "Scp command failed: copying uniprotNcbiMapping.dat from production location to ".$config->pfamliveAdmin->{host}." failed ".$scp->{errstr});

# create a temporary table, but for testing i have created a permanant table and load it, later remove it.

eval{
  $pfamdbh->do( 
    "CREATE TEMPORARY TABLE uniprotNcbiMapping (
       pfamseq_acc VARCHAR(6) NOT NULL
      , seq_version TINYINT(4) NOT NULL
      , gi INT(10) UNSIGNED NOT NULL
      , PRIMARY KEY (pfamseq_acc, gi, seq_version)
     )engine = InnoDB;"
  );
};

if( $@ ){
  $logger->logdie( "SQL: cant create temporary table uniprotNcbiMapping ".$pfamdbh->errstr);    
}else{
  $logger->info( "SQL:Table uniprotNcbiMapping created succesfully" );
}

# load the data from the file in pfamdb2a instance to this table;

my $load_sth = $pfamdbh->prepare( "LOAD DATA INFILE '/tmp/uniprotNcbiMapping.dat' INTO TABLE uniprotNcbiMapping ");

eval{
  $load_sth->execute;
};
if( $@ ){
  $logger->logdie( "SQL Error: cant load the data from the file to the table uniprotNcbiMapping".$load_sth->errstr );  
}else{
  $logger->info( "Data loaded succesfully into table uniprotNcbiMapping" );
}


# now query to get the auto_pfamseq ,gi from pfamseq, ncbi_seq for corresponding data in uniprotNcbiMapping and 
# load them to uniprotNcbiMapping;

my $select_sth = $pfamdbh->prepare( "
    INSERT INTO ncbi_map SELECT pfamseq.auto_pfamseq ,uniprotNcbiMapping.gi 
      FROM uniprotNcbiMapping, pfamseq, ncbi_seq 
      WHERE uniprotNcbiMapping.pfamseq_acc = pfamseq.pfamseq_acc 
      AND uniprotNcbiMapping.seq_version = pfamseq.seq_version 
      AND uniprotNcbiMapping.gi  = ncbi_seq.gi 
      AND pfamseq.pfamseq_acc is not null 
      AND ncbi_seq.gi is not null
"); 

eval{
  $logger->info( "Going to populate ncbi_map table");
  $select_sth->execute;
};
if( $@ ){
  $logger->logdie( "SQL Error: cant write the join data to file on machine pfamdb2a ".$pfamdbh->errstr );  
}else{
  $logger->info( "Join succesful: written the output to file on pfamdb2a " );
}
  
$logger->info( "SCRIPT FINISHED HERE, QUITING SMOOTHLY....");
  
  
  
  
  
  
  
  
  
  
  
  
  
  
