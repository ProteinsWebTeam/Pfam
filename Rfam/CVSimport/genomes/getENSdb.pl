#!/software/bin/perl

use strict;
use Getopt::Long;
use Bio::SeqIO;
use DBI;  
use Cwd;
use Rfam;


my $asth;
my $dbh;
   
my $release='56';
#DB connection stuff

    my( $dbHost, $dbPort, $dbUser, $dbName );
    $dbHost = 'ensembldb.ensembl.org';
    $dbPort = '5306';
    #$dbName = $dbname;
    $dbUser = 'anonymous';
    
# set up the DB connection and statement handles
    
    my $dsn    = "dbi:mysql:;host=$dbHost;port=$dbPort";
    my $dbAttr = { RaiseError => 1,
		   PrintError => 1 };
# connect
    $dbh = DBI->connect( $dsn, $dbUser, '', $dbAttr )
	or die "(EE) ERROR: couldn't connect to database: $!";
    
# statement for inserting data into genome_entry table
$asth = $dbh->prepare( 'show databases')
  or die '(EE) ERROR: couldn\'t prepare query to select name and version from coord_system ' . $dbh->errstr;

$asth->execute();  
my $array=$asth->fetchall_arrayref();

    $dbh->disconnect   
	or warn "Error disconnecting: $DBI::errtr\n";

my @ensemble_databases;

foreach my $edb (@$array){
    my $spdb=$edb->[0], "\n";
    if ($spdb=~/\_core\_$release/){
	push(@ensemble_databases, $spdb);

    }
}

print join("\n", @ensemble_databases), "\n";
