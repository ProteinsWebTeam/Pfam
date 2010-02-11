#!/software/bin/perl

=head1 NAME

getCoordSystem.pl

=head1 DESCRIPTION

Needs the list of databases to connect to.
This code connects to each species database for a given release and 
generates a lookup list for which mapping (coordinate) system is needed
in order to get from the largest to the smallest seq fragment for each
genome. The out put of this code is needed for the assembly mapping code.

=head1 AUTHOR

jd7@sanger.ac.uk

=cut

use strict;
use Getopt::Long;
use Bio::SeqIO;
use DBI;  
use Cwd;
use Rfam;


my($infile,);

&GetOptions(  'infile=s' => \$infile,
	     );

my $dir=cwd;

if (! -e $infile) {
    die "Need to specify the file with list of ensembl databases for this release\n";
}

print STDERR "(ii) Reading in list of database names\n";

my @lines;
open (IN, "$dir/$infile") || die "cant open the infile  $!";
@lines=<IN>;
chomp @lines;
close IN;

open (OUT, ">$infile\.coordmapping") || die "cant open the file for writing to $!";
my ($asth, $bsth);
my $dbh;
#connect for each species and get the co-ordinate system name and version
foreach my $line (@lines){
    my $dbname=$line;
   
#DB connection stuff

    my( $dbHost, $dbPort, $dbUser, $dbName );
    $dbHost = 'ensembldb.ensembl.org';
    $dbPort = '5306';
    $dbName = $dbname;
    $dbUser = 'anonymous';
    
# set up the DB connection and statement handles
    
    my $dsn    = "dbi:mysql:database=$dbName;host=$dbHost;port=$dbPort";
    my $dbAttr = { RaiseError => 1,
		   PrintError => 1 };
# connect
    $dbh = DBI->connect( $dsn, $dbUser, '', $dbAttr )
	or die "(EE) ERROR: couldn't connect to database: $!";
    
# statement for inserting data into genome_entry table
    $asth = $dbh->prepare( 'Select name, version from coord_system where rank=1')
  or die '(EE) ERROR: couldn\'t prepare query to select name and version from coord_system ' . $dbh->errstr;
    
    $bsth = $dbh->prepare( 'SELECT name FROM coord_system WHERE rank = (select max(rank) from coord_system)')
	or die '(EE) ERROR: couldn\'t prepare query to select name ' . $dbh->errstr;


    print STDERR "(ii) Getting coord data for $dbname\n";
    my $data;
    unless( $data=get_coordSystem() ) {
	print STDERR "(WW) WARNING: couldn't get the data from the coord_systems for $dbname";
	next;;
    }
    my ($coord_name, $coord_ver)=@$data;
    
    #get the lowest ranking:
    my $lowest_rank;
    unless( $lowest_rank=get_lowestRank() ) {
	print STDERR "(WW) WARNING: couldn't get the lowest rank from the coord_systems for $dbname";
	next;;
    }
    
    
    print OUT "$dbname\t$coord_name\t$coord_ver\t$lowest_rank\n";
    
    $dbh->disconnect   
	or warn "Error disconnecting: $DBI::errtr\n";
    
    
} #end of each species
close(OUT);
exit(1);
# ###-------------------------------
# #SUBROUTINES

sub get_coordSystem {
     $asth->execute() ;
     my  $row=$asth->fetchrow_arrayref();
     die "(EE) ERROR: error whilst getting data into genome_entry : " 
 	. $dbh->errstr . "\n" if $DBI::err;
     $asth->finish();
     return $row;
   
 }


 sub get_lowestRank {
     $bsth->execute() ;
     my  $rank_name=$bsth->fetchrow();
     die "(EE) ERROR: error whilst getting data into genome_entry : " 
 	. $dbh->errstr . "\n" if $DBI::err;
     $bsth->finish();
     return $rank_name;
   
 }
