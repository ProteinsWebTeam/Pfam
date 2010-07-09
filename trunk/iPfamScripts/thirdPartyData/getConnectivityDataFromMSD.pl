#!/usr/local/bin/perl

use strict;
use warnings;
use Data::Dumper;
use DBI;

#my $rdb = shift; #This is in the new format pfam_X_0

#die "Please provide a database name that you wish to update\n" unless ($rdb);

$ENV {"ORACLE_HOME"} = "/software/oracle";
#my $output_dir = "/pfam/data1/localdbs/msd";
#my $output_dir = "/home/rob/";
#Okay - This is the set up for connecting to the msd database. This should go into a module!!!!

my $host = "ocs16";
my $port = 1527;
my $db = "msd";
my $password = "search";
my $user = "search";

my $dbh = DBI->connect("dbi:Oracle:host=$host;sid=$db;port=$port", $user, $password) or  die "Couldn't connect to database: ".DBI->errstr;
# We should now have a connection string!

#Before performing the query, lets try and open the file
open(CON, ">connectivity.dat") || die "Could not open connectivity.dat:[$!]\n";


#Now get the data for connectivity
my $sthConData = $dbh->prepare("select 
c.CHEM_COMP_ID,
CODE_3_LETTER, 
CHEM_ATOM_1_NAME, 
CHEM_ATOM_2_NAME 
FROM 
chem_comp c, 
chem_bond b where 
c.chem_comp_id=b.chem_comp_id and PDB_TOPOL_VAR_CHEM_COMP_ID is null");
$sthConData->execute;

my $rows = $sthConData->fetchall_arrayref;
$sthConData->finish;

foreach my $r (@{ $rows }){
  my $line1 = join("\t", @$r );
  my $line2 = $r->[0]."\t".$r->[1]."\t".$r->[3]."\t".$r->[2];
  print CON "$line1\n$line2\n";
}
close(CON);

