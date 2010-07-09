#!/usr/local/bin/perl

use strict;
use warnings;
use Bio::Pfam;
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


#Now get the data for the pdb table
my $sthLigandData = $dbh->prepare("SELECT 
CHEM_COMP_ID,
CHEM_COMP_CODE,
CODE_3_LETTER,
CODE_1_LETTER,
NAME,
SYSTEMATIC_NAME,
NUM_ATOMS_ALL,
NUM_ATOMS_NON_H,
STEREO_SMILES,
NONSTEREO_SMILES,
FORMAL_CHARGE,
RCSB_HETTYPE,
FORMULA,
WEIGHT
FROM CHEM_COMP");
$sthLigandData->execute;

open(LIG, ">LigandData.dat") || die "Could not open LigandData.dat:[$!]\n";
while (my $hash = $sthLigandData->fetchrow_hashref){
  eval{
    chomp($$hash{NAME});
    chomp($$hash{SYSTEMATIC_NAME});
    print LIG $$hash{CHEM_COMP_ID}."\t".
      $$hash{CHEM_COMP_CODE}."\t".
	$$hash{CODE_3_LETTER}."\t".
	$$hash{CODE_1_LETTER}."\t".
	  $$hash{NAME}."\t".
	    $$hash{SYSTEMATIC_NAME}."\t".
	      $$hash{NUM_ATOMS_ALL}."\t".
		$$hash{NUM_ATOMS_NON_H}."\t".
		  $$hash{STEREO_SMILES}."\t".
		    $$hash{NONSTEREO_SMILES}."\t".
		      $$hash{FORMAL_CHARGE}."\t".
			$$hash{RCSB_HETTYPE}."\t".
			  $$hash{FORMULA}."\t".
			    $$hash{WEIGHT}."\n";

  };
 }
$sthLigandData->finish;
close(LIG);


my $sthLigandSyn = $dbh->prepare( "SELECT CHEM_COMP_ID, SERIAL, NAME_SYNONYM FROM COMP_SYNONYM");
$sthLigandSyn->execute;
open(LIGSYN, ">LigandSyn.dat") || die "Could not open LigandSyn.dat:[$!]\n";
while(my $hash = $sthLigandSyn->fetchrow_hashref){
  eval{
    print LIGSYN $$hash{CHEM_COMP_ID}."\t".$$hash{SERIAL}."\t".$$hash{NAME_SYNONYM}."\n";
  };
}
$sthLigandSyn->finish;
close(LIGSYN);




