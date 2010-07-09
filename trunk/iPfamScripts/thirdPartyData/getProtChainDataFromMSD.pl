#!/usr/local/bin/perl
#
# This is the iPfam version of the script that retrieves data from the MSD database.
#

use strict;
use warnings;
use Bio::Pfam;
use Data::Dumper;
use DBI;

$ENV {"ORACLE_HOME"} = "/software/oracle";
my $output_dir = "/tmp/";
#Okay - This is the set up for connecting to the msd database. This should go into a module!!!!

my $host = "ocs16";
my $port = "1527";
my $db = "msd";
my $password = "search";
my $user = "search";

my $dbh = DBI->connect("dbi:Oracle:host=$host;sid=$db;port=$port", $user, $password) or  die "Couldn't connect to database: ".DBI->errstr;
# We should now have a connection string!


#my $ipfamDbh = DBI->conect("dbi:mysql:host");

###################################
# Get the PDB entry data from MSD #
###################################

my @entryIds; # We will store all of the entry ids in this array so that we can get the residue mapping entry by entry. 



#The run the system command.....

#system("mysql -h pfam -u pfam -pmafp1 -e \'load data infile \"$output_dir/msd_data.dat\" into table msd_data\'  $rdb") and die "Error uploading msd_data file to $rdb :[$!]\n";

# select e.accession_code,  e.entry_id, e.header, e.title,  e.creation_date, e.res_val, e.short_experiment_type, e.experiment_type,  pl.pubmedid
# from
# (select t.accession_code, t.entry_id, t.pubmedid, t.ordinal
# from whouse1.pubmedlist t where nvl(t.ordinal,0) = 0 )pl,
# entry e
# where
# pl.accession_code  (+) = e.accession_code


#Now get the data for the pdb table
my $sthEntryData = $dbh->prepare("
SELECT e.accession_code, 
       e.entry_id, 
       e.header, 
       e.title,  
       e.creation_date, 
       e.res_val, 
       e.short_experiment_type, 
       e.experiment_type
FROM entry e");

$sthEntryData->execute;

my $autoPdb =1;
my %autoPdb;
open(MSD2, ">$output_dir/entryData.dat") || die "Could not open entryData.dat:[$!]";
while (my $hash = $sthEntryData->fetchrow_hashref){
    print MSD2 $$hash{ENTRY_ID}."\t";
    print MSD2 $$hash{ACCESSION_CODE}."\t";
    chomp($$hash{HEADER});
    $$hash{TITLE} =~ s/\n//g; #erros with unitialised
    print MSD2 $$hash{HEADER}."\t";
    print MSD2 $$hash{TITLE}."\t";
    if($$hash{CREATION_DATE} =~ /(\d{2}\-\S{3}\-\d{2})/){
	     print MSD2 "$1\t";
    }else{
	     print MSD2 "\t";
    }
    if($$hash{RES_VAL}){
      print MSD2 $$hash{RES_VAL}."\t"; #Put in sprintf
    }else{
      print MSD2 "\t"; 
    }
    print MSD2 $$hash{SHORT_EXPERIMENT_TYPE}."\t";
    print MSD2 $$hash{EXPERIMENT_TYPE}."\t\t\n";
    #print MSD2 $$hash{PUBMEDID}."\t";
    push(@entryIds, $$hash{ENTRY_ID});
}
$sthEntryData->finish;
close(MSD2);


###########################
# Get the PDB author data #
###########################

#Now get the data for the pdb_author table
my $sthAuthor = $dbh->prepare("SELECT entry_id, ordinal, last_name, name_initials FROM author");
$sthAuthor->execute;
open(MSD3, ">$output_dir/entryAuthor.dat") || die "Could not open entryAuthor.dat:[$!]";
while (my $hash = $sthAuthor->fetchrow_hashref){
	print MSD3 $$hash{ENTRY_ID}."\t";
	print MSD3 $$hash{ORDINAL}."\t";
	print MSD3 $$hash{LAST_NAME}."\t";
	print MSD3 $$hash{NAME_INITIALS}."\n";
}
$sthAuthor->finish;
close(MSD3);

################################################################
# Finally get the residue by residue mapping and the DSSP code #
################################################################

open(MSD, ">$output_dir/msd_data.dat") || die "Could not open msd_data.dat:[$!]";
my $sthDssp = $dbh->prepare("SELECT residue_id, dssp_symbol from residue_data where entry_id = ?");
#Now get the swissprot mapping
my $sthMapping =$dbh->prepare(
"SELECT 
S.ENTRY_ID,
S.RESIDUE_ID,
S.CHAIN_CODE, 
S.CHAIN_PDB_CODE, 
S.RESIDUE_SERIAL, 
S.RESIDUE_PDB_CODE, 
S.RESIDUE_PDB_SEQ, 
S.SP_PRIMARY_ID, 
S.SP_1_LETTER_CODE, 
S.SP_SERIAL,
S.ACCESSION_CODE
FROM 
SWISS_PROT_MAPPING S
WHERE S.ENTRY_ID = ?
order by 
S.ENTRY_ID, 
S.CHAIN_CODE, 
S.RESIDUE_SERIAL ASC");


foreach my $entryId (@entryIds){

  #First get the residue dssp code;
  $sthDssp->execute($entryId);
  my $dsspRes = $sthDssp->fetchall_arrayref;
  my %dssp = map{$$_[0] => $$_[1]}@{$dsspRes};

  $sthMapping->execute($entryId);
  while (my $hash = $sthMapping->fetchrow_hashref){
    if($$hash{ACCESSION_CODE}  && defined($$hash{RESIDUE_PDB_SEQ})){#Something is unitialised here
      #
      print MSD "$$hash{ENTRY_ID}\t";
      print MSD "$$hash{ACCESSION_CODE}\t";
      

      if($$hash{CHAIN_PDB_CODE}){
      	print MSD "$$hash{CHAIN_PDB_CODE}\t";
      }
      print MSD "$$hash{SP_PRIMARY_ID}\t";
      
      print MSD "$$hash{RESIDUE_PDB_SEQ}\t";
      print MSD "$$hash{RESIDUE_PDB_CODE}\t";
      if($dssp{$$hash{RESIDUE_ID}}){
      	print MSD "$dssp{$$hash{RESIDUE_ID}}\t";
      }else{
	      print MSD "\t";
      }
      print MSD "$$hash{SP_SERIAL}\t";
      print MSD "$$hash{SP_1_LETTER_CODE}\n";
      
    }
  }
}
$sthMapping->finish;
close(MSD);
