#!/usr/local/bin/perl
#===============================================================================
#
#         FILE:  getLigandDataFromMSD.pl
#
#        USAGE:  ./getLigandDataFromMSD.pl  
#
#  DESCRIPTION: Script to get the ligand data from the MSD database and populate 
#               the following tables, ligand_chemistry, pdb_connectivity,
#               ligand_synonyms and ligand_summary. 
#
# REQUIREMENTS:  ---
#       AUTHOR:  Prasad Gunasekaran (pg6@sanger.ac.uk )
#      VERSION:  1.0
#      CREATED:  13/07/2010 10:30:04
#     REVISION:  ---
#===============================================================================

use strict;
use warnings;

use Data::Dump qw(dump);
use Net::SCP;
use Log::Log4perl qw(:easy);
use DBI;
use Getopt::Long;
use Config::General;

use Bio::iPfam::Config;

my ( $ipfam_config );

GetOptions(
    "ipfam_config=s"  =>  \$ipfam_config
);

#-------------------------------------------------------------------------------

#Start up the logger
Log::Log4perl->easy_init($DEBUG);
my $logger = get_logger();

$ENV {"ORACLE_HOME"} = "/software/oracle";

my $config  = Bio::iPfam::Config->new;
my $statusdir = $config->statusDir;

$logger->info( "the statusDir is located in $statusdir" );

unless( -d $statusdir){
  $logger->logdie("You need to pass a statusdir in:[$!]");
}

unless ( $ipfam_config ) {
  $logger->logdie( 'You need to give config file containing connection params to ipfam database' );
}
#-------------------------------------------------------------------------------
# data files are written in localdbs/msd for populating into the database;
my $output_dir = $config->localDbsLoc."/msd";

# check whether the ligand data is already fetched and the status is updated in the statusdir
unless ( -e "$statusdir/fetchedLigandData" ){
  
  # try to connect to PDBe database and die if it fails,
  $logger->info( 'Connecting to PDBe database' );
  my $host = "ocs16";
  my $port = "1530";
  my $db = "msd";
  my $password = "pdbe_ro";
  my $user = "pdbe_ro";
  
  my $dbh = DBI->connect("dbi:Oracle:host=$host;sid=$db;port=$port", $user, $password)
   or  $logger->logdie("Couldn't connect to database: ".DBI->errstr);
  
  #-------------------------------------------------------------------------------
  # PROCESS 1: getting data for ligand chemistry table;
  # prepare the query to get the ligand Chemistry data from MSD;
  my $ligChemistry = $dbh->prepare( "SELECT
                        cc.id as CHEM_COMP_ID,
                        cc.id as CHEM_COMP_CODE,
                        cc.three_letter_code as CODE_3_LETTER,
                        cc.one_letter_code as CODE_1_LETTER,
                        cc.name,
                        cs2.identifier as SYSTEMATIC_NAME,
                        cc.number_atoms_all as NUM_ATOMS_ALL,
                        cc.number_atoms_nh as NUM_ATOMS_NON_H,
                        cd3.descriptor as STEREO_SMILES,
                        cd1.descriptor as NONSTEREO_SMILES,
                        cc.formal_charge as FORMAL_CHARGE,
                        cc.type_text as RCSB_HETTYPE,
                        cc.FORMULA,
                        cc.formula_weight as WEIGHT
                        FROM
                        pdbe.chem_comp cc, pdbe.chem_identifier cs2, pdbe.chem_descriptor cd1, pdbe.chem_descriptor cd3
                        where
                        cc.id = cs2.chem_comp_id (+) and
                        cs2.type (+) = 'SYSTEMATIC NAME' and cs2.program (+) = 'ACDLabs' and
                        cc.id = cd1.chem_comp_id (+) and
                        (cd1.type (+) = 'SMILES' and cd1.program (+) = 'CACTVS') and
                        cc.id = cd3.chem_comp_id (+) and
                        (cd3.type (+) = 'SMILES_CANONICAL' and cd3.program (+) = 'CACTVS') "
                      );
  # now execute the query;
  #$ligChemistry->execute() or $logger->logdie( "LigandChemistry query cannot be executed ".$dbh->errstr() );
  
  # open the stream to write the output;
  open( CHEMISTRY, "$output_dir/ligand_chemistry.dat") 
    or $logger->logdie( "$output_dir/ligand_chemistry.dat cannot be opened for writing ".$! );
  
  # retrieve the rows
  while( my $row = $ligChemistry->fetchrow_hashref ){
    print dump( $row );
    exit;
  } # end of while( my$row );
  
  #-------------------------------------------------------------------------------
  # Process 2: getting data for ligand synonym table;
  my $ligSynonym = $dbh->prepare( "select
                      ci.chem_comp_id,
                      ci.identifier as NAME_SYNONYM
                      FROM 
                      pdbe.chem_identifier ci 
                      where ci.type = 'NAME_SYNONYM'"
                   );
  
  # now execute the query to get the synonyms;
  $ligSynonym->execute() or $logger->logdie( 'Ligand synonym query could not be executed '.$dbh->errstr() );
  
  # now open the stream to write the output;
  open( SYN, "$output_dir/ligand_synonym.dat") 
    or $logger->logdie( "$output_dir/ligand_synonym.dat cant be opened for writing ".$! );
  
  # now retrieve the rows.
  SYNONYM: while( my $row = $ligSynonym->fetchrow_hashref ){
    print dump( $row );
    last SYNONYM;
  } # end of while( my $row,ligsynonym ) 
   
  #-------------------------------------------------------------------------------
  # Process 3: getting data for pdb_conectivity table;
  my $pdbConnection = $dbh->prepare( "SELECT
                        c.id CHEM_COMP_ID,
                        c.three_letter_code CODE_3_LETTER,Ê
                        b.chem_atom_1_id CHEM_ATOM_1_NAME,Ê
                        b.chem_atom_1_id CHEM_ATOM_2_NAMEÊ
                        FROMÊ
                        pdbe.chem_comp c,Ê
                        pdbe.chem_bond b whereÊ
                        c.id=b.chem_comp_idÊ
                        order by c.id, b.ordinal" 
                      );
  
  # now execute the query to get the synonyms;
  $pdbConnection->execute() or $logger->logdie( 'pdb connectivity query could not be executed '.$dbh->errstr() );
  
  # now open the stream to write the output;
  open( SYN, "$output_dir/pdb_connectivity.dat") 
    or $logger->logdie( "$output_dir/pdb_connectivity.dat cant be opened for writing ".$! );
  
  # now retrieve the rows.
  CONNECTION: while( my $row = $pdbConnection->fetchrow_hashref ){
    print dump( $row );
    last CONNECTION;
  } # end of while( my $row,pdbConnection ) 
                        
  #-------------------------------------------------------------------------------
        
}# end of unless ( -e fetchedLigandData )
else{
  $logger->info( 'Ligand data already exists in $output_dir ' );
}

#-------------------------------------------------------------------------------
# Now, we have data in files, scp them using the Net::SCP module and copy it to the
# host mentioned in the config file and load them in to the database;
=out
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

# hash to store the mappings btw the table to populate and the files;

my %ftmap = ( 'ligand_chemistry.dat'    => 'ligand_chemistry',
              'ligand_synonym.dat'      => 'ligand_synonyms',
              'pdb_connectivity.dat'    => 'pdb_connectivity' );

#Now copy to the instance and upload

my $scp = Net::SCP->new( { "host"=> $ipfam_host } );
my $tmp = "/tmp/";

foreach my $f (qw( ligand_chemistry.dat ligand_synonym.dat pdb_connectivity.dat  )){
  
    $scp->put("$output_dir/$f", "$tmp/$f") or die $logger->logdie("Could not scp $output_dir/$f to $tmp/$f " . $scp->{errstr});

    my $sth = $ipfam_dbh->prepare("load data infile '$tmp/$f' into table ".$ftmap{$f}) or $logger->logdie("Failed to prepare upload statement for $f:".$ipfam_dbh->errstr);
    $sth->execute or $logger->logdie("Failed to upload $f:".$ipfam_dbh->errstr);
}


=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut


