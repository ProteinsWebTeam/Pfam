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
unless ( -e "$statusdir/fetched_ligand_data" ){
  
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
                        pdbe_ro.chem_comp cc, pdbe_ro.chem_identifier cs2, pdbe_ro.chem_descriptor cd1, pdbe_ro.chem_descriptor cd3
                        where
                        cc.id = cs2.chem_comp_id (+) and
                        cs2.type (+) = 'SYSTEMATIC NAME' and cs2.program (+) = 'ACDLabs' and
                        cc.id = cd1.chem_comp_id (+) and
                        (cd1.type (+) = 'SMILES' and cd1.program (+) = 'CACTVS') and
                        cc.id = cd3.chem_comp_id (+) and
                        (cd3.type (+) = 'SMILES_CANONICAL' and cd3.program (+) = 'CACTVS') "
                      );
  # now execute the query;
  $ligChemistry->execute() or $logger->logdie( "LigandChemistry query cannot be executed ".$dbh->errstr() );
  
  # open the stream to write the output for ligand chemistry table;
  open( CHEMISTRY, ">$output_dir/ligand_chemistry.dat") 
    or $logger->logdie( "$output_dir/ligand_chemistry.dat cannot be opened for writing ".$! );
  
  open( SUMMARY, ">$output_dir/ligand_summary.dat" )
    or $logger->logdie( "$output_dir/ligand_summary.dat cannot be opened for writing ".$! );

  # retrieve the rows
  my $count = 0;
  while( my $row = $ligChemistry->fetchrow_hashref ){
    
    $count++;
      
    # chomp the name and systematic name to remove trailing characters;
   
    print CHEMISTRY $row->{ CHEM_COMP_ID } ."\t";
    
    if( $row->{ CODE_3_LETTER } ){
      print CHEMISTRY $row->{ CODE_3_LETTER } ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }
    
    if( $row->{ CODE_1_LETTER } ){
      print CHEMISTRY $row->{ CODE_1_LETTER } ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }

    if( $row->{  NAME } ){
      chomp( $row->{ NAME } ); 
      print CHEMISTRY $row->{ NAME } ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }
    if( $row->{ SYSTEMATIC_NAME }  ){
      chomp( $row->{ SYSTEMATIC_NAME } ); 
      print CHEMISTRY $row->{ SYSTEMATIC_NAME }  ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }
    if( $row->{ NUM_ATOMS_ALL }  ){
      print CHEMISTRY $row->{ NUM_ATOMS_ALL }  ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }
    if( $row->{ STEREO_SMILES } ){
      print CHEMISTRY $row->{ STEREO_SMILES } ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }
    if( $row->{ NONSTEREO_SMILES } ){
      print CHEMISTRY $row->{ NONSTEREO_SMILES } ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }
    if( $row->{ FORMAL_CHARGE }  ){
      print CHEMISTRY $row->{ FORMAL_CHARGE }  ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }
    if( $row->{ RCSB_HETTYPE } ){
      print CHEMISTRY $row->{ RCSB_HETTYPE } ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }
    if( $row->{ FORMULA } ){
      print CHEMISTRY $row->{ FORMULA } ."\t";
    }else{
      print CHEMISTRY '\N'."\t";
    }       
    if( $row->{ WEIGHT } ){
      print CHEMISTRY $row->{ WEIGHT } ."\n";
    }else{
      print CHEMISTRY '\N'."\n";
    }        
    
     # now for the table ligand_summary, 
    print SUMMARY  $row->{ CHEM_COMP_ID } ."\t";
     
    if( $row->{ CODE_3_LETTER } ){
      print SUMMARY $row->{ CODE_3_LETTER } ."\t";
    }else{
      print SUMMARY '\N'."\t";
    }
    
    if( $row->{ NAME } ){
      print SUMMARY $row->{ NAME } ."\t";
    }else{
      print SUMMARY '\N'."\t";
    }
    if( $row->{ FORMULA } ){
      print SUMMARY $row->{ FORMULA } ."\t".'\N'."\t".'\N'."\n";
    }else{
      print SUMMARY '\N'."\t".'\N'."\t".'\N'."\n";
    }
    
  } # end of while( my$row );
  
  $ligChemistry->finish;
  
  close( CHEMISTRY );
  close( SUMMARY ); 
  
  $logger->info( "I got $count rows in total to populate ligand chemistry & ligand summary ");
  $count = 0;
  #-------------------------------------------------------------------------------
  # Process 2: getting data for ligand synonym table;
  my $ligSynonym = $dbh->prepare( "select
                      ci.chem_comp_id,
                      ci.identifier as NAME_SYNONYM
                      FROM 
                      pdbe_ro.chem_identifier ci 
                      where ci.type = 'NAME_SYNONYM'"
                   );
  
  # now execute the query to get the synonyms;
  $ligSynonym->execute() or $logger->logdie( 'Ligand synonym query could not be executed '.$dbh->errstr() );
  
  # now open the stream to write the output;
  open( SYN, ">$output_dir/ligand_synonym.dat") 
    or $logger->logdie( "$output_dir/ligand_synonym.dat cant be opened for writing ".$! );
  
  # now retrieve the rows.
  while( my $row = $ligSynonym->fetchrow_hashref ){
    $count++;
    print SYN $row->{ CHEM_COMP_ID } ."\t";
           
    if( $row->{ NAME } ){
      chomp ( $row->{ NAME } );
      print SYN $row->{ NAME } ."\n";
    }else{
      print SYN '\N'."\n";
    }

  } # end of while( my $row,ligsynonym ) 
  
  $ligSynonym->finish;
  close( SYN );
  
  $logger->info( "I got $count rows in total to populate ligand synonym table"); 
  $count = 0;

  #-------------------------------------------------------------------------------
  # Process 3: getting data for pdb_conectivity table;
  my $pdbConnection = $dbh->prepare( "SELECT
                        c.id CHEM_COMP_ID,
                        c.three_letter_code CODE_3_LETTER,
                        b.chem_atom_1_id CHEM_ATOM_1_NAME,
                        b.chem_atom_1_id CHEM_ATOM_2_NAME
                        FROM
                        pdbe_ro.chem_comp c,
                        pdbe_ro.chem_bond b where
                        c.id=b.chem_comp_id
                        order by c.id, b.ordinal" 
                      );
  
  # now execute the query to get the synonyms;
  $pdbConnection->execute() or $logger->logdie( 'pdb connectivity query could not be executed '.$dbh->errstr() );
  
  # now open the stream to write the output;
  open( CONNECTIVITY, ">$output_dir/pdb_connectivity.dat") 
    or $logger->logdie( "$output_dir/pdb_connectivity.dat cant be opened for writing ".$! );
  
  # now retrieve the rows.
  while( my $row = $pdbConnection->fetchrow_hashref ){
    $count++;
    print CONNECTIVITY $row->{ CHEM_COMP_ID } ."\t".
        $row->{ CODE_3_LETTER } ."\t".   
        $row->{ CHEM_ATOM_1_NAME } ."\t".   
        $row->{ CHEM_ATOM_2_NAME } ."\n";
          
  } # end of while( my $row,pdbConnection ) 
  
  $pdbConnection->finish;
  close( CONNECTIVITY );
  $logger->info( "I got $count rows in total to populate pdb_connectivity table " ); 
  
  #-------------------------------------------------------------------------------
  
  system("touch $statusdir/fetched_ligand_data") and 
  $logger->logdie("Could not touch $statusdir/fetched_ligand_data");
        
}# end of unless ( -e fetchedLigandData )
else{
  $logger->info( 'Ligand data already exists in $output_dir ' );
}

#-------------------------------------------------------------------------------
# Now, we have data in files, scp them using the Net::SCP module and copy it to the
# host mentioned in the config file and load them in to the database;
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
              'ligand_summary.dat'      => 'ligand_summary',
              'pdb_connectivity.dat'    => 'pdb_connectivity' );

#Now copy to the instance and upload

my $scp = Net::SCP->new( { "host"=> $ipfam_host } );
my $tmp = "/tmp/";

foreach my $f (qw( ligand_chemistry.dat ligand_synonym.dat ligand_summary.dat pdb_connectivity.dat  )){
  
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


