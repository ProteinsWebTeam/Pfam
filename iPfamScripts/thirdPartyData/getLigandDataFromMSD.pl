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
  $ligChemistry->execute() or $logger->logdie( "LigandChemistry query cannot be executed ".$dbh->errstr() );
  
  # open the stream to write the output;
  open( CHEMISTRY, "$output_dir/ligand_chemistry.dat") 
    || $logger->logdie( "$output_dir/ligand_chemistry.dat cannot be opened for writing ".$! );
  
  # retrieve the rows
  while( my $row = $ligChemistry->fetchrow_hashref ){
    print dump( $row );
    exit;
  } # end of while( my$row );
        
}# end of unless ( -e fetchedLigandData )
else{
  $logger->info( 'Ligand data already exists in $output_dir ' );
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


