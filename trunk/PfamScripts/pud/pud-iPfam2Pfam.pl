#!/usr/bin/perl

# This script retrieves data from iPfam database and populates
# the appropriate tables in Pfam database.

use strict;
use warnings;
use DBI;
use Bio::Pfam::config;
use Bio::Pfam::PfamLiveDBManager;
use Log::Log4perl qw(:easy);

#-------------------------------------------------------------------------------
# Start up the logger
my $logger = get_logger();

#-------------------------------------------------------------------------------
# Get the Pfam Config
my $config = Bio::Pfam::Config->new;

#-------------------------------------------------------------------------------
# Connecting to pfam_live database
$logger->info("Connecting to pfam_live database");
my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamliveAdmin } );
my $dbh    = $pfamDB->getSchema->storage->dbh;
$dbh->{AutoCommit} = 0;

# Connecting to iPfam_live database
$logger->info("Connecting to iPfam_live database");
my $ipfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->ipfamlive } );
my $idbh    = $ipfamDB->getSchema->storage->dbh;

#-------------------------------------------------------------------------------
# Retrieve all protein families accessions from pfam_live database
my %pfamAccs;    # Hash for storing the retrieved pfam accessions
# print "Get accessions...";

my $rs = $pfamDB->getSchema->resultset('PfamA')->search(
    {

    },
    { select => 'pfama_acc' }
                                                       );
while ( my $family = $rs->next )
{
    $pfamAccs{ $family->pfama_acc } = 1;
}
# print "done\n";

#-------------------------------------------------------------------------------
# Transfer data from iPfam database to Pfam live database
eval {
    $logger->info("Starting data transfer");
    print "Transfer ligand data...";

    # Transfer data from ipfam_live.ligand and ipfam_live.ligand_chemistry to pfam_live.ligand
    my ( $ligand_id, $name, $formula, $molecular_weight, $smiles, $inchi );
    my %ligandIDs;    # Hash to store all ligand IDs

    my $ligandQuery
      = "select distinct l.ligand_id, lc.name, lc.formula, lc.molecular_weight, lc.smiles, lc.inchi from ligand l join ligand_chemistry lc on l.ligand_id=lc.ligand_id;";
    my $ligandQueryHandler = $idbh->prepare($ligandQuery) or die "cannot execute: " . $idbh->errstr();
    $ligandQueryHandler->execute() or die "Can't execute statement: $DBI::errstr";
    $ligandQueryHandler->bind_columns( \$ligand_id, \$name, \$formula, \$molecular_weight, \$smiles, \$inchi );

    while ( $ligandQueryHandler->fetch() )
    {
        # INSERT INTO ligand(ligand_id, name, formula, molecular_weight, smiles, inchi) VALUES (?,?,?,?,?,?)
        $pfamDB->getSchema->resultset('Ligand')->create( { ligand_id        => $ligand_id,
                                                           name             => $name,
                                                           formula          => $formula,
                                                           molecular_weight => $molecular_weight,
                                                           smiles           => $smiles,
                                                           inchi            => $inchi,
                                                         }
                                                       );
        $ligandIDs{$ligand_id} = 1;
    }
    print "done\n";

    # Transfer data from ipfam_live.pdb_protein_region_int to pfam_live.pfamA_interactions
    my ( $domA, $domB );
    my %domdomInt;    #store domain-domain interactions
    print "Inserting dom dom interactions...";
    my $DomDomIntQuery
      = "select distinct pa.prot_fam_acc, pb.prot_fam_acc from pdb_protein_region_int as i inner join pdb_protein_region as pa on i.region_id_A = pa.region_id inner join pdb_protein_region as pb on i.region_id_B = pb.region_id;";
    my $DomDomIntQueryHandler = $idbh->prepare($DomDomIntQuery) or die "cannot execute: " . $idbh->errstr();
    $DomDomIntQueryHandler->execute() or die "Can't execute statement: $DBI::errstr";
    $DomDomIntQueryHandler->bind_columns( \$domA, \$domB );

    while ( $DomDomIntQueryHandler->fetch() )
    {
        # avoid inserting duplicate inverted pairs (i.e. domainA-domainB and domainB-domainA)
        # make sure that both accessions are present in pfamA table
        if ( !$domdomInt{ $domB . ";" . $domA } and $pfamAccs{$domA} and $pfamAccs{$domB} )
        {

            # INSERT INTO pfamA_interactions(pfamA_acc_A,pfamA_acc_B) VALUES(?,?)
            $pfamDB->getSchema->resultset('PfamAInteraction')->create( { pfama_acc_a => $domA,
                                                                         pfama_acc_b => $domB,
                                                                       }
                                                                     );
        }
    }

    print "done\n";

    # Transfer data from ipfam_live.pdb_protein_region_lig_int to pfam_live.pfamA_ligand
    my ( $dom, $lig );
    print "Inserting dom ligand interactions...";
    my $domLigIntQuery
      = "select distinct p.prot_fam_acc, l.ligand_id from pdb_protein_region_lig_int as i inner join pdb_protein_region as p on i.region_id = p.region_id inner join ligand as l on i.auto_ligand = l.auto_ligand;";
    my $domLigIntQueryHandler = $idbh->prepare($domLigIntQuery) or die "cannot execute: " . $idbh->errstr();
    $domLigIntQueryHandler->execute() or die "Can't execute statement: $DBI::errstr";
    $domLigIntQueryHandler->bind_columns( \$dom, \$lig );

    while ( $domLigIntQueryHandler->fetch() )
    {
        # make sure that pfam accession is present in pfamA table and ligand ID in ligand table
        if ( $pfamAccs{$dom} and $ligandIDs{$lig} )
        {
            # INSERT INTO pfamA_ligand(pfamA_acc, ligand_id) VALUES(?,?)
            $pfamDB->getSchema->resultset('PfamALigand')->create( { pfama_acc => $dom,
                                                                    ligand_id => $lig,
                                                                  }
                                                                );
        }
    }

    print "done\n";
    $logger->info("Transfer successful. Commiting changes to database");
    $dbh->commit();
};

if ($@)
{
    print "Rolling back\n";
    $logger->warn("Rolling back, errors found: $@");
    $dbh->rollback();
}

# -----------------------------------------------------------
# Disconnect from databases

$logger->info("Disconnecting from pfam_live database");
$dbh->disconnect();

$logger->info("Disconnecting from ipfam_live database");
$idbh->disconnect();
