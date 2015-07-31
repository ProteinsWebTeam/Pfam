#!/usr/bin/env perl
#
#finish half populated pdb redisue data table
#to be used when pud-getPdbDataAndMapping.pl has failed to complete during population of pdb_residue_data
#likely cause is a database time out
#pud-getPdbDataAndMapping.pl reports the pdb entry it is working on, use this as an arguement for this script:
#for example: finish_pdbs.pl 3GT1
#this script will identify all pdbs for which there is no data in pdb_residue data and will carry on populating this table


use strict;
use warnings;
use DBI;
use Data::Printer;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

#make hash of pdbs which are done

my $last = $ARGV[0];

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $schema = $pfamDB->getSchema;
my $dbhp = $pfamDB->getSchema->storage->dbh;

$ENV{TNS_ADMIN}='/ebi/msd/software/common/tns_admin';

#Switch to commented out line once the pfam_search account is set up
my $dbh = DBI->connect("dbi:Oracle:pdbe_live", "search_interpro", "search_interpro55");
#my $dbh = DBI->connect("dbi:Oracle:pdbe_live", "search_pfam", "search_pfam55");

my %done;
my %todo;

my $stdone = $dbhp->prepare("select distinct pdb_id from pdb_residue_data") or die "Can't prepare statement $!\n";
my $sttodo = $dbhp->prepare("select distinct pdb_id from pdb") or die "Can't prepare statement $!\n";
#to get residue data
my $sthr = $dbh->prepare("select
 a.entry_id,
 a.auth_asym_id,
 a.pdb_seq_id,
 a.chem_comp_id,
 a.auth_seq_id,
 a.auth_seq_id_ins_code,
 a.observed,
 b.dssp_symbol,
 a.accession,
 a.unp_one_letter_code,
 a.unp_seq_id
from
 sifts_xref_residue a
left join
 ss_monomer_property b
on
 a.entry_id = b.entry_id AND
 a.struct_asym_id = b.struct_asym_id AND
 a.pdb_seq_id = b.residue_id
where
 a.entry_id = ?");

$stdone->execute();
my $arrayref1 = $stdone->fetchall_arrayref;
foreach my $row1 (@$arrayref1){
	$done{$row1->[0]}=1;
}

#remove the half done id - 3mvn - if it is in the hash

if ($done{$last}){
	delete $done{$last};
}

#make hash of all pdbs in the pdb table (not in pdb db in case there are new ones) excluding those in done

$sttodo->execute();
my $arrayref2 = $sttodo->fetchall_arrayref;
foreach my $row2 (@$arrayref2){
my $id = $row2->[0];
	if (!$done{$id}){
		$todo{$id} = 1;
	}
}


#for each entry in the todo hash populate residue data

foreach my $pdbid (keys %todo){
	$pdbid = lc($pdbid);
	print "Deleting old PDB residue information for $pdbid\n";
	$pfamDB->getSchema->resultset('PdbResidueData')->search( { 'pdb_id' => uc($pdbid) } )->delete;

	print "Searching for PDB residue data for $pdbid\n";
	$sthr->execute($pdbid);

	my $tbl_ary_ref = $sthr->fetchall_arrayref;

#make some changes to this array ref to populate the db
#need to change observed (Y/N) into an integer (1/0)
#fix insert code so null is displayed when there is no insert code
#can only populate where pfamseq_acc is not NULL and also the pfamseq_acc is found in pfamseq/uniprot table - done in loadPdbResidueData
#make these changes directly into the array ref

	foreach my $row (@$tbl_ary_ref){

		if ($row->[6] eq "Y"){
			$row->[6] = 1;
		} elsif ($row->[6] eq "N"){
			$row->[6] = 0;
		}
		my $ins;
		unless ($row->[5] =~ /\w+/){
			$row->[5] = $ins;
		}
#uppercase pdbid
		$row->[0] = uc($row->[0]);
	
	}


	my $guard = $pfamDB->getSchema->txn_scope_guard;

	print "commiting to database\n";

	$pfamDB->getSchema->resultset('PdbResidueData')->loadPdbResidueData($tbl_ary_ref, $dbhp);


	$guard->commit;

	$sthr->finish;

}
$dbh->disconnect;
