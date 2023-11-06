#!/usr/bin/env perl

use strict;
use warnings;
use DBI;
use Data::Printer;
use Bio::Pfam::Config;
use Bio::Pfam::PfamLiveDBManager;

my $config = Bio::Pfam::Config->new;

my $pfamDB = Bio::Pfam::PfamLiveDBManager->new( %{ $config->pfamlive } );
my $schema = $pfamDB->getSchema;
my $dbh_pfam = $pfamDB->getSchema->storage->dbh;

# noah
#$ENV{TNS_ADMIN}='/ebi/msd/software/common/tns_admin';

# codon
$ENV{TNS_ADMIN}='/usr/lib/oracle/19.9/client64/network/admin';

my $dbh = DBI->connect("dbi:Oracle:pdbe_live", "search_pfam", "search_pfam55");

my %pdbs;

#to get list of all current pdbs
my $sthe = $dbh->prepare("select id from entry");
print "Fetching pdb ids\n";
$sthe->execute();
my $tbl_ary_refe = $sthe->fetchall_arrayref;
$sthe->finish();

foreach my $row_e (@$tbl_ary_refe){
  $pdbs{$row_e->[0]}=1;
}

#to get data for pdb table (title, date etc)
my $sthp = $dbh->prepare(
  "select id, title, resolution, keywords,to_char(pdb_rev_date, 'yyyy-mm-dd') from entry where id = ?"
);
#get authors for pdb entry
my $stha = $dbh->prepare("select name, ordinal from audit_author where entry_id = ? order by ordinal");

#get methods for pdb entry
my $sthm = $dbh->prepare("select method from exptl b where entry_id= ? ");

print "Fetching data to populate pdb table\n";
foreach my $pdb (keys %pdbs){
  $sthp->execute($pdb);
  $stha->execute($pdb);
  $sthm->execute($pdb);

  my $tbl_ary_ref_p = $sthp->fetchall_arrayref;
  my $tbl_ary_ref_a = $stha->fetchall_arrayref;
  my $names = '';
  my $count = 0;
  foreach my $row_a (@$tbl_ary_ref_a){
    if ($count == 0){
      $names = $row_a->[0];
      $count ++;
    } else {
      $names = $names . ", " . $row_a->[0];
    }
  }

  my $tbl_ary_ref_m = $sthm->fetchall_arrayref;
  my $methods = '';
  my $count2 = 0;
  foreach my $row_m (@$tbl_ary_ref_m){
    if ($count2 == 0){
      $methods = $row_m->[0];
      $count2++;
    } else {
      $methods = $methods . ", " .  $row_m->[0];
    }
  }

  my @data = ($tbl_ary_ref_p->[0][1], $tbl_ary_ref_p->[0][2], $tbl_ary_ref_p->[0][3], $tbl_ary_ref_p->[0][4], $names, $methods);
  
  $pdbs{$pdb}=\@data;
  $stha->finish;
  $sthp->finish;
}


#NOW POPULATE pdb table
print "Populating pdb table\n";

foreach my $id (keys %pdbs){

  my $guard = $pfamDB->getSchema->txn_scope_guard;
  my $r = $pfamDB->getSchema->resultset('Pdb')->update_or_create(
    {
      pdb_id => uc($id),
      keywords => $pdbs{$id}->[2],
      title => $pdbs{$id}->[0],
      date => $pdbs{$id}->[3],
      resolution => $pdbs{$id}->[1],
      method => $pdbs{$id}->[5],
      author => $pdbs{$id}->[4]
    }
  );
  $guard->commit;
}


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

foreach my $pdbid (keys %pdbs){

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

  $pfamDB->getSchema->resultset('PdbResidueData')->loadPdbResidueData($tbl_ary_ref, $dbh_pfam);


  $guard->commit;


  #$sthr->finish;
} #end of loop through pdb ids to query for residue data
$sthr->finish;
$dbh->disconnect;
