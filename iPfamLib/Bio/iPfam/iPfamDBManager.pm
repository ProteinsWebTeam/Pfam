
#
# BioPerl module for Bio::iPfam::iPfamDBManager
#
# $Author: rdf $

package Bio::iPfam::iPfamDBManager;
use strict;
use warnings;
use iPfamDB;
use Digest::MD5 qw(md5 md5_hex);
use Data::Dumper;
use Log::Log4perl qw( get_logger );
use Data::Dump qw( dump );

my $logger = get_logger(__PACKAGE__);


sub new {
    my $caller = shift;
    my $class = ref($caller) || $caller;
    #my %dbiParams = ( RaiseError => 1, AutoCommit => 0 );
    my %dbiParams = ( RaiseError => 1, AutoCommit => 0 );
    my $self = { user      => "pfam",
		             host      => "pfamdb2a",
		             port      => "3302",
		             database  => "iPfam",
		             driver    => "mysql",
	               debug     => 0,
		             @_,};
		 
    #print STDERR Dumper($self);
    $logger->info( "dbi".
				       ":".$self->{driver}.
				       ":".$self->{database}.
				       ":".$self->{host}.
				       ":".$self->{port}.",".
				       $self->{user}.",".
				       $self->{password});
    eval{
     $self->{'schema'} = iPfamDB->connect("dbi".
				       ":".$self->{driver}.
				       ":".$self->{database}.
				       ":".$self->{host}.
				       ":".$self->{port},
				       $self->{user},
				       $self->{password},
				       \%dbiParams);
    };
    
    if($@){
      $logger->fatal("Failed to get schema for database:: Error:[$@]\n");     
    }
    
    #print STDERR "HERE".Dumper($self);
    return bless($self, $caller);
} 


sub getSchema{
    my $self = shift;
    return $self->{schema};
}

sub DESTROY {
  my $self = shift;
  $self->{'schema'}->storage->disconnect;
}

#---------------------------- Biomolcule Objecs -------------------------------------

sub getLigandChemistryData{
  my ($self, $ligand) = @_;

  my $ligandRDBObj;
  
  if($ligand->hetID  && $ligand->numHetAtoms){
    $ligandRDBObj = $self->getSchema
                          ->resultset("LigandChemistry")
                            ->find({'three_letter_code' => $ligand->hetID,
                                    'num_atoms_no_H'     => $ligand->numHetAtoms },
#                                    { key => 'ligand_chemistry_three_letter_code'}
                                    );
  }
  if(!$ligandRDBObj and $ligand->hetID){
    $ligandRDBObj = $self->getSchema
                          ->resultset("LigandChemistry")
                            ->find({'three_letter_code' => $ligand->resName },
#                            { key => 'ligand_chemistry_three_letter_code'}
                            );
    
  }
  
  if($ligandRDBObj){
    $logger->debug("Got ".$ligandRDBObj->ligand_id );
    return  $ligandRDBObj;
  }
}


sub getChainData {
 my($self, $chain, $pdbId ) = @_;
 $pdbId = lc $pdbId;
 my $chainRDBObj;
 my $fa = $chain->chain2fasta;
 if($chain->type eq 'protein'){
   $logger->debug( "find or create on Protein table for pdb_id \n $pdbId chainId: | ".$chain->internal_chain_id ."\n sequence: $fa \n");
   $chainRDBObj = $self->getSchema
                        ->resultset("Protein")
                          ->find_or_create({ 'accession' => $pdbId."_".$chain->internal_chain_id,
                                            'id'        => $pdbId."_".$chain->internal_chain_id,
                                            'seq_version' => 1,
                                            'md5' =>  md5_hex($fa),
                                            'source_db' => 2,
                                            'length' => length($fa),
                                            'sequence' => $fa });
   
   # print OUT "protein\taccession\t".$chainRDBObj->get_column( 'accession')."\n";
   
   $logger->debug( "find or create on PdbChainData table for $pdbId and | ".$chain->internal_chain_id );
   my $test1 = $self->getSchema
          ->resultset("PdbChainData")
            ->find_or_create({ internal_chain_id => $chainRDBObj->id,
                              internal_chain_accession => $chainRDBObj->accession,
                              original_chain => $chain->chainID,
                              pdb_id         => $pdbId },
                              { key => 'internal_chain_accession_idx'});
   
   # print OUT "pdb_chain_data\tinternal_chain_accession\t".$test1->get_column( 'internal_chain_accession')."\n";
   
                              
 }elsif($chain->type eq 'DNA'){
   $logger->debug( 'the chain type is DNA and populating the nucleicacid table ' );
   $chainRDBObj = $self->getSchema
                        ->resultset("NucleicAcid")
                          ->find_or_create({ 'accession' => $pdbId."_".$chain->internal_chain_id,
                                            'id'        => $pdbId."_".$chain->internal_chain_id,
                                            'seq_version' => 1,
                                            'md5' =>  md5_hex($fa),
                                            'source_db' => 2,
                                            'type'  => 'DNA',
                                            'length' => length($fa),
                                            'sequence' => $fa,
                                            'ncbi_code' => 0 },
                                            { key => 'unique_accession' } );
   # print OUT "nucleic_cid\taccession\t".$chainRDBObj->get_column( 'accession')."\n";
                                               
 }elsif($chain->type eq 'RNA'){
  $logger->debug( 'the chain type is RNA and populating the nucleicacid table ' );
   $chainRDBObj = $self->getSchema
                        ->resultset("NucleicAcid")
                          ->find_or_create({ 'accession' => $pdbId."_".$chain->internal_chain_id,
                                            'id'        => $pdbId."_".$chain->internal_chain_id,
                                            'seq_version' => 1,
                                            'md5' =>  md5_hex($fa),
                                            'type' => 'RNA',
                                            'source_db' => 2,
                                            'length' => length($fa),
                                            'sequence' => $fa,
                                            'ncbi_code' => 0 },
                                            { key => 'unique_accession' });
   
   # print OUT "nucleic_cid\taccession\t".$chainRDBObj->get_column( 'accession')."\n";
                                               
 }elsif($chain->type eq 'ligand'){
     $logger->warn("Use getLigandChemsityData");
 }else{
    $logger->warn("Unknown chain type");
 }
  
  if($chainRDBObj){
    return($chainRDBObj);
  }else{
   $logger->warn("Failed to get RDB data for ". $pdbId."_".$chain->internal_chain_id );
  }
} 


sub addLigand{
  my ($self, $ligand, $accession, $ligand_id) =@_;
  my $atoms = $ligand->atoms;
  
  my $ligandRDBObj = $self->getSchema
                       ->resultset("Ligands")
                        ->find_or_create({"accession" => $accession,
                                          "ligand_id" => $ligand_id,
                                          "ligand_number" => $ligand->resSeq,
                                          "chain" => $ligand->chainID,
                                          "atom_start" => $atoms->[0]->serial,
                                          "atom_end"   => $atoms-> [$#{$atoms}]->serial },
                                         { key => 'ligands_uniq_idx' }
                                         );
  # print OUT "ligands\tinternal_ligand_id\t".$ligandRDBObj->get_column('internal_ligand_id')."\n";                                         
  return($ligandRDBObj) if ($ligandRDBObj);
}



#-------------------------------- Interactions ------------------------------------------------
#------(Proteins, Domains/Pfams, Residues, Ligands, Nucleic Acids, Rfams, Dfams, Bases)--------

#Add data to protein-protien interaction tables

sub addPpi{
  my($self, $id1, $id2) = @_;
  
  unless($id1 and $id2){
    $logger->warn("Could not add protein-protein interaction data as both chain accessions were not supplied");
    return; 
  }
    
  my $ppi = $self->getSchema
                  ->resultset("Ppi")
                    ->find_or_create({ "protein_acc_a" => $id1,
                                       "protein_acc_b" => $id2 },
#                                       { key => 'ppiConst' }
                                       );
  ## print OUT "ppi\tprotein_acc_a\t".$ppi->protein_acc_a."\n";
  $logger->debug( "ppi\tppi\t".$ppi->get_column( 'ppi')."\n");                                        
  return($ppi);                            
}

#Add data to protein-protien interaction residue table

sub addPpiRes {
  my($self, $ppi, $id1, $id2, $aa1, $aa2, $bond) = @_;
  my $ppiRes = $self->getSchema
                       ->resultset("PpiRes")
                        ->create({"ppi"     => $ppi,
                                  "protein_acc_a" => $id1,
                                  "protein_acc_b" => $id2,
                                  "residue_a" => $aa1,
                                  "residue_b" => $aa2,
                                  "bond" => $bond },
#                                  { key => 'ppiResConst'}
                                );
  # print OUT "ppi_res\tppi\t".$ppiRes->get_column( 'ppi')."\n";
  return ( $ppiRes );                                           
}

#Get from the protein-protien interaction residue table

sub getPpiRes {
  my($self, $ppi) = @_;
  my @ppiRes = $self->getSchema
                     ->resultset("PpiRes")
                      ->search( { "ppi" => $ppi });
  return ( \@ppiRes );           
}

#Get protein-protien interaction residues between two specific proteins and that fall within range
sub getPpiResWithRange {
  my ( $self, $chainA, $chainB, $aStart, $aEnd, $bStart, $bEnd ) = @_;
  my @interface = $self->getSchema
                         ->resultset("PpiRes")
                          ->search ( { protein_acc_a => $chainA, 
                                       protein_acc_b => $chainB,
                                       residue_a => { '<=' =>  $aEnd, 
                                                      '>=' =>  $aStart },
                                       residue_b => { '<=' =>  $bEnd,
                                                      '>=' =>  $bStart } });                                                      
  if(scalar(@interface)){
    return \@interface; 
  }
}

#Add data to protein-ligand interaction tables

sub addPli{
  my($self, $id1, $id2) = @_;
  
  unless($id1 and $id2){
    $logger->warn("Could not add protein-ligand interaction data as both chain accessions were not supplied");
    return; 
  }
    
  my $pli = $self->getSchema
                  ->resultset("Pli")
                    ->find_or_create({ "protein_acc" => $id1,
                                       "internal_ligand_id" => $id2 },
#                                       { key => 'pliConst' }
                                       );
  
  # print OUT "pli\tpli\t".$pli->get_column( 'pli')."\n";                                       
  return($pli);                            
}

#Add data to protein-ligand interaction residue table

sub addPliRes {
  my($self, $pli, $id1, $id2, $aa1, $aa2, $bond) = @_;
  my $pliRes = $self->getSchema
                       ->resultset("PliRes")
                        ->create({"pli"     => $pli,
                                  "protein_acc" => $id1,
                                  "internal_ligand_id" => $id2,
                                  "residue_a" => $aa1,
                                  "residue_b" => $aa2,
                                  "bond" => $bond },
#                                   { key => 'pliResConst' }
                                   );
  # print OUT "pli_res\tpli\t".$pliRes->get_column( 'pli')."\n";                                       
  
  return ( $pliRes );                                           
}

#Get from the protein-ligand interaction residue table

sub getPliRes {
  my($self, $pli) = @_;
  my @pliRes = $self->getSchema
                     ->resultset("PliRes")
                      ->search( { "pli" => $pli });
  return ( \@pliRes );           
}

#Get protein-protien interaction residues between two specific proteins and that fall within range
sub getPliResWithProteinRange {
  my ( $self, $ligand, $protein, $protStart, $protEnd ) = @_;
  my @interface = $self->getSchema
                         ->resultset("PliRes")
                          ->search ( { internal_ligand_id => $ligand, 
                                       protein_acc        => $protein,
                                       residue_a => { '<=' =>  $protEnd,
                                                      '>=' =>  $protStart } });
  if(scalar(@interface)){
    return \@interface; 
  }
}

#Add data to nucleic-protien interaction tables

sub addNapi{
  my($self, $id1, $id2) = @_;
  my $napi = $self->getSchema
                   ->resultset("Napi")
                     ->find_or_create({ "nucleic_acid_acc" => $id1,
                                           "protein_acc" => $id2 },
#                                          { key => 'napiConst' }
                                          );
  # print OUT "napi\tnapi\t".$napi->get_column( 'napi')."\n";                                          
  return $napi;
}

#Add data to protein-protien interaction residue table

sub addNapiRes {
  my($self, $napi, $na, $protein, $base, $aa, $bond) = @_;
  my $napiRes = $self->getSchema
                       ->resultset("NapiRes")
                        ->create({"napi"     => $napi,
                                  "nucleic_acid_acc" => $na,
                                  "protein_acc" => $protein,
                                  "base" => $base,
                                  "residue" => $aa,
                                  "bond" => $bond },
#                                  { key => 'napiResConst' }
                                  );
  # print OUT "napi_res\tnapi\t".$napiRes->get_column( 'napi')."\n";                                   

  return ( $napiRes );                                           
}

#Get from the nucleic-protien interaction residue table

sub getNapiRes {
  my($self, $napi) = @_;
  my @ppiRes = $self->getSchema
                     ->resultset("NapiRes")
                      ->search( { "napi" => $napi });
  return ( \@ppiRes );           
}

#Get protein-protien interaction residues between two specific proteins and that fall within range
sub getNapiResWithProteinRange {
  my ( $self, $na, $protein, $protStart, $protEnd ) = @_;
                 
  my @interface = $self->getSchema
                         ->resultset("NapiRes")
                          ->search ( { protein_acc => $protein, 
                                       nucleic_acid_acc => $na,
                                       residue => { '<=' =>  $protEnd, 
                                                     '>=' =>  $protStart } });
  if(scalar(@interface)){
    return \@interface; 
  }
}


#Add data to domain-domain interaction tables
sub addDdi{
  my($self, $id1, $id2, $intrachain) = @_;
  
  unless($id1 and $id2){
    $logger->warn("Could not add domain-domain interaction data as both chain accessions were not supplied");
    return; 
  }    
  
  my $ddi = $self->getSchema
                  ->resultset("Ddi")
                    ->find_or_create({"region_id_a" => $id1,
                                       "region_id_b" => $id2,
                                       "intrachain"  => $intrachain},
                                       #{ key => 'ddiConst' }
                                       );
  # print OUT "ddi\tddi\t".$ddi->get_column( 'ddi')."\n";                                       
  return($ddi);                            
}

#Add data to domain-domain interaction residue table
sub addDdiRes {
  my($self, $ddi, $id1, $id2, $aa1, $aa2, $intra, $bond) = @_;
  my $ddiRes = $self->getSchema
                       ->resultset("DdiRes")
                        ->create({"ddi"     => $ddi,
                                  "region_id_a" => $id1,
                                  "region_id_b" => $id2,
                                  "residue_a" => $aa1,
                                  "residue_b" => $aa2,
                                  "intrachain" => $intra,
                                  "bond" => $bond },
#                                  { key => 'ddiResConst' }
                                  );
  # print OUT "ddi_res\tddi\t".$ddiRes->get_column( 'ddi')."\n";
  return ( $ddiRes );                                           
}


#Get from the domain-domain interaction residue table

sub getDdiRes {
  my($self, $ddi) = @_;
  my @ddiRes = $self->getSchema
                     ->resultset("DdiRes")
                      ->search( { "ddi" => $ddi });
  return ( \@ddiRes );           
}


#Add data to domain-nucleic interaction tables
sub addNadi{
  my($self, $id1, $id2) = @_;
  
  unless($id1 and $id2){
    $logger->warn("Could not add nucleic-domain interaction data as both chain accessions were not supplied");
    return; 
  }    
  my $nadi = $self->getSchema
                  ->resultset("Nadi")
                    ->find_or_create({"nucleic_acid_acc" => $id1,
                                       "region_id" => $id2},
#                                       { key => 'nadiConst' }
                                       );
  # print OUT "nadi\tnadi\t".$nadi->get_column( 'nadi')."\n";                                       
  return($nadi);                            
}


#Add data to nucleic acid-domain interaction residue table
sub addNadiRes {
  my($self, $nadi, $id1, $id2, $base, $aa, $bond) = @_;
  my $nadiRes = $self->getSchema
                       ->resultset("NadiRes")
                        ->create({"nadi"     => $nadi,
                                  "nucleic_acid_acc" => $id1,
                                  "region_id" => $id2,
                                  "base" => $base,
                                  "region_residue" => $aa,
                                  "bond" => $bond },
#                                  { key => 'nadiResConst' }
                                  );
  # print OUT "nadi_res\tnadi\t".$nadiRes->get_column( 'nadi')."\n";                                  
  return ( $nadiRes );                                           
}


#Get from the nucleic acid-domain interaction residue table
sub getNadiRes {
  my($self, $nadi) = @_;
  my @nadiRes = $self->getSchema
                     ->resultset("NadiRes")
                      ->search( { "nadi" => $nadi });
  return ( \@nadiRes );           
}




#Add data to domain-ligand interaction tables
sub addDli{
  my($self, $did, $lid) = @_;
  
  unless($did and $lid){
    $logger->warn("Could not add domain-ligand interaction data as both chain accessions were not supplied");
    return; 
  }
    
  my $dli = $self->getSchema
                  ->resultset("Dli")
                    ->find_or_create({ "region_id" => $did,
                                       "internal_ligand_id" => $lid },
#                                       { key => 'dliConst' }
                                       );
  # print OUT "dli\tdli\t".$dli->get_column( 'dli')."\n";                                       
  return($dli);                            
}

#Add data to domain-ligand interaction residue table

sub addDliRes {
  my($self, $dli, $id1, $id2, $aa1, $aa2, $bond) = @_;
  my $dliRes = $self->getSchema
                       ->resultset("DliRes")
                        ->create({"dli"     => $dli,
                                  "region_id" => $id1,
                                  "internal_ligand_id" => $id2,
                                  "region_residue" => $aa1,
                                  "ligand_residue" => $aa2,
                                  "bond" => $bond },
#                                  { key => 'dliResConst'}
                                  );
  # print OUT "dli_res\tdli\t".$dliRes->get_column( 'dli')."\n";
  return ( $dliRes );                                           
}

#Get from the domain-ligand interaction residue table
sub getDliRes {
  my($self, $dli) = @_;
  my @dliRes = $self->getSchema
                     ->resultset("DliRes")
                      ->search( { "Dli" => $dli });
  return ( \@dliRes );           
}



#----------------------------- ATOM AND BONDS -----------------------------------------------

sub addProteinAtomData {
  my($self, $pacc, $pid, $aa, $aa_atom) = @_;
  my $protAtomdObj = $self->getSchema
                           ->resultset("ProteinIntAtoms")
                             ->create({"protein_acc" => $pacc,
                                       "protein_id" => $pid,
                                          "residue"     => $aa->resSeq,
                                          "atom"        => $aa_atom->realName, 
                                          "atom_number" => $aa_atom->serial},
#                                          { key => 'intAtom sConst' }
                                          );
  
  # print OUT "protein_int_atoms\tprotein_acc\t".$protAtomdObj->get_column( 'protein_acc')."\n";                                          
  return $protAtomdObj;
}
 
 sub getProteinAtomAndBondData {
    my($self, $pacc1, $pacc2, $intra) = @_;
    
    # Need to run a query like this: select a.protein_acc, a.atom_number, b.protein_acc, b.atom_number, bond_type from protein_protein_bonds, protein_int_atoms a, protein_int_atoms b where a.atom_acc=atom_a and b.atom_acc=atom_b and b.protein_acc='2ngr_asym.A.1.1' and a.protein_acc='2ngr_asym.B.1.1';
   
    my @atoms = $self->getSchema
                      ->resultset("ProteinProteinBonds")
                        ->search({"atom_a.protein_acc" => $pacc1,
                                  "atom_b.protein_acc" => $pacc2,
                                  "intrachain"        => $intra},
                                  { join        => [ qw( atom_a atom_b ) ],
                                    select      => [ qw( atom_a.protein_acc atom_a.atom_number atom_b.protein_acc atom_b.atom_number atom_a atom_b bond_type ) ],
                                    as          => [ qw( protein_acc_a atom_number_a protein_acc_b atom_number_b atom_a atom_b bond_type ) ]});
    return \@atoms;
 }
 
 sub addNucleicAcidAtomData {
  my($self, $acc, $id, $base, $base_no, $atom, $atom_number) = @_;
  my $nucAtomdObj = $self->getSchema
                       ->resultset("NucleicAcidIntAtoms")
                        ->create({"nucleic_acid_acc" => $acc, 
                                          "nucleic_acid_id"  => $id, 
                                          "base"             => $base_no,
                                          "base_name"        => $base,
                                          "atom"             => $atom, 
                                          "atom_number"      => $atom_number},
#                                          { key => 'intAtomsConst' }
                                          );
  # print OUT "nucleic_acid_int_atoms\tnucleic_acid_acc\t".$nucAtomdObj->get_column( 'nucleic_acid_acc')."\n";                                          
  return $nucAtomdObj;
 }
 
 
sub getNucleicAcidAtomData {
  my($self, $na_acc) = @_; 
  my @atoms = $self->getSchema
                      ->resultset("NucleicAcidIntAtoms")
                        ->search({"nucleic_acid_acc" => $na_acc});
    return \@atoms;
}
 
sub addLigandAtomData {
  my($self, $ligand_id, $ligand, $lig_atom) = @_;
  my $ligAtomdObj = $self->getSchema
                       ->resultset("LigandIntAtoms")
                        ->create({"internal_ligand_id"  => $ligand_id, 
                                          "ligand"      => $ligand->hetID,
                                          "atom"        => $lig_atom->realName, 
                                          "atom_number" => $lig_atom->serial},
#                                          { key => 'intAtomsConst' }
                                          );
  # print OUT "ligand_int_atoms\tatom_acc\t".$ligAtomdObj->get_column( 'atom_acc')."\n";                                          
  return $ligAtomdObj;
}

# original method, which i have changed for new realtionships;
#sub getProteinLigandAtomAndBondData {
#   my($self, $pacc, $ligacc) = @_;
#   my @atoms = $self->getSchema
#                      ->resultset("ProteinLigandBonds")
#                        ->search({"protein_acc" => $pacc,
#                                  "internal_ligand_id" => $ligacc},
#                                  { join        => [ qw( proteinAtom ligandAtom ) ],
#                                    select      => [ qw( proteinAtom.protein_acc 
#                                                         proteinAtom.atom_number 
#                                                         ligandAtom.internal_ligand_id 
#                                                         ligandAtom.atom_number 
#                                                         protein_atom 
#                                                         ligand_atom 
#                                                         bond_type ) ],
#                                    as          => [ qw( protein_acc
#                                                         protein_atom_number 
#                                                         internal_ligand_id 
#                                                         ligand_atom_number 
#                                                         protein_atom 
#                                                         ligand_atom 
#                                                         bond_type ) ]});
#  return \@atoms;
#}
 
sub getProteinLigandAtomAndBondData {
   my($self, $pacc, $ligacc) = @_;
   my @atoms = $self->getSchema
                      ->resultset("ProteinLigandBonds")
                        ->search({"protein_acc" => $pacc,
                                  "internal_ligand_id" => $ligacc},
                                  { join        => [ qw( protein_atom ligand_atom ) ],
                                    select      => [ qw( protein_atom.protein_acc 
                                                         protein_atom.atom_number 
                                                         ligand_atom.internal_ligand_id 
                                                         ligand_atom.atom_number 
                                                         protein_atom 
                                                         ligand_atom 
                                                         bond_type ) ],
                                    as          => [ qw( protein_acc
                                                         protein_atom_number 
                                                         internal_ligand_id 
                                                         ligand_atom_number 
                                                         protein_atom 
                                                         ligand_atom 
                                                         bond_type ) ]});
  return \@atoms;
}

 sub addProteinProteinBond{
   my ($self, $atom1_acc, $atom2_acc, $bond, $distance, $intrachain) = @_;
   my $bondObj = $self->getSchema
                       ->resultset("ProteinProteinBonds")
                        ->create({"atom_a" => $atom1_acc,
                                          "atom_b" => $atom2_acc,
                                          "bond_type" => $bond,
                                          "distance" => $distance,
                                          "intrachain" => $intrachain},
#                                         { key => 'protein_protein_bonds_unique' }
                                         );
   # print OUT "protein_protein_bonds\tatom_a\t".$bondObj->get_column( 'atom_a')."\n";                                          
   return $bondObj if($bondObj);
}


 
sub addProteinLigandBond{
   my ($self, $prot_atom, $lig_atom, $bond, $distance) = @_;
   my $bondObj = $self->getSchema
                       ->resultset("ProteinLigandBonds")
                        ->create({"protein_atom" => $prot_atom,
                                          "ligand_atom" => $lig_atom,
                                          "bond_type" => $bond,
                                          "distance" => $distance},
#                                         { key => 'protein_ligand_bonds_unique' }
                                         );
   # print OUT "protein_ligand_bonds\tprotein_atom\t".$bondObj->get_column( 'protein_atom')."\n";                                          
   return $bondObj if($bondObj);
 }
 
sub addProteinNucleicAcidBond{
   my ($self, $prot_atom, $na_atom, $bond, $distance) = @_;
   my $bondObj = $self->getSchema
                       ->resultset("ProteinNucleicAcidBonds")
                        ->create({"protein_atom" => $prot_atom,
                                          "nucleic_acid_atom" => $na_atom,
                                          "bond_type" => $bond,
                                          "distance" => $distance},
#                                         { key => 'protein_na_bonds_unique' }
                                         );
   # print OUT "protein_nucleic_acid_bonds\tprotein_atom\t".$bondObj->get_column( 'protein_atom')."\n";                                         
   return $bondObj if($bondObj);
 }

sub getProteinNucleicAtomAndBondData{
   my($self, $pacc, $naacc) = @_;
   my @atoms = $self->getSchema
                      ->resultset("ProteinNucleicAcidBonds")
                        ->search({"protein_acc" => $pacc,
                                  "nucleic_acid_acc" => $naacc},
                                  { join        => [ qw( protein_atom nucleic_acid_atom ) ],
                                    select      => [ qw( protein_atom.protein_acc 
                                                         protein_atom.atom_number 
                                                         nucleic_acid_atom.nucleic_acid_acc 
                                                         nucleic_acid_atom.atom_number 
                                                         protein_atom 
                                                         nucleic_acid_atom 
                                                         bond_type ) ],
                                    as          => [ qw( protein_acc
                                                         protein_atom_number 
                                                         na_acc 
                                                         na_atom_number 
                                                         protein_atom 
                                                         na_atom 
                                                         bond_type ) ]});
  return \@atoms;
}

#-------------------------------- Regions ------------------------------------------------

## Regions Queries

sub getDomainsForPDB{
  my ($self, $pdbId, $intChainId) = @_; 
  if($pdbId and $intChainId){
    my @domains = $self->getSchema
                        ->resultset("Domain")
                          ->search("protein_accession" => $pdbId."_".$intChainId); 
    
    $logger->debug("Got ".scalar(@domains)." domains");
    return(\@domains) if(@domains);
  }else{
     $logger->warn('pdb Id or chian data not supplied');
  }
}

sub getRfamsForPDB{
  my ($self, $pdbId, $intChainId) = @_; 
 
  my @domains = $self->getSchema
                      ->resultset("NucleicAcidRegion")
                        ->search("accession" => $pdbId."_".$intChainId); 
  
  $logger->debug("Got ".scalar(@domains)." rfams");
  
  return(\@domains) if(@domains);
}

sub transferDomainAnnotation {
  my($self, $pdbId, $oriChain, $newChain) = @_;
  
  #look up the domain for the old chain first.
  my @domain = $self->getSchema
                     ->resultset("Domain")
                       ->search("protein_accession" => $pdbId."_".$oriChain);

  foreach my $d (@domain){
     my $test = $self->getSchema
           ->resultset("Domain")
            ->find_or_create( "pfam_acc"          => $d->pfam_acc,
                              "start"             => $d->start,
                              "end"               => $d->end,
                              "region_source_db"  => $d->region_source_db,
                              "protein_id"        => $pdbId."_".$newChain,
                              "protein_accession" => $pdbId."_".$newChain);
    # print OUT "domain\tprotein_id\t".$test->get_column( 'protein_id')."\n";                              
  }  
}
#----------------------------- Quality Control -----------------------------------------------

sub addQualityControl {
  my($self, $qcFkey, $intType, $method, $score, $comment) = @_;
  
  my $qcRow = $self->getSchema
                    ->resultset("QualityControl")
                      ->find_or_create( { quality_control => $qcFkey,
                                          int_type        => $intType,
                                          method          => $method,
                                          score           => $score,
                                          comment         => $comment
                                         },
                                         { key => "UQ_quality_control_1"}
                                          );
  
  # print OUT "quality_control\tquality_control\t".$qcRow->quality_control."\n";                                       
  return $qcRow;
}

sub getQualityControl {
  my($self, $qcFkey, $intType, $method) = @_;
  
  my @qcRows = $self->getSchema
                    ->resultset("QualityControl")
                      ->search( { quality_control => $qcFkey,
                                  int_type        => $intType,
                                  method          => $method
                                 });                                        
  return \@qcRows;
}
#-------------------------------- PDB TABLES ------------------------------------------------

#Queries that deal specifically with PDB tables

sub getPdbData {
 my($self, $pdbId, $assembly, $bioMol ) = @_;
 
  $logger->debug( "getPdbData method called with $pdbId" );
  my $pdbRDBObj;
  if($pdbId and $pdbId =~ /^\S{4}$/ ){
       $pdbRDBObj = $self->getSchema
                           ->resultset("Pdb")
                            ->find({"pdb_id" => $pdbId});
  }
 
  if($assembly and $bioMol){
    $pdbRDBObj = $self->getSchema
                        ->resultset("Pdb")
                          ->find_or_create({"pdb_id" => $pdbId,
#                                            "header" => $pdbRDBObj->header,
                                            "keywords"  =>  $pdbRDBObj->keywords,
                                            "title"  => $pdbRDBObj->title,
                                            "date"   => $pdbRDBObj->date,
                                            "resolution" => $pdbRDBObj->resolution,
                                            "method"  =>  $pdbRDBObj->method,
#                                            "experiment_short" => $pdbRDBObj->experiment_short,
#                                            "experiment_long" => $pdbRDBObj->experiment_long,
                                            "pubmed_id" => $pdbRDBObj->pubmed_id,                                            
#                                            "biological_unit" => $bioMol
                                            },
#                                            { key => 'pdb_accession_Idx'}
                                            );
    # print OUT "pdb\tpdb_id\t".$pdbRDBObj->get_column( 'pdb_id')."\n";                                            
  }else{
   $logger->warn("No assembly information supplied and/or biomatrix data supplied."); 
  }
  
  if($pdbRDBObj){
    return($pdbRDBObj);
  }
} 


sub getConnectivity {
  my($self, $resName, $atomName) = @_;
  
  my @connections = $self->getSchema
                          ->resultset("PdbConnectivity")
                            ->search({three_letter_code => $resName,
                                      atom1_name        => $atomName});
  if(scalar(@connections)){
    return \@connections;
  } 
  
}

#------------------------------- Tracking ----------------------------------

sub startJob {
  my($self, $pdbId, $jobType) = @_;
  
  my $row; 
  if($pdbId){
    $row = $self->getSchema
          ->resultset("Tracking")
            ->update_or_create( { pdb_id => $pdbId,
                                  started => \'NOW()',
                                  finished => '',
                                  job_type => $jobType } );
  } 
  return($row);
}

sub endJob{
  my($self, $pdbId) = @_;
   my $row; 
  if($pdbId){
    $row = $self->getSchema
          ->resultset("Tracking")
            ->find( {pdb_id => $pdbId});
    $row->update( { finished => \'NOW()' } );
  } 
  return($row);
}

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut


1;



