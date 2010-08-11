package iPfam;

use strict;
use warnings;

use Cwd;
use Math::Trig;
use Pfetch;
use Bio::iPfam::Structure::PDBFactory;
use Bio::iPfam::Structure::Region;
use Bio::iPfam::Structure::PfamChain;
use Bio::iPfam::Structure::RfamChain;
use Bio::iPfam::iPfamDBManager;

use Log::Log4perl qw( get_logger );
use Data::Dump qw( dump );
use File::Temp;

my $DEBUG  = 1;
my $logger = get_logger(__PACKAGE__);

#We only want to do this once.....
my $carDataRef = &_readCAR;
my $connectivityRef;

=head2 calInts 

  Title    : calInts
  Usage    : iPfam::calInts($pdbObj, $db);
  Function : Caulates all interactions for a given pdb file
  Args     : A pdb (Bio::iPfam::Structure::Chainset) object and aB connection (from Bio::iPfam::iPfamDBManager);
  Returns  : Nothing
  
=cut

sub calInts {
  my ( $pdbObj, $db ) = @_;

  #Need to get some stuff from the database
  #Make sure we know about this PDB file
  my $pdbRdbObj =
    $db->getPdbData( $pdbObj->id, $pdbObj->assembly,
    $pdbObj->biomolecule_number );
  
  unless($pdbRdbObj){
    $logger->warn($pdbObj->id." is not in the iPfam PDB table");
    return; 
  }
  
  $logger->debug("Got $pdbRdbObj");

  #Get the domains, rfams etc. for it!
  $logger->info("Getting domains");
  getRegions( $pdbObj, $db );
  $logger->info("Got domains");
    
  #Chains will include Protein, DNA, RNA and/or ligands
  #if ( scalar( $pdbObj->chains ) > 1 ) {
  if( scalar( @{ $pdbObj->chains } ) ){
    $logger->info( 'We got '.scalar(@{ $pdbObj->chains } ).' chains in total so calculating interchain interactions adn inter domain interactions ' );
    calInterChainInts( $pdbObj, $db, $pdbRdbObj );
    deriveInterDomInts( $pdbObj, $db );
  }

  #So the only thing else to do is to calculate intra chain domain interations
  calIntraDomDomInts( $pdbObj, $db, $pdbRdbObj );

}

sub getRegions {
  my ( $pdbObj, $db ) = @_;

  foreach my $chain ( @{ $pdbObj->chains } ) {

    #If it is protien chain, look of Pfam domains
    if ( $chain->type eq 'protein' ) {
      if ( $chain->internal_chain_id =~ /biomt\.(\S{1})\./ ) {
        my $oriChain = "asym." . $1 . ".1.1";
        $logger->debug("Transfering domain information from $oriChain");
        $db->transferDomainAnnotation( $pdbObj->id, $oriChain,
          $chain->internal_chain_id );
      }
      my $domInfo = $db->getDomainsForPDB( $pdbObj->id, $chain->internal_chain_id );
      
      if ( $domInfo and ref($domInfo) eq "ARRAY" ) {
        $logger->info("********Adding domains*************");
        my $pfamChain = Bio::iPfam::Structure::PfamChain->new;
        $chain->add_pfam_chain($pfamChain);
        
        foreach my $d (@$domInfo) {

          #Make a new region object, populate it and add it to the list of Pfam chains
          my $reg = Bio::iPfam::Structure::Region->new;
          $reg->acc( $d->get_column( 'pfam_acc') );
          $reg->start( $d->start );
          $reg->end( $d->end );
          $reg->uniqueID( $d->region_id );
          $pfamChain->add_region($reg);
          
          #print "teh region for acc is ".$d->get_column( 'pfam_acc' )."\n".dump( $reg )."\n";
          
        } # END OF @$domInfo
        
      }    # Warn if we did not get any domains and we are debuging
      else {
        $logger->debug( "Did not find any domains on " . $chain->internal_chain_id );
        
      }
      
    }    # If is it an RNA chain, look for Rfams
    elsif ( $chain->type eq 'RNA' ) {
      my $rfamInfo =
        $db->getRfamsForPDB( $pdbObj->id, $chain->internal_chain_id );
      if ( $rfamInfo and ref($rfamInfo) eq "ARRAY" ) {
        $logger->info("Adding Rfams");
        my $rfamChain = Bio::iPfam::Structure::RfamChain->new;
        $chain->add_pfam_chain($rfamChain);
        foreach my $d (@$rfamInfo) {

          # Make a new region object, populate it and add it to the Rfam chain.
          my $reg = Bio::iPfam::Structure::Region->new;
          $reg->acc( $d->pfam_acc );
          $reg->start( $d->start );
          $reg->end( $d->end );
          $reg->uniqueID( $d->region_id );
          $rfamChain->add_region($reg);
        }
      }    # Warn if we did not get any Rfams and we are debuging
      else {
        $logger->debug(
          "Did not find any domains on " . $chain->internal_chain_id );
      }
    }    # In the future, we will want to get Dfams.....
    elsif ( $chain->type eq 'DNA' ) {
      $logger->info("Adding Dfams when they arrive!");
    }
  }
}

=head2

  This method calculates all interchain interactions.

=cut

sub calInterChainInts {
  my ( $pdbObj, $db, $pdbRdbObj ) = @_;
  foreach my $chain1 ( @{ $pdbObj->chains } ) {
    foreach my $chain2 ( @{ $pdbObj->chains } ) {
      $logger->debug( "Working on "
          . $chain1->chainID . ","
          . $chain1->type . " and "
          . $chain2->chainID . ","
          . $chain2->type );
      $logger->debug( "Working on "
          . $chain1->chainID . ","
          . $chain1->type . " and "
          . $chain2->chainID . ","
          . $chain2->type );
      #Skip self-self interactions!
#      next if ( $chain1->internal_chain_id eq $chain2->internal_chain_id );
      if ( $chain1->internal_chain_id eq $chain2->internal_chain_id ){
        $logger->debug( "****Self-Self interactions are skipped for ".$chain1->internal_chain_id .'-'. $chain2->internal_chain_id.
         ' , types are '.$chain1->type . "-" . $chain2->type );
        next;  
      }
      
#Depending on the chain types involved, we are going to neeed to call different methods.
#Protein:protein interactions
      if (  $chain1->type eq "protein"
        and $chain2->type eq "protein" )
      {
        $logger->debug( 'calculating Interchain ProteinProtein Interactions*********');
        proteinProteinInts( $chain1, $chain2, $db, $pdbRdbObj );
        $logger->debug( 'finsihed calculating Interchain ProteinProtein Interactions*********');
        
      }    #Protein nucleic-acid interactions
      elsif ( $chain1->type eq "protein"
        and ( $chain2->type eq "DNA" or $chain2->type eq "RNA" ) )
      {
        $logger->debug("Going to calculate interchain protein:nucleic interactions");
        proteinNucleicInts( $chain1, $chain2, $db, $pdbRdbObj );
      }    #
      elsif ( ( $chain1->type eq "DNA" or $chain1->type eq "RNA" )
        and ( $chain2->type eq "DNA" or $chain2->type eq "RNA" ) )
      {

        #TODO - Add na-na interactions!
        #nucleicNucleicInts($chain1, $chain2, $db, $pdbRdbObj);
      }    #Look for protein - ligand interactions
      elsif ( $chain1->type eq "protein" and $chain2->type eq "ligand" ) {
        $logger->debug( 'calculating Interchain Protein ligand Interactions*********');
        proteinLigandInts( $chain1, $chain2, $db, $pdbRdbObj );
      }
      elsif ( $chain1->type eq "ligand" and $chain2->type eq "protein" ) {
        $logger->debug( "chain 1 has to be Protein so skipping this chain 1" );
        #We only want to store stuff one way for ligands.
        next;
      }
      else {
        $logger->warn( "Interaction type between a chain of type "
            . $chain1->type . " and "
            . $chain2->type
            . " is being ignored" );
      }
    }
  }
}

sub proteinProteinInts {
  my ( $chain1, $chain2, $db, $pdbRdbObj ) = @_;

  my $pdbId  = $pdbRdbObj->pdb_id;
  my $ch1Obj = $db->getChainData( $chain1, $pdbId );
  $logger->debug( "Got chain data for $chain1 and $pdbId" );
  my $ch2Obj = $db->getChainData( $chain2, $pdbId );
  $logger->debug( "Got chain data for $chain2 and $pdbId" );
  
  #TODO Fix this query/caching
  my ( $foundInteraction, $ppi, %ppiResStored, %ppiAtomStore, %ppiBondStore );
  my $atomsRef =
    $db->getProteinAtomAndBondData( $ch1Obj->accession, $ch2Obj->accession, 0 );
  
  foreach my $bondObj (@$atomsRef) {
    $ppiAtomStore{ $bondObj->get_column('protein_acc_a') . ":"
        . $bondObj->get_column('atom_number_a') } =
      $bondObj->get_column('atom_a');
    $ppiAtomStore{ $bondObj->get_column('protein_acc_b') . ":"
        . $bondObj->get_column('atom_number_b') } =
      $bondObj->get_column('atom_b');
    $ppiBondStore{ $bondObj->get_column('atom_a') . ":"
        . $bondObj->get_column('atom_b') . ":"
        . $bondObj->get_column('bond_type') } = 1;
  }

#Now look up the reverse to in case the chains have already been considered in the reverse order
#However, we only want to store the atoms......
  $atomsRef =
    $db->getProteinAtomAndBondData( $ch2Obj->accession, $ch1Obj->accession, 0 );
  foreach my $bondObj (@$atomsRef) {
    $ppiAtomStore{ $bondObj->get_column('protein_acc_a') . ":"
        . $bondObj->get_column('atom_number_a') } =
      $bondObj->get_column('atom_a');
    $ppiAtomStore{ $bondObj->get_column('protein_acc_b') . ":"
        . $bondObj->get_column('atom_number_b') } =
      $bondObj->get_column('atom_b');
  }

  foreach my $aa1 ( @{ $chain1->monomers } ) {
    my $ca1 = $aa1->get_primary_atom;
    
  AA:
    foreach my $aa2 ( @{ $chain2->monomers } ) {
      my $ca2 = $aa2->get_primary_atom;
      
      #See if these amino acids look close
      next AA if ( $ca1->distance($ca2) > 20 );
      my $addToInterface = 0;

      #If we get here, these look close
      foreach my $aa2_atom ( @{ $aa2->atoms } ) {
      ATOM:
        foreach my $aa1_atom ( @{ $aa1->atoms } ) {
          my $distance = $aa1_atom->distance($aa2_atom);
          next ATOM if ( $distance > 6 );
          
          #The two atoms are cloes enough to form a bond.
          my $bond = &_bondType( $aa1, $aa2, $aa1_atom, $aa2_atom, $distance,$carDataRef, $db );
#          my $bond;
#          eval{
#            $bond = &_bondType( $aa1, $aa2, $aa1_atom, $aa2_atom, $distance,$carDataRef, $db );
#          };
#          
#          if( $@ ){
#           $logger->fatal( "the error while calcualting bondType is _bondType( $aa1, $aa2, $aa1_atom, $aa2_atom, $distance,$carDataRef, $db ); \n".$@ );
#          }else{
#           $logger->debug( 'No error: the bond is '.$bond);
#          }
          
          if ( $bond && $bond ne "no_bond" ) {
            
            my ( $a1_acc, $a2_acc );
            
            unless ( $ppiAtomStore{ $ch1Obj->accession . ":" . $aa1_atom->serial } ) {
              my $a1 = $db->addProteinAtomData( $ch1Obj->accession, $ch1Obj->id, $aa1, $aa1_atom );
              $ppiAtomStore{ $ch1Obj->accession . ":" . $aa1_atom->serial } = $a1->atom_acc;
            }
            
            $a1_acc = $ppiAtomStore{ $ch1Obj->accession . ":" . $aa1_atom->serial };
            
            unless ( $ppiAtomStore{ $ch2Obj->accession . ":" . $aa2_atom->serial } ) {
              my $a2 = $db->addProteinAtomData( $ch2Obj->accession, $ch2Obj->id, $aa2,$aa2_atom );
              $ppiAtomStore{ $ch2Obj->accession . ":" . $aa2_atom->serial } = $a2->atom_acc;
            }
            
            $a2_acc = $ppiAtomStore{ $ch2Obj->accession . ":" . $aa2_atom->serial };
            
            unless ( $ppiBondStore{ $a1_acc . ":" . $a2_acc . ":" . $bond } ) {
              $db->addProteinProteinBond( $a1_acc, $a2_acc, $bond, $distance, 0 );
              $ppiBondStore{ $a1_acc . ":" . $a2_acc . ":" . $bond }++;
            }
            
            unless ($ppi) {
              #Add the chain
              $ppi = $db->addPpi( $ch1Obj->id, $ch2Obj->id );

              #Get a list of all residues
              my $ppiRes = $db->getPpiRes( $ppi->ppi );

              #Build a hash of them......This stops us adding the samething!
              %ppiResStored = map {
                    $_->residue_a . ":"
                  . $_->residue_b . ":"
                  . $_->bond => 1
              } @$ppiRes;
            }

            unless ( $ppiResStored{ $aa1->resSeq . ":" . $aa2->resSeq . ":" . $bond } ){
              my $res = $db->addPpiRes( $ppi->ppi,    $ch1Obj->id,  $ch2Obj->id, $aa1->resSeq, $aa2->resSeq, $bond );
              $ppiResStored{ $res->residue_a . ":" . $res->residue_b . ":". $res->bond } = 1;
            }
            
            $foundInteraction = 1;
          
          } # end of if( $bond && $bond)
        }
      }
    }
  }

  if ($foundInteraction) {
    my $rowRef = $db->getQualityControl( $ppi->ppi, "ppi", "NACCESS" );

    unless ( scalar(@$rowRef) >= 1 ) {

      #Now calculate the protein interaction information
      my $dasa = calculateInteractionASA( $chain1, $chain2 );
      $logger->debug("The deltaASA is $dasa");
      $db->addQualityControl( $ppi->ppi, "ppi", "NACCESS", $dasa, "deltaASA" );
      
    }

    #TODO - Get NoxClass Fix this......
    #my $noxClass = _runNoxClass($chain1, $chain2, $pdbId);
  }

}

sub proteinNucleicInts {
  my ( $chain1, $chain2, $db, $pdbRdbObj ) = @_;
  $logger->debug("Calculating protein Nucleic acid interactions");

  #Check that we have to chains of the right type!
  unless ( $chain1->type eq "protein"
    and ( $chain2->type eq "DNA" or $chain2->type eq "RNA" ) )
  {
    $logger->fatal(
      "expected protein chain (first) and a nucleic acid chain (second)");
  }

  my $pdbId = $pdbRdbObj->pdb_id;

  #Add the chains to the database if not already present
  my $ch1Obj = $db->getChainData( $chain1, $pdbId );
  my $ch2Obj = $db->getChainData( $chain2, $pdbId );
  $logger->debug( "chain queries are succesful ");
  $logger->debug( "the accession called for teh get Protein Nucleic data are ". $ch1Obj->get_column( "accession") , $ch2Obj->get_column( "accession") );
  
  my ( %napiAtomStore, %napiBondStore );
  my $atomsRef = $db->getProteinNucleicAtomAndBondData( $ch1Obj->get_column( "accession") , $ch2Obj->get_column( "accession") );
  $logger->debug( "finsihed getting protein nuclcie data ");  
  
  foreach my $bondObj (@$atomsRef) {
    
    $napiAtomStore{ $bondObj->get_column('protein_acc') . ":" . $bondObj->get_column('protein_atom_number') } = $bondObj->get_column('protein_atom');
    $napiAtomStore{ $bondObj->get_column('na_acc') . ":". $bondObj->get_column('na_atom_number') } = $bondObj->get_column('na_atom');
    $napiBondStore{ $bondObj->get_column('protein_atom') . ":". $bondObj->get_column('na_atom') . ":". $bondObj->get_column('bond_type') } = 1;
    
  }
  my ( $foundInteraction, $napi, %napiResStored );
  foreach my $aa ( @{ $chain1->monomers } ) {
    my $ca = $aa->get_primary_atom;
    $logger->debug("Got primary $ca");
  AA:
    foreach my $base ( @{ $chain2->monomers } ) {
      my $bp = $base->get_primary_atom;

      #See if these amino acids look close
      next AA if ( $ca->distance($bp) > 20 );
      my $addToInterface = 0;

      #If we get here, these look close
      foreach my $aa_atom ( @{ $aa->atoms } ) {
      ATOM:
        foreach my $base_atom ( @{ $base->atoms } ) {
          my $distance = $aa_atom->distance($base_atom);
          next ATOM if ( $distance > 6 );

          #The two atoms are cloes enough to form a bond.
          my $bond = &_bondType( $aa, $base, $aa_atom, $base_atom, $distance,$carDataRef, $db );
          
          if ( $bond && $bond ne "no_bond" ) {
            $logger->debug("Got bond, $bond");

            unless ( $napiAtomStore{ $ch1Obj->get_column( "accession" ) . ":" . $aa_atom->serial } ) {
             
              my $a1 = $db->addProteinAtomData( $ch1Obj->get_column( "accession" ), $ch1Obj->id, $aa, $aa_atom );
              $napiAtomStore{ $ch1Obj->get_column( "accession" ) . ":" . $aa_atom->serial } = $a1->atom_acc;
              
            }

            unless ( $napiAtomStore{ $ch2Obj->get_column( "accession" ) . ":" . $base_atom->serial } ) {
              
              my $a2 = $db->addNucleicAcidAtomData( $ch2Obj->get_column( "accession" ), $ch2Obj->id, $base->resName, $base->resSeq, $base_atom->realName, $base_atom->serial );
              $napiAtomStore{ $ch2Obj->get_column( "accession" ) . ":" . $base_atom->serial } = $a2->atom_acc;
              $logger->debug( "the atom_acc added to the table is ".$a2->atom_acc);
                            
            }
             
            $logger->debug("**********teh accession and serial for protein are ".$ch1Obj->get_column( "accession" ) . ":" . $aa_atom->serial );
            $logger->debug( "*********the accession and serial for nucleic are ".$ch2Obj->get_column( "accession" ) . ":" . $base_atom->serial );
            
            my $a1_acc = $napiAtomStore{ $ch1Obj->get_column( "accession" ) . ":" . $aa_atom->serial };
            my $a2_acc = $napiAtomStore{ $ch2Obj->get_column( "accession" ) . ":" . $base_atom->serial };
            
            $logger->debug( "a1acc $a1_acc | a2_acc $a2_acc " );
            
            unless ( $napiBondStore{ $a1_acc . ":" . $a2_acc . ":" . $bond } ) {
              
              $db->addProteinNucleicAcidBond( $a1_acc, $a2_acc, $bond, $distance );
              $napiBondStore{ $a1_acc . ":" . $a2_acc . ":" . $bond }++;
              
            }

            unless ($napi) {

              #Add the chains, RNA/DNA first
              $napi = $db->addNapi( $ch2Obj->id, $ch1Obj->id );

              #Get a list of all residues
              my $napiRes = $db->getNapiRes( $napi->napi );

              #Build a hash of them......This stops us adding the samething!
              %napiResStored =
                map { $_->base . ":" . $_->residue . ":" . $_->bond => 1 }
                @$napiRes;
            }

            unless ( $napiResStored{ $base->resSeq . ":" . $aa->resSeq . ":" . $bond } ) {
            
              my $res = $db->addNapiRes( $napi->napi,   $ch2Obj->id, $ch1Obj->id, $base->resSeq, $aa->resSeq, $bond );
              $napiResStored{ $res->base . ":" . $res->residue . ":" . $res->bond } = 1;
            }
            
            $foundInteraction = 1;
          
          }
        }
      }
    }
  }

  #If we find an interaction calculate deltaASA and upload into the database
  if ($foundInteraction) {
    my $rowRef = $db->getQualityControl( $napi->napi, "napi", "NACCESS" );

    unless ( scalar(@$rowRef) >= 1 ) {
      my $dasa = calculateInteractionASA( $chain1, $chain2 );
      $db->addQualityControl( $napi->napi, "napi", "NACCESS", $dasa,
        "deltaASA" );
      $logger->debug("The deltaASA is $dasa");
    }
  }
}

sub proteinLigandInts {
  my ( $chain, $ligandChain, $db, $pdbRDBObj ) = @_;
  
  #my $carDataRef = &readCAR;
  my $chainObj = $db->getChainData( $chain, $pdbRDBObj->pdb_id );
  
  foreach my $ligand ( @{ $ligandChain->monomers } ) {
    
    # check whether the momomer we are dealing with is ligand or residue, if its residue, then skip it.
    next unless( ref( $ligand ) eq "Bio::iPfam::Structure::Ligand" );
    $logger->debug( "******************the dump of the ligands in proteinLigandInts is ".ref( $ligand ) );
    
    next if ( $ligand->resName eq "HOH" or $ligand->resName eq "DOD" );
    my ( $foundInteraction, $pli, %pliResStored, %pliAtomStore, %pliBondStore );
    my $ligChemObj = $db->getLigandChemistryData($ligand);
    my $ligRDBObj;
    if ($ligChemObj) {
      $ligRDBObj =
        $db->addLigand( $ligand,
        $pdbRDBObj->pdb_id . "_" . $ligandChain->internal_chain_id,
        $ligChemObj->ligand_id );
    }
    else {
      $logger->warn( "Could not find chemistry for " . $ligand->resName );
      next;
    }

    my $atomsRef =
      $db->getProteinLigandAtomAndBondData( $chainObj->accession,
      $ligRDBObj->internal_ligand_id );
    foreach my $bondObj (@$atomsRef) {
      $pliAtomStore{ $bondObj->get_column('protein_acc') . ":"
          . $bondObj->get_column('protein_atom_number') } =
        $bondObj->get_column('protein_atom');
      $pliAtomStore{ $bondObj->get_column('internal_ligand_id') . ":"
          . $bondObj->get_column('ligand_atom_number') } =
        $bondObj->get_column('ligand_atom');
      $pliBondStore{ $bondObj->get_column('protein_atom') . ":"
          . $bondObj->get_column('ligand_atom') . ":"
          . $bondObj->get_column('bond_type') } = 1;
    }

    foreach my $ligAtom ( @{ $ligand->atoms } ) {

      foreach my $aa ( @{ $chain->monomers } ) {
        my $ca = $aa->get_primary_atom;

        #See if this amino acid look closse
        next if ( $ligAtom->distance($ca) > 20 );

        #If we get here, these look close
        foreach my $aaAtom ( @{ $aa->atoms } ) {
          my $distance = $ligAtom->distance($aaAtom);
          next if ( $distance > 6 );

          #The two atoms are cloes enough to form a bond.
          my $bond =
            &_bondType( $ligand, $aa, $ligAtom, $aaAtom, $distance, $carDataRef,
            $db );
          if ( $bond && $bond ne "no_bond" ) {
            $logger->debug(
              $ligand->resName . "," . $ligand->hetID . "|$bond" );
            unless (
              $pliAtomStore{ $chainObj->accession . ":" . $aaAtom->serial } )
            {
              my $a1 =
                $db->addProteinAtomData( $chainObj->accession, $chainObj->id,
                $aa, $aaAtom );
              $pliAtomStore{ $chainObj->accession . ":" . $aaAtom->serial } =
                $a1->atom_acc;
            }
            unless (
              $pliAtomStore{ $ligRDBObj->internal_ligand_id . ":"
                  . $ligAtom->serial } )
            {
              my $a2 = $db->addLigandAtomData( $ligRDBObj->internal_ligand_id,
                $ligand, $ligAtom );
              $pliAtomStore{ $ligRDBObj->internal_ligand_id . ":"
                  . $ligAtom->serial } = $a2->atom_acc;
            }

            my $a1_acc =
              $pliAtomStore{ $chainObj->accession . ":" . $aaAtom->serial };
            my $a2_acc = $pliAtomStore{ $ligRDBObj->internal_ligand_id . ":"
                . $ligAtom->serial };
            unless ( $pliBondStore{ $a1_acc . ":" . $a2_acc . ":" . $bond } ) {
              $db->addProteinLigandBond( $a1_acc, $a2_acc, $bond, $distance );
              $pliBondStore{ $a1_acc . ":" . $a2_acc . ":" . $bond }++;
            }

            unless ($pli) {

              #Add the chains, ligand second
              $pli = $db->addPli( $chainObj->id, $ligRDBObj->id );

              #Get a list of all residues
              my $pliRes = $db->getPliRes( $pli->pli );

              #Build a hash of them......This stops us adding the samething!
              #TODO - Change to ligand_residue and protein_residue
              %pliResStored = map {
                    $_->residue_a . ":"
                  . $_->residue_b . ":"
                  . $_->bond => 1
              } @$pliRes;
            }

            unless (
              $pliResStored{ $aa->resSeq . ":"
                  . $ligand->resSeq . ":"
                  . $bond } )
            {
              my $res =
                $db->addPliRes( $pli->pli, $chainObj->id, $ligRDBObj->id,
                $aa->resSeq, $ligand->resSeq, $bond );
              $pliResStored{ $res->residue_a . ":"
                  . $res->residue_b . ":"
                  . $res->bond } = 1;
            }

            $foundInteraction = 1;

          }
        }
      }
    }

    #If we find an interaction calculate deltaASA
    if ($foundInteraction) {
      my $rowRef = $db->getQualityControl( $pli->pli, "pli", "NACCESS" );

      unless ( scalar(@$rowRef) >= 1 ) {

        #Add the data to the pli table
        my $dasa = calculateLigandInteractionASA( $chain, undef, $ligandChain );
        $db->addQualityControl( $pli->pli, "pli", "NACCESS", $dasa,
          "deltaASA" );
        $logger->debug("The deltaASA is $dasa");
      }
    }
  }
}

sub calIntraDomDomInts {
  my ( $pdbObj, $db ) = @_;

  foreach my $chain ( @{ $pdbObj->chains } ) {
    
    next unless ( $chain->type eq "protein" );
    
    my $chainObj = $db->getChainData( $chain, $pdbObj->id );
    $logger->debug("Got chain");
    
    if ( $chain->pfam_chain ) {
      $logger->debug("Got pfam chain");
      
      if ( scalar( @{ $chain->pfam_chain->regions } ) > 1 ) {
        $pdbObj->log->debug("Got pfam region");
        
        foreach my $reg1 ( @{ $chain->pfam_chain->regions } ) {
    
          foreach my $reg2 ( @{ $chain->pfam_chain->regions } ) {
            
            $logger->debug( "the uniquID of region1 and region2 are ". $reg1->uniqueID ." - ". $reg2->uniqueID );
                    
            next if ( $reg1->uniqueID eq $reg2->uniqueID );
            my ( $foundInteraction, $ddi, %ddiResStored, %ddiAtomStore, %ddiBondStore );
            
            my $atomsRef = $db->getProteinAtomAndBondData( $chainObj->accession, $chainObj->accession, 1 );
            
            foreach my $bondObj (@$atomsRef) {
              
              $ddiAtomStore{ $bondObj->get_column('protein_acc_a') . ":". $bondObj->get_column('atom_number_a') } = $bondObj->get_column('atom_a');
              $ddiAtomStore{ $bondObj->get_column('protein_acc_b') . ":". $bondObj->get_column('atom_number_b') } = $bondObj->get_column('atom_b');
              $ddiBondStore{ $bondObj->get_column('atom_a') . ":". $bondObj->get_column('atom_b') . ":". $bondObj->get_column('bond_type') } = 1;
            
            } # end of @£atomsRef

            $logger->debug( "Going to calulate domain interactions between ". $reg1->acc . " and ". $reg2->acc );
            
            foreach my $aa1 ( @{ $chain->monomers } ) {
              next if ( $aa1->resSeq < $reg1->start or $aa1->resSeq > $reg1->end );
              
              $logger->debug( "Residue1" . $aa1->resSeq );
              my $ca1 = $aa1->get_primary_atom;
              
            AA:
              foreach my $aa2 ( @{ $chain->monomers } ) {
                
                next if ( $aa2->resSeq < $reg2->start or $aa2->resSeq > $reg2->end );
                
                $logger->debug( "Residue2 " . $aa2->resSeq );
                my $ca2 = $aa2->get_primary_atom;

                #See if these amino acids look close
                next AA if ( $ca1->distance($ca2) > 20 );
                my $addToInterface = 0;

                #If we get here, these look close
                foreach my $aa2_atom ( @{ $aa2->atoms } ) {
                
                ATOM:
                  foreach my $aa1_atom ( @{ $aa1->atoms } ) {
                    
                    my $distance = $aa1_atom->distance($aa2_atom);
                    next ATOM if ( $distance > 6 );

                    #The two atoms are cloes enough to form a bond.
                    $logger->debug("$distance");
                    
                    my $bond = &_bondType( $aa1, $aa2, $aa1_atom, $aa2_atom, $distance, $carDataRef, $db );
                    $logger->debug("Got bond, $bond");
                    
                    if ( $bond && $bond ne "no_bond" ) {
                      
                      $logger->debug("Got bond, $bond");
                      my ( $a1_acc, $a2_acc );

                      unless ( $ddiAtomStore{ $chainObj->accession . ":" . $aa1_atom->serial } ) {
                        my $a1 = $db->addProteinAtomData( $chainObj->accession, $chainObj->id, $aa1, $aa1_atom );
                        $ddiAtomStore{ $chainObj->accession . ":". $aa1_atom->serial } = $a1->atom_acc;
                      } # en dof ddiAtomStore
                      
                      $a1_acc = $ddiAtomStore{ $chainObj->accession . ":". $aa1_atom->serial };

                      unless ( $ddiAtomStore{ $chainObj->accession . ":". $aa2_atom->serial } ) {
                        my $a2 = $db->addProteinAtomData( $chainObj->accession, $chainObj->id, $aa2, $aa2_atom );
                        $ddiAtomStore{ $chainObj->accession . ":". $aa2_atom->serial } = $a2->atom_acc;
                      } # end of ddiAtomStore
                      
                      $a2_acc = $ddiAtomStore{ $chainObj->accession . ":". $aa2_atom->serial };

                      unless ( $ddiBondStore{ $a1_acc . ":" . $a2_acc . ":" . $bond } ) {
                        $db->addProteinProteinBond( $a1_acc, $a2_acc, $bond, $distance, 1 );
                        $ddiBondStore{ $a1_acc . ":" . $a2_acc . ":". $bond }++;
                      
                      } # end of ddiBondStore;

                      unless ($ddi) {

                        #Add the chains, ligand second
                        $ddi = $db->addDdi( $reg1->uniqueID, $reg2->uniqueID, 1, $bond );

                        #Get a list of all residues
                        my $ddiRes = $db->getDdiRes( $ddi->ddi );

                        # Build a hash of them......This stops us adding the samething!
                        %ddiResStored = map {
                              $_->residue_a . ":"
                            . $_->residue_b . ":"
                            . $_->bond => 1
                        } @$ddiRes;
                      } # end of ddi;
                   
                      unless ( $ddiResStored{ $aa1->resSeq . ":" . $aa2->resSeq . ":" . $bond } ) {
                        my $res = $db->addDdiRes( $ddi->ddi, $chainObj->id, $chainObj->id, $aa1->resSeq, $aa2->resSeq, 1, $bond );

                        # $db->addDdi($reg1->uniqueID, $reg2->uniqueID, $aa1->resSeq, $aa2->resSeq, 1, $bond);
                        $ddiResStored{ $res->residue_a . ":". $res->residue_b . ":". $res->bond } = 1;
                        
                      } # end of $ddiResStored
                      $foundInteraction = 1;
                      
                    } # end of  if( $bond && $bond new 'nobond' )
                    
                  } # end of( @{ $aa1->atoms } )
    
                } # end of @{ $aa2->atoms }
    
              } # end of $aa2 @{ $chain->monomers }
    
            } # end of $aa1 @{ $chain->monomers }
            
            if ($foundInteraction) {
              my $rowRef = $db->getQualityControl( $ddi->ddi, "ddi", "NACCESS" );
               
               unless ( scalar(@$rowRef) >= 1 ) {
 #                my $dasa = calculateDomainInteractionASA( $chain, $reg1, $chain, $reg2 );
                 my $dasa = calculateDomainInteractionASA( $chain, $reg1, $chain, $reg2 , 770 );  
                 $db->addQualityControl( $ddi->ddi, "ddi", "NACCESS", $dasa, "deltaASA" );
                 
               }
               
            } # end of if( $foundInteraction )
          
          } # $reg2 ( @{ $chain->pfam_chain->regions } )
    
        } # end of $reg1 ( @{ $chain->pfam_chain->regions } )
    
      }# end of scalar( @{ $chain->pfam_chain->regions } ) > 1 )
    
    } # end of $chain->pfam_chain
    
  } # end of $chain ( @{ $pdbObj->chains }

}

sub deriveInterDomInts {
  my ( $pdbObj, $db ) = @_;

  $logger->debug( "inside deriveINerDOnInts");
  foreach my $chain1 ( @{ $pdbObj->chains } ) {
    $logger->debug("Got chain1");
    foreach my $chain2 ( @{ $pdbObj->chains } ) {
      $logger->debug("Got chain2");

      # Skip self-self interactions
      next if ( $chain1->internal_chain_id eq $chain2->internal_chain_id );

      #If both chains have pfam_chains they must proteins
      if ( $chain1->pfam_chain and $chain2->pfam_chain ) {
        $logger->debug(
          "Got pfam chains on both PDB, look to see if they interact");

        #Go through each domain and look for interactions
        foreach my $reg1 ( @{ $chain1->pfam_chain->regions } ) {
          foreach my $reg2 ( @{ $chain2->pfam_chain->regions } ) {
            $logger->debug( "looking for inteactions between"
                . $reg1->uniqueID . " and "
                . $reg2->uniqueID );

#We should have already calcualted the interactions, so lets get the from the database
            my $interface = $db->getPpiResWithRange(
              $pdbObj->id . "_" . $chain1->internal_chain_id,
              $pdbObj->id . "_" . $chain2->internal_chain_id,
              $reg1->start,
              $reg1->end,
              $reg2->start,
              $reg2->end
            );

            #If there are interactions, lets add them back
            if ( ref($interface) eq "ARRAY" ) {

              my ( $ddi, %ddiResStored );
              foreach my $int ( @{$interface} ) {
                $logger->debug("Got interface from the database");
                unless ($ddi) {

                  #Find or create the domain-domain interactions
                  $ddi =
                    $db->addDdi( $reg1->uniqueID, $reg2->uniqueID, 0,
                    $int->bond );

    #Get a list of all residues already calculated for this interaction (if any)
                  my $ddiRes = $db->getDdiRes( $ddi->ddi );

                  #Build a hash of them......This stops us adding the samething!
                  %ddiResStored = map {
                        $_->residue_a . ":"
                      . $_->residue_b . ":"
                      . $_->bond => 1
                  } @$ddiRes;
                }

   #If we have not got the domain-domain interacting residues already, add them.
                unless (
                  $ddiResStored{
                    $int->residue_a . ":" . $int->residue_b . ":" . $int->bond
                  }
                  )
                {
                  $db->addDdiRes( $ddi->ddi, $reg1->uniqueID, $reg2->uniqueID,
                    $int->residue_a, $int->residue_b, 0, $int->bond );
                  $ddiResStored{ $int->residue_a . ":"
                      . $int->residue_b . ":"
                      . $int->bond }++;
                }
              }
              my $rowRef =
                $db->getQualityControl( $ddi->ddi, "ddi", "NACCESS" );

              unless ( scalar(@$rowRef) >= 1 ) {
                my $dasa = calculateDomainInteractionASA( $chain1, $reg1, $chain2,$reg2 );
                $db->addQualityControl( $ddi->ddi, "ddi", "NACCESS", $dasa,"deltaASA" );
              }
            }
          }
        }
      }    #look for pfam domains and nucleic acid interactions
      elsif ( $chain1->pfam_chain
        and ( $chain2->type eq "RNA" or $chain2->type eq "DNA" ) )
      {
        foreach my $reg ( @{ $chain1->pfam_chain->regions } ) {
          $logger->debug(
            "Getting DNA/RNA interaction information with " . $reg->acc );

          $logger->debug(
            "Got pfam chains on both PDB, look to see if they interact");
          foreach my $reg1 ( @{ $chain1->pfam_chain->regions } ) {
            $logger->debug( "looking for inteactions between "
                . $reg1->uniqueID . " and "
                . $chain2->internal_chain_id );
            my $interface = $db->getNapiResWithProteinRange(
              $pdbObj->id . "_" . $chain2->internal_chain_id,
              $pdbObj->id . "_" . $chain1->internal_chain_id,
              $reg1->start, $reg1->end
            );
            if ( ref($interface) eq "ARRAY" ) {
              my ( $nadi, %nadiResStored );
              
              foreach my $int ( @{$interface} ) {
                unless ($nadi) {

                  $nadi = $db->addNadi(  $pdbObj->id . "_" . $chain2->internal_chain_id, $reg1->uniqueID );
                  #Get a list of all residues
                  my $nadiRes = $db->getNadiRes( $nadi->nadi );

                  #Build a hash of them......This stops us adding the same thing twice!
                  %nadiResStored = map {
                        $_->base . ":"
                      . $_->region_residue . ":"
                      . $_->bond => 1
                  } @$nadiRes;
       
                }
                $logger->debug("Got interface");
                unless ( $nadiResStored{ $int->base . ":" . $int->residue . ":" . $int->bond } ) {
                 
                  $db->addNadiRes( $nadi->nadi, $int->nucleic_acid_acc, $reg1->uniqueID, $int->base, $int->residue, $int->bond );
                  $nadiResStored{ $int->base . ":" . $int->residue . ":" . $int->bond }++;
                  
                }
              }
              my $rowRef = $db->getQualityControl( $nadi->nadi, "nadii", "NACCESS" );

              unless ( scalar(@$rowRef) >= 1 ) {

                #Calculate deltaASA
                my $dasa = calculateDomainInteractionASA( $chain1, $reg1, $chain2 );
                $db->addQualityControl( $nadi->nadi, "nadi", "NACCESS", $dasa, "deltaASA" );
                
              }
            }
          }
        }
      }    #Calculate domain ligand interactions
      elsif ( $chain1->pfam_chain and $chain2->type eq "ligand" ) {
        foreach my $ligand ( @{ $chain2->monomers } ) {
          
          # check whether the momomer we are dealing with is ligand or residue, if its residue, then skip it.
          next unless( ref( $ligand ) eq "Bio::iPfam::Structure::Ligand" );
          $logger->debug( "******************the dump of the ligands in deriveInterDomInts is ".ref( $ligand ) );
         
          next if ( $ligand->resName eq "HOH" or $ligand->resName eq "DOD" );
          
          my $ligChemObj = $db->getLigandChemistryData($ligand);
          my $ligRDBObj;
          if ($ligChemObj) {
            $ligRDBObj =
              $db->addLigand( $ligand,
              $pdbObj->id . "_" . $chain2->internal_chain_id,
              $ligChemObj->ligand_id );
          }
          else {
            $logger->warn( "Could not find chemistry for " . $ligand->resName );
            next;
          }
          foreach my $reg ( @{ $chain1->pfam_chain->regions } ) {
            $logger->debug( "Getting ligand interaction information with "
                . $reg->acc . " and "
                . $ligand->resName );
            foreach my $reg1 ( @{ $chain1->pfam_chain->regions } ) {
              my $interface = $db->getPliResWithProteinRange(
                $ligRDBObj->internal_ligand_id,
                $pdbObj->id . "_" . $chain1->internal_chain_id,
                $reg1->start, $reg1->end
              );
              if ( ref($interface) eq "ARRAY" ) {
                my ( $dli, %dliResStored );
                foreach my $int ( @{$interface} ) {
                  unless ($dli) {
                    $dli =
                      $db->addDli(
                      #TODO - Fix
                      $reg1->uniqueID,
                      $ligRDBObj->internal_ligand_id );

                    #Get a list of all residues
                    my $dliRes = $db->getDliRes( $dli->dli );

           #Build a hash of them......This stops us adding the same thing twice!
                    %dliResStored = map {
                      $_->region_residue . ":"
                        . $_->ligand_residue . ":"
                        . $_->bond => 1
                    } @$dliRes;
                  }
                  $logger->debug("Got interface");
                  unless (
                    $dliResStored{
                      $int->residue_a . ":" . $int->residue_b . ":" . $int->bond
                    }
                    )
                  {
                    $db->addDliRes(
                      $dli->dli,
                      $reg1->uniqueID,
                      $ligRDBObj->internal_ligand_id,
                      $int->residue_a,
                      $int->residue_b,
                      $int->bond
                    );
                    $dliResStored{ $int->residue_a . ":"
                        . $int->residue_b . ":"
                        . $int->bond }++;
                  }
                }
                my $rowRef =
                  $db->getQualityControl( $dli->dli, "dli", "NACCESS" );

                unless ( scalar(@$rowRef) >= 1 ) {

                  #Calculate deltaASA
                  my $dasa =
                    calculateLigandInteractionASA( $chain1, $reg1, $ligand );
                  $db->addQualityControl( $dli->dli, "dli", "NACCESS", $dasa,
                    "deltaASA" );
                }
              }
            }
          }
        }
      }
    }
  }
  $logger->info( "********************completed deriveINerDOnInts");
}

=head2 getPdb



=cut

sub getPdb {
  my ($pdbId) = shift;

  my $factory = Bio::iPfam::Structure::PDBFactory->new;
  $logger->debug("Going to get $pdbId via a sockect connection");
  my $pfetch = new Pfetch;
  my $pdbFile = $pfetch->retrieve( { "--new_pdb" => $pdbId } );
  $logger->debug( "Got the following from pfetch |" . $pdbFile . "|" );
  my $pdbObj;
  eval { $pdbObj = $factory->parse($pdbFile); };

  if ($@) {
    $logger->warn("Error getting $pdbId: $@");
    return undef;
  }
  else {

    #Now set the pdbId so we know where the chainset has come from!
    $pdbObj->id($pdbId);
    return $pdbObj;
  }
}

=head2 _bondType



=cut

sub _bondType {
  my ( $r1, $r2, $atom1, $atom2, $distance, $carDataRef, $db ) = @_;

  my $at1 = $atom1->realName;
  my $at2 = $atom2->realName;
  my ( $pat1, $pat2, $pr1, $pr2, $bond );
  my $slopfactor = 1;
  $logger->debug( "|" . $r1->resName . ",$at1/" . $r2->resName . ",$at2| $distance" );

  #Get rid of any whitespace around the atom

  #process the residue/monomer and atoms if they are not found in the hash
  if ( !keys( %{ $$carDataRef{ $r1->resName }{$at1} } ) ) {
    $pr1  = $atom1->element;
    $pat1 = $atom1->element;
  }
  else {
    $pr1  = $r1->resName;
    $pat1 = $at1;
  }

  if ( !keys( %{ $$carDataRef{ $r2->resName }{$at2} } ) ) {
    $pr2  = $atom2->element;
    $pat2 = $atom2->element;
  }
  else {
    $pr2  = $r2->resName;
    $pat2 = $at2;
  }

  #Now, lets try and work out the bonds
  $logger->debug( "Have not work out what residue " . $r1->type . " is" ) unless ($pr1);
  $logger->debug("Have not work out what atom $at1 is") unless ($pat1);
  $logger->debug( "Have not work out what " . $r2->type . " is" ) unless ($pr2);
  $logger->debug("Have not work out what $at1 is") unless ($pat2);

  if ( $pr1 && $pr2 && $pat1 && $pat2 ) {
    $logger->debug( "Going to estimate the bond with the following residues/atoms info |$pr1, $pr2, $pat1, $pat2|" );
    
    #Check that the process res/mono and atoms are in the hash
    if ( ( !keys( %{ $$carDataRef{$pr2}{$pat2} } ) ) || ( !keys( %{ $$carDataRef{$pr1}{$pat1} } ) ) ) {
      $logger->warn("Undefined bond for $r1, $at1 and $r2, $at2 as one of them is not in the charge & radii hash" );
      $bond = undef;
    }
    else {

      if ( $distance < ( $$carDataRef{$pr1}{$pat1}{'cr'} + $$carDataRef{$pr2}{$pat2}{'cr'} ) ) {
        if ( $pat1 eq "SG" && $pat2 eq "SG" ) {
          $bond = "disulphide";
        }
        else {
          $bond = "covalent";
        }
      }
      elsif ( ( $distance < ( $slopfactor * ( $$carDataRef{$pr1}{$pat1}{'vr'} + $$carDataRef{$pr2}{$pat2}{'vr'} ) ) )
        && ( ( ( $$carDataRef{$pr1}{$pat1}{'Charge'} eq "+" ) && ( $$carDataRef{$pr2}{$pat2}{'Charge'} eq "-" ) ) || ( ( $$carDataRef{$pr1}{$pat1}{'Charge'} eq "-" ) 
        && ( $$carDataRef{$pr2}{$pat2}{'Charge'} eq "+" ) ) ) ) {
        
        $bond = "electrostatic";
      }
      elsif ( ( ( $distance < 3.6 ) && ( $distance > ( 1 + $$carDataRef{$pr1}{$pat1}{'cr'} + $$carDataRef{$pr2}{$pat2}{'cr'} )) ) 
        && ( ( ( $$carDataRef{$pr1}{$pat1}{'Hb'} eq "D" ) && ( $$carDataRef{$pr2}{$pat2}{'Hb'} eq "A" ) ) || ( ( $$carDataRef{$pr1}{$pat1}{'Hb'} eq "A" )
        && ( $$carDataRef{$pr2}{$pat2}{'Hb'} eq "D" ) ) ) )
      {
        $logger->debug( "the distance btw atoms is less than 3.6, so gettign the connectivity for ".$r1->resName.' | '.$atom1->realName );
         
        unless ( $connectivityRef->{ $r1->resName }->{ $atom1->realName } ) {
          my $cons = $db->getConnectivity( $r1->resName, $atom1->realName );
          
          if ($cons) {
            
            foreach my $c (@$cons) {
              push(
                @{ $connectivityRef->{ $r1->resName }->{ $atom1->realName } },
                $c->atom2_name
              );
              $logger->debug( "Connections " . $c->atom1_name . "," . $c->atom2_name );
            }
          }
          else {
            $logger->warn( "Failed to get connectivity for ". $r1->resName . "-" . $atom1->realName );
          }
        }  # end of $connectivityRef->{ $r1->resName  }
        
        unless ( $connectivityRef->{ $r2->resName }->{ $atom2->realName } ) {
          my $cons = $db->getConnectivity( $r2->resName, $atom2->realName );
          if ($cons) {
            foreach my $c (@$cons) {
              push(
                @{ $connectivityRef->{ $r2->resName }->{ $atom2->realName } },
                $c->atom2_name
              );
              $logger->debug( "Connections " . $c->atom1_name . "," . $c->atom2_name );
            }
          }
          else {
            $logger->warn( "Failed to get connectivity for " . $r2->resName . "-" . $atom2->realName );
          }
        }
        
        my ( $bond_angle, $bond_plane );
        #$logger->info( "************** bond_angle sub called with the params ".$atom1->realName.' | '. $$carDataRef{$pr1}{$pat1}{'Hb'}.' | '.$atom2->realName.' | '.$$carDataRef{$pr2}{$pat2}{'Hb'}.' | '.$r1->resName.' | '.$r2->resName.' | '."\n and we got angle $bond_angle" );
        # delete everything from this line till # EOF
        unless ( defined $atom1->realName ){ $logger->debug( "********atom1 not defined" ); }
        unless ( defined $$carDataRef{$pr1}{$pat1}{'Hb'} ){ $logger->debug( "*******$pr1, $pat1, hb not defined" ); }
        unless ( defined $atom2->realName ){ $logger->debug( "*********atom2 not defined" ); }
        unless ( defined $$carDataRef{$pr2}{$pat2}{'Hb'} ){ $logger->debug( "********$pr2, $pat2, hb not defined" ); }
        unless ( defined $r1->resName ){ $logger->debug( "**********Res1 not defined" ); }
        unless ( defined $r2->resName ){ $logger->debug( "*******Res2 not defined" ); }
        
        # EOF 
        eval{
          ( $bond_angle, $bond_plane ) = bond_angle( $atom1, $$carDataRef{$pr1}{$pat1}{'Hb'}, $atom2, $$carDataRef{$pr2}{$pat2}{'Hb'},$r1, $r2, $db ); 
        };
        
        if( $@ ){
          $logger->debug( "************** bond_angle sub called with the params ".$atom1->realName.' | '. $$carDataRef{$pr1}{$pat1}{'Hb'}.' | '.$atom2->realName.' | '.$$carDataRef{$pr2}{$pat2}{'Hb'}.' | '.$r1->resName.' | '.$r2->resName.' | '."\n and we got angle $bond_angle" );
          $logger->logdie( "there is some error in finding the bond angle ".$@ );
        }
         
        if ( $bond_angle > 90 && $bond_angle < 140 ) {

          #print STDERR "looks like we have a H-bond between $r1, $at1 and $pr2, $pat2, Check regex\n";
          if ( ( $r1 =~ /\S{3}/ )
            && ( ( $pat1 eq "O" ) || ( $pat1 eq "N" || ( $pat1 eq "OXT" ) ) ) )
          {
            $bond = "h-bond_back";
          }
          elsif ( ( $r2 =~ /\S{3}/ )
            && ( ( $pat2 eq "O" ) || ( $pat2 eq "N" || ( $pat2 eq "OXT" ) ) ) )
          {
            $bond = "h-bond_back";
          }
          else {
            $bond = "h-bond_side";
          }
        } # end of if( $bond_angle )
        else {
          $bond = "vanderwaals";
        }
        
      }
      elsif ( ( $distance < ( $slopfactor * 1.5 + $$carDataRef{$pr1}{$pat1}{'vr'} + $$carDataRef{$pr2}{$pat2}{'vr'} ) )
        && ( $distance > $$carDataRef{$pr1}{$pat1}{'vr'} + $$carDataRef{$pr2}{$pat2}{'vr'} ) )
      {
        $bond = "vanderwaals";
      }
      else {
        $bond = "no_bond";
      }
    }
  }
  else {

    #Okay, processing resulted in undefined values
    $bond = undef;
  }
  
  return $bond;
}

sub bond_angle {
  $logger->debug("in bond angle");
  
  # atom1, donor/acceptor, atom2, donor/acceptor, residue1, residue2
  my ( $a1, $h1, $a2, $h2, $r1, $r2, $db ) = @_;
  
  #vector1 A->D
  my ( $v1, $v2, $bond_plane_angle, $bond_res_plane );

  #vector2 A->C
  if ( $h1 eq "A" ) {
    $logger->debug("Getting carbon for accptor (h1)");
    my $c = getConnection( $r1, $a1, $db );
    $logger->debug("Calculating vector a1, c");
    $v2 = $a1->cal_vector($c);
    $logger->debug("Calculating vector a1, a2");
    $v1 = $a1->cal_vector($a2);
    
  }
  elsif ( $h2 eq "A" ) {
    $logger->debug("Getting carbon for accptor (h2)");
    my $c = getConnection( $r2, $a2, $db );
    $logger->debug("Calculating vector a1, c");
    $v2 = $a2->cal_vector($c);
    $logger->debug("Calculating vector a1, a2");
    $v1 = $a2->cal_vector($a1);
    
  }
  else {
    $logger->debug("no donor");
  }
  
  
  if ( $v1 and $v2 ) {
    
    # Then need to find the angle between these two vectors.
    # Uses the dot product equation a.b = |a||b| cos THETA
    #|a| -> sqroot of X2 + Y2 + Z2
    my $mod_v1 = &mod_vector($v1);
    my $mod_v2 = &mod_vector($v2);

    #a.b -> X x X + Y x Y + Z X Z
    my $vp = vector_product( $v1, $v2 );
    
    # IN SOME CASES, I AM GETTING THE VALUE 0 FOR X, Y AND Z, SO MAKE SURE WE DO CALCULATION FOR NON-ZERO VALUES;
    my $angle;
    if( $mod_v1 > 0 and $mod_v2 > 0 ){
      $angle = rad2deg( acos( $vp / ( $mod_v1 * $mod_v2 ) ) );
    }else{
      $logger->debug( "********one of the mod values is 0, so skipped the calculation");
      $angle = "0";
    }
    
    return ($angle);
  }
  else {
    $logger->warn("no angle determined ");
    return ("0");
  }
}

sub getConnection {
  my ( $res, $atom, $db ) = @_;

  unless ( $connectivityRef->{ $res->resName }->{ $atom->realName } ) {
    my $cons = $db->getConnectivity( $res->resName, $atom->realName );
    if ($cons) {
      foreach my $r ( @{$cons} ) {
        push(
          @{ $connectivityRef->{ $res->resName }->{ $atom->realName } },
          $r->atom2_name
        );
      }
    }
  }

  my $conAtom;
  if ( $connectivityRef->{ $res->resName }->{ $atom->realName }
    and scalar( @{ $connectivityRef->{ $res->resName }->{ $atom->realName } } )
    == 1 )
  {
    $conAtom =
      $res->get_atom(
      $connectivityRef->{ $res->resName }->{ $atom->realName }->[0] );
  }
  else {
    foreach my $atomName (
      @{ $connectivityRef->{ $res->resName }->{ $atom->realName } } )
    {
      if ( $atomName =~ /C/ ) {
        $conAtom = $res->get_atom($atomName);
        last;
      }
    }
    if ( !$conAtom ) {
      foreach my $atomName (
        @{ $connectivityRef->{ $res->resName }->{ $atom->realName } } )
      {
        if ( $atomName =~ /O/ ) {
          $conAtom = $res->get_atom($atomName);
          last;
        }
      }
    }

#$conAtom = $res->get_atom($connectivityRef->{$res->resName}->{$atom->realName}->[0]);
  }
  if ($conAtom) {
    $logger->debug( "Returning connection $conAtom for " . $res->resName );
    $logger->debug( "Returning connection $conAtom for " . $res->resName );
    return ($conAtom);
  }
}

sub mod_vector {

  #|a| -> sqroot of X2 + Y2 + Z2
  my ($v) = @_;
  my $mod_v;
  foreach ( keys %$v ) {
    $mod_v += $$v{$_} * $$v{$_};
  }
  return ( sqrt($mod_v) );
}

sub vector_product {
  my ( $v1, $v2 ) = @_;
  my $prod;
  foreach ( keys %{$v1} ) {
    $prod += $$v1{$_} * $$v2{$_};
  }
  return ($prod);
}

=head2 calculateInteractionASA

  Title   : calculateInteractionASA
  Usage   : iPfam::calculateInteractionASA($domain1, $domain2)
  Function: Calculates the available solvent accessibility of the two chain objects and of the two combined.
          : Third party software naccess is used ti calulate solvent accessibility.
          : This will make is possible ti calulate the change in ASA upon interaction formation.  Usually
          : a threshold of 200A is used.
  Args    : Two Bio::Pfam::Structure::Chain objects
  returns : Arrayref containing the naccess for domain1 (element 0), domain2 (element 1) and combined (element 2) 

=cut

sub calculateInteractionASA {
  my ( $obj1, $obj2 , $line) = @_;

  #Write out each object as a pdb file
  #domain1
  open( OBJ1, ">entity1_$$.pdb" ) || die "Could not open entity1.$$.pdb:[$!]\n";
  $obj1->write( \*OBJ1 );
  close(OBJ1);

  #nucleic acid/domain2
  open( OBJ2, ">entity2_$$.pdb" ) || die "Could not open entity2.$$.pdb:[$!]\n";
  $obj2->write( \*OBJ2 );
  close(OBJ2);

  #combined
  open( OBJALL, ">entityAll_$$.pdb" )
    || die "Could not open entityAll.$$.pdb:[$!]\n";
  $obj1->write( \*OBJALL );
  $obj2->write( \*OBJALL );
  close(OBJALL);

#Now calculate the ASA for each pdb file. Extension left off as the naccess litters
#the cwd with loads of temporary files.

  my @asa;
  my $i = 0;
  foreach my $file ( "entity1_$$", "entity2_$$", "entityAll_$$" ) {
    my $data = _runAndParseNaccess($file);
    $asa[$i] = $data if ($data);
    $i++;
  }
  my $dasa = ( $asa[0]->{all} + $asa[1]->{all} ) - $asa[2]->{all};

  #Return the results
  return $dasa;
}

=head2 calculateInteractionASA

  Title   : calculateInteractionASA
  Usage   : iPfam::calculateInteractionASA($domain1, $domain2)
  Function: Calculates the available solvent accessibility of the two chain objects and of the two combined.
          : Third party software naccess is used ti calulate solvent accessibility.
          : This will make is possible ti calulate the change in ASA upon interaction formation.  Usually
          : a threshold of 200A is used.
  Args    : Two Bio::Pfam::Structure::Chain objects
  returns : Arrayref containing the naccess for domain1 (element 0), domain2 (element 1) and combined (element 2) 

=cut

sub calculateDomainInteractionASA {
  my ( $chain1, $reg1, $chain2, $reg2 ) = @_;

  open( OBJALL, ">entityAll_$$.pdb" )
    || die "Could not open entityAll.$$.pdb:[$!]\n";

  #Write out each object as a pdb file
  #domain1
  open( OBJ1, ">entity1_$$.pdb" ) || die "Could not open entity1.$$.pdb:[$!]\n";
  foreach my $aa1 ( @{ $chain1->monomers } ) {
    if ($reg1) {
      next if ( $aa1->resSeq < $reg1->start or $aa1->resSeq > $reg1->end );
    }
    $aa1->write( \*OBJ1 );
    $aa1->write( \*OBJALL );
  }
  close(OBJ1);

  open( OBJ2, ">entity2_$$.pdb" ) || die "Could not open entity2.$$.pdb:[$!]\n";
  foreach my $aa2 ( @{ $chain2->monomers } ) {
    if ($reg2) {
      next if ( $aa2->resSeq < $reg2->start or $aa2->resSeq > $reg2->end );
    }
    $aa2->write( \*OBJ2 );
    $aa2->write( \*OBJALL );
  }
  close(OBJ2);

  close(OBJALL);

#Now calculate the ASA for each pdb file. Extension left off as the naccess litters
#the cwd with loads of temporary files.

  my @asa;
  my $i = 0;
  foreach my $file ( "entity1_$$", "entity2_$$", "entityAll_$$" ) {
    my $data = _runAndParseNaccess($file);
    $asa[$i] = $data if ($data);
    $i++;
  }

  my $dasa;
  if ( $asa[0]->{all} and $asa[0]->{all} and $asa[2]->{all} ) {
    $dasa = ( $asa[0]->{all} + $asa[1]->{all} ) - $asa[2]->{all};
  }

  #Return the results
  return $dasa;
}

sub calculateLigandInteractionASA {
  my ( $chain1, $reg1, $ligand ) = @_;

  open( OBJALL, ">entityAll_$$.pdb" )
    || die "Could not open entityAll.$$.pdb:[$!]\n";

  #Write out each object as a pdb file
  #domain1
  open( OBJ1, ">entity1_$$.pdb" ) || die "Could not open entity1.$$.pdb:[$!]\n";
  foreach my $aa1 ( @{ $chain1->monomers } ) {
    if ($reg1) {
      next if ( $aa1->resSeq < $reg1->start or $aa1->resSeq > $reg1->end );
    }
    $aa1->write( \*OBJ1 );
    $aa1->write( \*OBJALL );
  }
  close(OBJ1);

  open( OBJ2, ">entity2_$$.pdb" ) || die "Could not open entity2.$$.pdb:[$!]\n";
  $ligand->write( \*OBJ2 );
  $ligand->write( \*OBJALL );
  close(OBJ2);

  close(OBJALL);

#Now calculate the ASA for each pdb file. Extension left off as the naccess litters
#the cwd with loads of temporary files.

  my @asa;
  my $i = 0;
  foreach my $file ( "entity1_$$", "entity2_$$", "entityAll_$$" ) {
    my $data = _runAndParseNaccess($file);
    $asa[$i] = $data if ($data);
    $i++;
  }
  my $dasa = ( $asa[0]->{all} + $asa[1]->{all} ) - $asa[2]->{all};

  #Return the results
  return $dasa;
}

=head2 _runAndParseNaccess

  Title   : _runAndParseNaccess
  Usage   : _runAndParseNaccess($fileNameNotExt)
  Function: Run Naccess on the file.  Naccess produces various temp files, based on the first part of the file name.
          : So these can be deleted, just pass in the first part of the file name. The extension is assumed to be .pdb, not
          : great, but add it to the method. File names of something.somethingelse.pdb get munged by naccess to be just something.
  Args    : file name of the pdb file lacking the extension.
  returns : Hashref containing the naccess data

=cut

sub _runAndParseNaccess {
  my ($file) = @_;
  my $asaData;
  
  #$logger->info( 'running naccess for the file '.$file );
  #Run naccess
  system("naccess $file.pdb -h > /dev/null 2>&1");

  # and die "Problem running naccess on $file.pdb:[$!] \n";
  my ( $asa1, $asa2, $asaAll );

  #Parse the results file
  open( RES, "$file.rsa" ) || die "Could not open $file.rsa :[$!]\n";
  while (<RES>) {
    if (/^TOTAL\s+(\S+)\s+\S+\s+\S+\s+\S+\s+\S+/) {
      $asaData->{all}      = $1;
      $asaData->{side}     = $2;
      $asaData->{main}     = $3;
      $asaData->{nonPolar} = $4;
      $asaData->{polar}    = $5;
    }
  }

  #Clean up
  foreach my $ext ( "asa", "log", "rsa", "pdb" ) {
    unlink("$file\.$ext") || die "Could not remove $file\.$ext :[$!]\n";
  }
  return ($asaData);
}

sub _runNoxClass {
  my ( $obj1, $obj2, $pdbId ) = @_;

  my ($oriChain1) = $obj1->internal_chain_id =~ m/[BIOM|ASYM]\d+\.\d+\.(\S+)/;
  my ($oriChain2) = $obj2->internal_chain_id =~ m/[BIOM|ASYM]\d+\.\d+\.(\S+)/;

  #Write out each object as a pdb file
  #Chain1
  open( OBJ1, ">entity1_$$.pdb" ) || die "Could not open entity1.$$.pdb:[$!]\n";
  $obj1->write( \*OBJ1 );
  close(OBJ1);

  #Chain2
  open( OBJ2, ">entity2_$$.pdb" ) || die "Could not open entity2.$$.pdb:[$!]\n";
  $obj2->write( \*OBJ2 );
  close(OBJ2);
  my $cwd = getcwd;
  chdir("/Users/rdf/Work/iPfamWebsite/iPfamScripts/NOXclass/python_scripts/");
  $logger->debug(
" NOXClass call /Users/rdf/Work/iPfamWebsite/iPfamScripts/NOXclass/python_scripts/predict.py -p 0 
            -b entity1_$$.pdb -c entity2_$$.pdb
            -e 1_1_1_0_0_1 -m 1 -f 1 
            -s /Users/rdf/Work/iPfamWebsite/iPfamData/ConSurf-HSSP/$pdbId$oriChain1.grades
            -t /Users/rdf/Work/iPfamWebsite/iPfamData/ConSurf-HSSP/$pdbId$oriChain2.grades"
  );
  open( NX,
"/Users/rdf/Work/iPfamWebsite/iPfamScripts/NOXclass/python_scripts/predict.py -p 0 -b $cwd/entity1_$$.pdb -c $cwd/entity2_$$.pdb -e 1_1_1_0_0_1 -m 1 -f 1 -s /Users/rdf/Work/iPfamWebsite/iPfamData/ConSurf-HSSP/$pdbId$oriChain1.grades -t /Users/rdf/Work/iPfamWebsite/iPfamData/ConSurf-HSSP/$pdbId$oriChain2.grades |"
  ) or die "Could not open NOXclass results\n";
  while (<NX>) {
    if (/^#/) {
      $logger->debug($_);
    }
  }
  chdir($cwd);
  exit;
}

sub _readCAR {
  my $car_data;
  while (<DATA>) {
    chomp;
    if (/^(\S{1,3})\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)\s+(\S+)/) {
      my $r = uc $1;
      my $a = uc $2;
      $$car_data{$r}{$a}{'Charge'} = $3;
      $$car_data{$r}{$a}{'Hb'}     = $4;
      $$car_data{$r}{$a}{'cr'}     = $5;
      $$car_data{$r}{$a}{'vr'}     = $6;
    }
    elsif (/^#/) {

      #first line that contains the headings
      next;
    }
    else {
      die "\n*** Unknown line in charge_and_radii.dat:$_ ***\n\n";
    }
  }
  return $car_data;
}

1;

__DATA__
ALA	N	0	D	0.752	1.4
ALA	CA	0	N	0.68	1.548
ALA	C	0	N	0.68	1.548
ALA	O	0	A	0.728	1.348
ALA     CB      0       N       0.68    1.548
ALA	OXT	-	A	0.728	1.348
VAL	N	0	D	0.752	1.4
VAL	CA	0	N	0.68	1.548
VAL	C	0	N	0.68	1.548
VAL	O	0	A	0.728	1.348
VAL	OXT	-	A	0.728	1.348
VAL	CB	0	N	0.68	1.548
VAL     CG1	0	N	0.68	1.548
VAL     CG2	0	N	0.68	1.548
LEU	N	0	D	0.752	1.4
LEU	CA	0	N	0.68	1.548
LEU	C	0	N	0.68	1.548
LEU	O	0	A	0.728	1.348
LEU	OXT	-	A	0.728	1.348
LEU	CB	0	N	0.68	1.548
LEU	CG	0	N	0.68	1.548
LEU     CD1	0	N	0.68	1.548
LEU     CD2	0	N	0.68	1.548
ILE	N	0	D	0.752	1.4
ILE	CA	0	N	0.68	1.548
ILE	C	0	N	0.68	1.548
ILE	O	0	A	0.728	1.348
ILE	OXT	-	A	0.728	1.348
ILE	CB	0	N	0.68	1.548
ILE     CG1	0	N	0.68	1.548
ILE     CG2	0	N	0.68	1.548
ILE     CD1	0	N	0.68	1.548
PRO	N	0	D	0.752	1.4
PRO	CA	0	N	0.68	1.548
PRO	C	0	N	0.68	1.548
PRO	O	0	A	0.728	1.348
PRO	CB	0	N	0.68	1.548
PRO	CG	0	N	0.68	1.548
PRO	CD	0	N	0.68	1.548
PRO	OXT	-	A	0.728	1.348
MET	N	0	D	0.752	1.4
MET	CA	0	N	0.68	1.548
MET	C	0	N	0.68	1.548
MET	O	0	A	0.728	1.348
MET	CB	0	N	0.68	1.548
MET	CG	0	N	0.68	1.548
MET	SD	0	N	1.02	1.808
MET	CE	0	N	0.68	1.548
MET	OXT	-	A	0.728	1.348
PHE	N	0	D	0.752	1.4
PHE	CA	0	N	0.68	1.548
PHE	C	0	N	0.68	1.548
PHE	O	0	A	0.728	1.348
PHE	CB	0	N	0.68	1.548
PHE	CG	0	N	0.68	1.548
PHE	CD1	0	N	0.68	1.548
PHE	CD2	0	N	0.68	1.548
PHE	CE1	0	N	0.68	1.548
PHE	CE2	0	N	0.68	1.548
PHE	CZ	0	N	0.68	1.548
PHE	OXT	-	A	0.728	1.348
TRP	N	0	D	0.752	1.4
TRP	CA	0	N	0.68	1.548
TRP	C	0	N	0.68	1.548
TRP	O	0	A	0.728	1.348
TRP	CB	0	N	0.68	1.548
TRP	CG	0	N	0.68	1.548
TRP	CD1	0	N	0.68	1.548
TRP	CD2	0	N	0.68	1.548
TRP	CE2	0	N	0.68	1.548
TRP	CE3	0	N	0.68	1.548
TRP	CZ2	0	N	0.68	1.548
TRP	CZ3	0	N	0.68	1.548
TRP	CH2	0	N	0.68	1.548
TRP	OXT	-	A	0.728	1.348
TRP	NE1	0	D	0.752	1.4
GLY	N	0	D	0.752	1.4
GLY	CA	0	N	0.68	1.548
GLY	C	0	N	0.68	1.548
GLY	O	0	A	0.728	1.348
GLY	OXT	-	A	0.728	1.348
SER	OG	0	B	0.728	1.348
SER	N	0	D	0.752	1.4
SER	CA	0	N	0.68	1.548
SER	CB	0	N	0.68	1.548
SER	C	0	N	0.68	1.548
SER	O	0	A	0.728	1.348
SER	OXT	-	A	0.728	1.348
THR	OG1	0	B	0.728	1.348
THR	N	0	D	0.752	1.4
THR	CA	0	N	0.68	1.548
THR	C	0	N	0.68	1.548
THR	O	0	A	0.728	1.348
THR	CB	0	N	0.68	1.548
THR     CG2	0	N	0.68	1.548
THR	OXT	-	A	0.728	1.348
CYS	SG	0	D	1.02	1.808
CYS	N	0	D	0.752	1.4
CYS	CA	0	N	0.68	1.548
CYS	C	0	N	0.68	1.548
CYS	O	0	A	0.728	1.348
CYS	CB	0	N	0.68	1.548
CYS	OXT	-	A	0.728	1.348
ASN	OD1	0	A	0.728	1.348
ASN	ND2	0	D	0.752	1.4
ASN	N	0	D	0.752	1.4
ASN	CA	0	N	0.68	1.548
ASN	C	0	N	0.68	1.548
ASN	O	0	A	0.728	1.348
ASN	CB	0	N	0.68	1.548
ASN	CG	0	N	0.68	1.548
ASN	OXT	-	A	0.728	1.348
GLN	OE1	0	A	0.728	1.348
GLN	NE2	0	D	0.68	1.548
GLN	N	0	D	0.752	1.4
GLN	CA	0	N	0.68	1.548
GLN	C	0	N	0.68	1.548
GLN	CB	0	N	0.68	1.548
GLN	CG	0	N	0.68	1.548
GLN	CD	0	N	0.68	1.548
GLN	O	0	A	0.728	1.348
GLN	OXT	-	A	0.728	1.348
TYR	OH	-	D	0.728	1.348
TYR	N	0	D	0.752	1.4
TYR	CA	0	N	0.68	1.548
TYR	C	0	N	0.68	1.548
TYR	O	0	A	0.728	1.348
TYR	CB	0	N	0.68	1.548
TYR	CG	0	N	0.68	1.548
TYR	CD1	0	N	0.68	1.548
TYR	CD2	0	N	0.68	1.548
TYR	CE1	0	N	0.68	1.548
TYR	CE2	0	N	0.68	1.548
TYR	CZ	0	N	0.68	1.548
TYR	OXT	-	A	0.728	1.348
ASP	OD1	0	A	0.728	1.348
ASP	OD2	-	A	0.728	1.348
ASP	N	0	D	0.752	1.4
ASP	CA	0	N	0.68	1.548
ASP	C	0	N	0.68	1.548
ASP	O	0	A	0.728	1.348
ASP	CB	0	N	0.68	1.548
ASP	CG	0	N	0.68	1.548
ASP	OXT	-	A	0.728	1.348
GLU	OE1	0	A	0.728	1.348
GLU     OE2	-	A	0.728	1.348
GLU	N	0	D	0.752	1.4
GLU	O	0	A	0.728	1.348
GLU	C	0	N	0.68	1.548
GLU	CA	0	N	0.68	1.548
GLU	CB	0	N	0.68	1.548
GLU	CG	0	N	0.68	1.548
GLU	CD	0	N	0.68	1.548
GLU	OXT	-	A	0.728	1.348
LYS	NZ	+	D	0.752	1.4
LYS	N	0	D	0.752	1.4
LYS	O	0	A	0.728	1.348
LYS	CA	0	N	0.68	1.548
LYS	C	0	N	0.68	1.548
LYS	CB	0	N	0.68	1.548
LYS	CG	0	N	0.68	1.548
LYS	CD	0	N	0.68	1.548
LYS	CE	0	N	0.68	1.548
LYS	OXT	-	A	0.728	1.348
ARG	NE	0	D	0.752	1.4	
ARG	NH2	0	D	0.752	1.4
ARG	NH1	+	D	0.752	1.4
ARG	N	0	D	0.752	1.4
ARG	O	0	A	0.728	1.348
ARG	CA	0	N	0.68	1.548
ARG	C	0	N	0.68	1.548
ARG	CB	0	N	0.68	1.548
ARG	CG	0	N	0.68	1.548
ARG	CD	0	N	0.68	1.548
ARG	NE	0	D	0.752	1.4	
ARG	CZ	0	N	0.68	1.548
ARG	OXT	-	A	0.728	1.348
HIS	ND1	0	D	0.752	1.4
HIS	NE2	+	D	0.752	1.4
HIS	N	0	D	0.752	1.4
HIS	O	0	A	0.728	1.348
HIS	CA	0	N	0.68	1.548
HIS	C	0	N	0.68	1.548
HIS	CB	0	N	0.68	1.548
HIS	CG	0	N	0.68	1.548
HIS	CD2	0	N	0.68	1.548
HIS	CE1	0	N	0.68	1.548
HIS	OXT	-	A	0.728	1.348
Be	Be 	+	D	0.352	0.628
N 	N	+	D	0.752	1.4
B	B	0	N	0.82	1.538		
O 	O	-	A	0.728	1.348
C	C	0	N	0.68	1.548
Na 	Na	+	D	0.972	2.2
Al 	Al	+	D	1.352	1.5
P  	P	+	D	1.048	1.88
S	S	-	A	1.02	1.808
Cl 	Cl	-	A	0.992	1.748
K  	K	+	D	1.328	2.388
Ca	Ca 	+	D	0.992 	1.948
Ti	Ti 	+	D	1.472 	1.948
V	V	+	D	1.328 	1.06
Cr 	Cr	+	D	1.352	1.128
Mn	Mn	+	D	1.352	1.188
Co	Co	+	D	1.328	1.128
Zn 	Zn	+	D	1.448	1.148
Ge 	Ge	+	D	1.168	3.996
As	As	+	D	1.208	0.828
Br 	Br	-	A	1.208	1.748
Mo 	Mo	+	D	1.472	1.748
Ag 	Ag	+	D	1.592 	1.548
Cs 	Cs	+	D	1.672 	3.008
Fe  Fe	+	D	1.42    1.948
F	F	-	A	0.72	1.3
H	H	0	N	0.32 	1.1
Mg 	Mg	+	D	1.1 	1.5
I	I	-	A	1.4	1.748
Au	Au	+	D	1.34	1.448
Xe	Xe	0	N	1.7     2.1
Yb 	Yb	+	D	0.86	1.54
Ar 	Ar	0	N	1.568	2.768
U  	U	+	D	0.968	1.748
W	W	+	D	0.7	1.26
Te	Te	+	D	0.7	1.26
Tl 	Tl	+	D	0.952	1.708
Ru 	Ru	+	D	1.4	1.2
Lu	Lu	+	D	0.848	1.528
Ga 	Ga	+	D	1.22	1.548
Gd	Gd	+	D	0.94	1.688
Cu  Cu  + D 1.278 1.148
Y Y 0       N       1.824   1.608
Ne      Ne      0       N       1.600   2.020
Sr      Sr      0       N       2.151   2.020
Ba      Ba      0       N       2.171   2.408
Zr      Zr      0       N       1.616   1.420
Se      Se      0       N       1.160   0.900
Pt      Pt      0       N       1.387   1.548
Md      Md      0       N       1.000   1.600
Pr      Pr      0       N       1.836   1.620
Si      Si      0       N       1.176   2.200
Kr      Kr      0       N       2.000   1.900
Nb      Nb      0       N       1.432   1.328
Am      Am      0       N       1.000   1.660
Pb      Pb      0       N       1.750   2.160
Sn      Sn      0       N       1.538   1.668
In      In      0       N       1.666   1.448
Fm      Fm      0       N       1.000   1.608
Tc      Tc      0       N       1.367   1.800
Po      Po      0       N       1.460   1.208
Cu      Cu      0       N       1.278   1.148
Rb      Rb      0       N       2.470   2.648
No      No      0       N       1.000   1.588
Lr      Lr      0       N       1.000   1.580
Er      Er      0       N       1.779   1.588
Sb      Sb      0       N       1.400   1.120
Hf      Hf      0       N       1.597   1.400
Ac      Ac      0       N       1.877   2.120
X       X       0       N       0.00    1.00
Ra      Ra      0       N       2.140   2.568
Sm      Sm      0       N       1.804   1.740
Cm      Cm      0       N       1.000   1.648
Es      Es      0       N       1.000   1.620
Ir      Ir      0       N       1.357   1.220
Bi      Bi      0       N       1.460   1.728
Re      Re      0       N       1.380   1.300
At      At      0       N       1.450   1.120
Nd      Nd      0       N       1.829   1.788
Cf      Cf      0       N       1.000   1.628
Eu      Eu      0       N       1.984   1.960
Th      Th      0       N       1.798   1.840
Li      Li      0       N       1.520   1.220
Dy      Dy      0       N       1.795   1.628
He      He      0       N       1.400   2.200
Tm      Tm      0       N       1.769   1.568
Os      Os      0       N       1.368   1.580
Ni      Ni      0       N       1.246   1.240
Sc      Sc      0       N       1.655   1.320
Pa      Pa      0       N       1.609   1.600
Pm      Pm      0       N       1.809   1.760
Xx      Xx      0       N       0.00    1.00
Pd      Pd      0       N       1.375   1.440
Ho      Ho      0       N       1.789   1.608
Ta      Ta      0       N       1.428   1.220
Fr      Fr      0       N       2.500   3.240
Hg      Hg      0       N       1.502   1.980
Bk      Bk      0       N       1.000   1.640
Np      Np      0       N       1.000   1.708
Rn      Rn      0       N       1.430   2.300
Cd      Cd      0       N       1.489   1.748
La      La      0       N       1.873   1.828
Ce      Ce      0       N       1.824   1.860
Tb      Tb      0       N       1.800   1.660
Rh      Rh      0       N       1.345   1.220
Pu      Pu      0       N       1.000   1.668
