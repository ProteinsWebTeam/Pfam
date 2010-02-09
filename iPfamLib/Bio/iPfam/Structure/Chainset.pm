
# Chainset.pm
# rdf/jt6 20080128 WTSI
#
# $Id: Chainset.pm,v 1.7 2008-03-03 13:16:46 rdf Exp $

=head1 NAME

Chainset - a bag of Chains

=cut

package Bio::iPfam::Structure::Chainset;

=head1 DESCRIPTION

The Chainset object conatins a list of any PDB chains and HETATMs/Ligand 
objects. It contains the experimental method used and the pdb assigned UID.  
There isalso the pdb header information contain within this object.

=cut

use strict;
use warnings;

use Carp;
use Data::Dump qw( dump );

use Bio::iPfam::Structure::Atom;
use Bio::iPfam::Structure::Monomer;
use Bio::iPfam::Structure::Chain;

use base qw( Bio::iPfam::Structure::Entity );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw(
    assembly
    chains
    header
    id
    biomolecule_number
    biomolecule
  )
);
__PACKAGE__->mk_ro_accessors(qw( log ));

# logging ! Why wouldn't you ?
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 add_chain

Adds a L<Chain|Bio::iPfam::Structure::Chain> to this chainset. Returns a 
reference to the added chain.

  $chainset->add_chain( $chain );

=cut

sub add_chain {
  my ( $self, $chain ) = @_;

  unless ( defined $chain and 
           $chain->isa( 'Bio::iPfam::Structure::Chain' ) ) {
    carp q(warning: can't add chain; not a Chain object);
    return;
  }

  push @{ $self->{chains} }, $chain;
  
  if ( defined $chain->chainID ) {
    $self->log->debug( 'adding chain with ID |' . $chain->chainID . '|' );
    $self->{chains_hash}->{$chain->chainID} = $chain;
  }
  else {
    carp q(warning: chain does not have a chain ID set; we will not be able to retrieve this chain using chain ID);
  }

  return $chain;
}

#-------------------------------------------------------------------------------

=head2 delete_chain

Deletes a L<Chain|Bio::iPfam::Structure::Chain> from this chainset. Returns a 
reference to the deleted chain, or undef if no chain was deleted. 

  $deleted_chain = $chainset->delete_chain( $chain );

=cut

sub delete_chain {
  my ( $self, $chain ) = @_;

  unless ( defined $chain ) {
    carp q(warning: can't delete chain; no Chain details supplied);
    return undef;
  }

  unless ( $chain->isa( 'Bio::iPfam::Structure::Chain' ) ) {
    $chain = $self->get_chain( $chain );
  }

  unless ( defined $chain and
           $chain->isa( 'Bio::iPfam::Structure::Chain' ) ) {
    carp q(warning: can't delete chain; no Chain details supplied);
    return undef;
  }

  for ( my $i = 0; $i < scalar @{ $self->{chains} }; $i++ ) {
    return splice( @{ $self->{chains} }, $i, 1 )
      if $chain eq $self->{chains}->[$i];
  }

  return undef;

}

#-------------------------------------------------------------------------------

=head2 delete_all_chains

Deletes all L<Chains|Bio::iPfam::Structure::Chain> from this Chainset.

=cut

sub delete_all_chains {
  my $self = shift;

  $self->{chains}      = [];
  $self->{chains_hash} = {};
}

#-------------------------------------------------------------------------------

=head2 write

Write the constituent Chains in PDB format. If a filehandle is supplied, 
writes to that, otherwise writes to STDOUT. No return value.

  open( PDBFILE, ">file.pdb" );
  $chainset->write( \*PDBFILE );

=cut

sub write {
  my ( $self, $fh ) = @_;

  # dump to STDOUT unless we get a different filehandle
  $fh ||= *STDOUT;

  foreach my $chain ( @{ $self->chains } ) {
    $self->log->debug( 'writing chain |' . $chain->chainID . '|' );
    $chain->write( $fh );
  }
  print $fh 'END', ' ' x 77, "\n";
}

#-------------------------------------------------------------------------------

=head2 get_chain

Returns the specified L<Chain|Bio::iPfam::Structure::Chain> or undef if not
found. Takes on argument, a scalar with the chain identifier. 

=cut

sub get_chain {
  my ( $self, $chainID ) = @_;

  return $self->{chains_hash}->{$chainID};
}

#-------------------------------------------------------------------------------

=head2 get_monomer

Returns the specified monomer. This method first tries to identify the chain
that holds the specified monomer and issues a warning if it can't find that 
chain. Behaves in the same way as 
L<get_monomer|Bio::iPfam::Structure::Monomer::get_monomer>; returns the 
specified monomer or undef if not found.

=cut

sub get_monomer {
  my ( $self, $identifier ) = @_;
  
  my $chainID;
  if ( ref $identifier eq 'HASH' ) {
    $self->log->debug( 'getting chainID from chainID element of identifier hash' );
    $chainID = ( $identifier->{chainID} || ' ' );
  }
  elsif( ref $identifier eq 'ARRAY' ) {
    $self->log->debug( 'getting chainID from first element of identifier array' );
    $chainID = ( $identifier->[0] || ' ' );
  }
  elsif( not ref $identifier ) {
    $self->log->debug( 'guessing chainID from first character of identifier' );
    $chainID = substr( $identifier, 0, 1 );
  } 

  $self->log->debug( "chainID: |$chainID|" );
  my $chain = $self->get_chain( $chainID );
  unless ( defined $chain ) {
    carp "warning: can't find that chain ($chainID)";
    return;
  }
  
  return $chain->get_monomer( $identifier );    
}

#-------------------------------------------------------------------------------

=head2 atom_count

Returns the number of atoms that are currently part of Chains within this 
Chainset. This will be 0 if there are no atoms.

  $atom_count = $chainset->atom_count;

=cut

sub atom_count {
  my $self = shift;
  
  my $count = 0;
  foreach my $chain ( @{ $self->{chains} } ) {
    $count += $chain->atom_count;
  }
  
  return $count;
}

#-------------------------------------------------------------------------------

=head2 monomer_count

Returns the number of L<Monomers|Bio::iPfam::Structure::Monomer> that are 
currently part of Chains within this Chainset. This will be 0 if there are no 
monomers.

  $monomer_count = $chainset->monomer_count;

=cut

sub monomer_count {
  my $self = shift;
  
  my $count = 0;
  foreach my $chain ( @{ $self->{chains} } ) {
    $count += $chain->monomer_count;
  }

  return $count;
}

#-------------------------------------------------------------------------------

=head2 transform

Transform the L<Atoms|Bio::iPfam::Structure::Atom> in this Chainset according to
the supplied matrix. No return value. The matrix should be of the form:

  [ [ r11, r12, r13, t1 ],
    [ r21, r22, r23, t2 ],
    [ r31, r32, r33, t3 ] ]

=cut

sub transform {
  my ( $self, $matrix ) = @_;
  
  foreach my $chain ( $self->chains ) {
     $chain->transform( $matrix );
  }

  return undef;
}

#-------------------------------------------------------------------------------

=head2 get_biomolecule

Returns the biomolecule with the specified number.

=cut

sub get_biomolecule {
  my ( $self, $biomol_number ) = @_;
  
  unless ( defined $biomol_number and
           $biomol_number =~ m/^\d+$/ ) {
    carp q(warning: not a valid biomolecule number);
    return undef;
  }
  
  return undef unless defined $self->{biomolecule}->{ $biomol_number };
  $self->log->debug( "found biomolecule number |$biomol_number|" );

  my $biomol = $self->{biomolecule}->{ $biomol_number };

  $self->log->debug( 'dumping biomol details: ', dump( $biomol ) );

  # biomolecule information is stored as follows:
  #   $self
  #     ->{biomolecule}
  #       ->{ BIOMOLECULE_NUMBER }
  #         ->{chains}
  #           ->[ CHAINS_LIST ]
  #         ->{matrices}
  #           ->{ MATRIX_NUMBER }

  # get a new Chainset
  my $chainset = Bio::iPfam::Structure::Chainset->new;

  # set some basic parameters on the new Chainset
  $chainset->id($self->id);
  $chainset->assembly(1);
  $chainset->biomolecule_number($biomol_number);

  my $chain_id_index = 0;
  foreach my $chainID ( @{ $biomol->{chains} } ) {
    $self->log->debug( "transforming chain |$chainID|" );

    foreach my $biomt_number ( sort keys %{ $biomol->{matrices} } ) {

      # get a chain from this chainset and clone it
      my $new_chain = $self->get_chain( $chainID )->clone;

      # give it a sensible chain ID. We get this from a list of acceptable
      # chain IDs that are stored in Entity
      if ( $chain_id_index > length $Bio::iPfam::Structure::Entity::chain_ids ) {
        $self->log->debug( "ran out of chain IDs after $chain_id_index; resetting" ); 
        $chain_id_index = 0;
      }
      my $new_chain_id = substr $Bio::iPfam::Structure::Entity::chain_ids, 
                                $chain_id_index++, 1;
      $new_chain->chainID( $new_chain_id );
      
      # give the clone a sensible internal chain ID
      $new_chain->internal_chain_id( 'biomt' . '.'
                                     . $chainID . '.'
                                     . $biomol_number . '.' 
                                     . $biomt_number ); 
      
      $self->log->debug( 'set chain ID / internal chain ID: |'
                         . $chainID . '/' 
                         . $new_chain->internal_chain_id . '|' );
      
      # now operate on the new chain
      my $biomt = $biomol->{matrices}->{$biomt_number};
      $self->log->debug( 'applying biomt: ', dump( $biomt ) );
      $new_chain->transform( $biomt );
   
      # and add it to the chainset
      $chainset->add_chain( $new_chain );
    } 
  }

  return $chainset;
}



#-------------------------------------------------------------------------------
#- stuff awaiting cargo culting ------------------------------------------------
#-------------------------------------------------------------------------------

#=head2 biomoleculeNo 
#
#Title    : biomoleculeNo
#Function : If this is an assembly, then it stores the biomolecule number
#Returns  : number (integer)
#Args     : integer
#Usage    : $cs->biomoleculeNo(1) or $a = $cs->biomoleculeNo();
#
#=cut
#
#sub biomoleculeNo {
#  my ( $self, $bmNo ) = @_;
#
#  if ( defined $bmNo and $bmNo =~ /d+/ ) {
#    $logger->debug("Setting the biomolecule number to be |$bmNo|");
#    $self->assembly(1);
#    $self->{biomol} = $bmNo;
#  }
#  return $self->{biomol};
#}

#=head2 write_header
#
#Title    : write_header
#Function : Write the data contained in the header
#Returns  : Nothing
#Args     : A writable filehandle.
#
#=cut
#
#sub write_header {
#  my ( $self, $FH ) = @_;
#  return unless $self->header;
#  foreach ( $self->header ) {
#    print $FH "$_\n";
#  }
#}
#
#=head2 write_pdb_style
#
#Title    : write_pdb_style
#Function : Write the data contained in the chainset object as a PDB file.
#Returns  : Nothing
#Args     : A writable filehandle.
#
#=cut
#
#sub write_pdb_style {
#  my ( $self, $FH ) = @_;
#
#  #Start off with a line to say where this has come from
#  print $FH 'HEADER   Produced from Robs code - Original ' . $self->id . "\n";
#  $self->write_header;
#
##Print all of the "Chains" in the PDB file. These can be protein or nucleic acid.
#  $self->write($FH);
#
#  #Finish off the file
#  print $FH "END\n";
#}
#
#=head2 expt_method
#
#Title    : expt_method
#Function : Stores the experimental method used to obtain the structure
#Returns  : string
#Args     : string containing method
#
#=cut
#
#sub expt_method {
#  my ( $self, $method ) = @_;
#
#  return $self->{expt} unless defined $method;
#
#  $logger->debug("Method is $method");
#
#  # What's wrong with using whatever is in the PDB file ?
#  if ( $method =~ /DIFFRACTION/ ) {
#
#    if ( $method =~ /X-RAY/ ) {
#      $self->{expt} = 'X-RAY DIFFRACTION';
#    }
#    elsif ( $method =~ /ELECTRON/ ) {
#      $self->{expt} = 'ELECTRON DIFFRACTION';
#    }
#    elsif ( $method =~ /FIBER/ ) {
#      $self->{expt} = 'ELECTRON DIFFRACTION';
#    }
#    elsif ( $method =~ /NEUTRON/ ) {
#      $self->{expt} = 'NEUTRON DIFFRACTION';
#    }
#
#  }
#  elsif ( $method =~ /NMR/ ) {
#    $self->{expt} = 'NMR';
#  }
#  elsif ( $method =~ /THEORETICAL/ ) {
#    $self->{expt} = 'THEORETICAL MODEL';
#  }
#  else {
#    $self->{expt} = 'OTHER';
#  }
#
#  return $self->{expt};
#}
#
#=head2 transform_chainset
#
#Title    : transform_chainset
#Function : Transforms the chainset...
#Returns  : reference to a matrix ?
#Args     : none
#
#=cut
#
#sub transform_chainset {
#  my ( $self, $trans ) = @_;
#  foreach my $chain ( @{ $self->each } ) {
#    $chain->transform_chain($trans);
#  }
#  foreach my $ligand ( @{ $self->each_ligand } ) {
#    $ligand->transform_ligand($trans);
#  }
#}
#
#=head2 domainer
#
#Title    : domainer
#Function : Locate the domain definitions and make a chain for the region
#Returns  : ref to domain set
#Args     : array of map data
#
#=cut
#
#sub domainer {
#  my ( $self, @mapdata ) = @_;
#  chain_id my $domainset =
#    Bio::iPfam::Structure::Chainset->new( 'uid' = $self->id );
#
#  my %domains;
#  foreach my $domain_coords (@mapdata) {
#    $domains{ $domain_coords->[11] }++
#      unless $domains{ $domain_coords->[11] };
#  }
#}
#
#foreach my $domain_index ( keys %domains ) {
#  my $domain;
#DOMAIN: foreach my $domain_coords (@mapdata) {
#    next DOMAIN unless $- > domain_coords [11] eq $domain_index;
#    my $domain_ chain = $domain_coords->[3] || 'null';
#  CHAIN: foreach my $chain ( @{ $self->each_chain } ) {
#      next CHAIN unless $domain_chain eq $chain->chain_id;
#      if ( defined $domain ) {
#        $domain =
#          $chain->region( $domain_coords->[4], $domain_coords->[5], $domain );
#      }
#      else {
#        $domain = $chain->region( $domain_coords->[4], $domain_coords->[5] );
#      }
#    }
#  }
#  $domainset->add($domain);
#}
#return $domainset;
#}
#
#=head2 bioMolData
#
#Title    :
#Usage    :  
#Function :
#Args     :
#Returns  :
#
#=cut
#
#sub bioMolData {
#  my ( $self, $matrices, $chAndComRef ) = @_;
#  if ( $matrices and $chAndComRef ) {
#    $self->{bioMat}       = $matrices;
#    $self->{bioMolChains} = $chAndComRef;
#  }
#  return ( $self->{bioMat}, $self->{bioMolChains} );
#}
#
#=head2 applyBioMat
#
#Title    : applyBioMat
#Function : This applies the biomolecule matrix found in the header of the pdb 
#           file remark 350. As there can me multiple different matrices, a 
#           matrix number needs to be passed in. 
#Usage    : $assemblySet = $chainset->applyBioMat(1)
#Args     : Index of the matrix that you want to apply.
#Returns  : A new chainset object
#
#=cut
#
#sub applyBioMat {    
#  my ( $self, $mNo ) = @_;
#
#  my ( $m, $c ) = $self->bioMolData;
#
#  #$logger->debug("$m, $c");
#
#  my $tChainSet = Bio::iPfam::Structure::Chainset->new(
#    'uid'      => $self->id,
#    'assembly' => 1,
#    'biomol'   => $mNo
#  );
#  return $tChainSet unless ( $m and $c );
#
#  my $chains = $c->{$mNo}->{chains};
#
#  #$logger->debug("Going to try and transform $chains");
#
#  foreach my $ch ( split( /\,/, $chains ) ) {
#    $ch =~ s/\s+//g;
#
#    #$logger->debug("Working on |$ch|");
#
#    foreach my $chain ( @{ $self->each_chain } ) {
#
#      #print STDERR "Got $chain\n";#|".$chain->chain_id."|\n";
#      if ( $chain->chain_id eq $ch ) {
#        foreach my $matNo ( sort { $a <=> $b } keys( %{ $m->{$mNo} } ) ) {
#
#          #$logger->debug("MATRIX No. $matNo");
#          my $tChain =
#            Bio::iPfam::Structure::Chain->new( "chain_id", $CHAINS[$matNo],
#            "intChainId", "BM$mNo.$matNo.$ch", "type", $chain->type );
#          $tChainSet->add_chain($tChain);
#
#          #$logger->debug( "Making new chain" . $tChain->chain_id() );
#          foreach my $res ( @{ $chain->each } ) {
#            my $tRes =
#              Bio::iPfam::Structure::Residue->new( "residue_no",
#              $res->residue_no, "type", $res->type );
#            $tRes->residue_no( $res->residue_no );
#            $tChain->add($tRes);
#            foreach my $atom ( @{ $res->each } ) {
#              my $tAtom =
#                Bio::iPfam::Structure::Atom->new( "a_type", $atom->type,
#                "a_temperature", $atom->temperature, "a_no", $atom->number,
#                "primary", $atom->primary );
#              $tRes->add($tAtom);
#              my @newXYZ = $atom->rotate_atom( $m->{$mNo}->{$matNo} );
#              $tAtom->xyz(@newXYZ);
#            }
#          }
#        }
#      }
#    }
#  }
#  return ($tChainSet);
#}

#=head1 read_pdb
#
#Title    : read_pdb
#Function : Read in a pdb file.  This is specific to the pdb file format.
#Returns  : nothins
#Args     : Either a filehandle or an array ref 
#Usage    : $chainset->read_pdb(\*FILEHANDLE) or chainset->read_pdb(\@pdb);
#
#=cut
#
#sub read_pdb {
#  my $self = shift;
#  my $PDB  = shift;
#  my $pdb;
#  if ( ref($PDB) eq "GLOB" ) {
#
#    #Filehandle has been passed in!
#    print STDERR "Using filehanle\n";
#    while (<$PDB>) {
#      push( @$pdb, $_ );
#    }
#  }
#  else {
#    $pdb = $PDB;
#  }
#
#  my ( $chain, $residue, $ligand, %lig_head_info );
#LINE:
#  for ( my $i = 0 ; $i < scalar(@$pdb) ; $i++ ) {
#    my @line = split( "", $pdb->[$i] );
#    my $record = join( "", @line[ 0 .. 5 ] );
#    $record =~ s/\s//g;
#    if ( $record eq "ATOM" ) {
#
#      #Here I need to try and distinguish between protein and DNA/RNA
#      my $atom_no = join( "", @line[ 6 .. 10 ] );
#      $atom_no =~ s/\s+//g;
#      my $atom_type = join( "", @line[ 12 .. 15 ] );
#      $atom_type =~ s/\s+//g;
#      my $alt_char = $line[16];
#      my $res_name = join( "", @line[ 17 .. 19 ] );
#      $res_name =~ s/\s+//g;
#      my $chain_id = $line[21];
#      $chain_id = "null" if ( $chain_id eq " " );
#      my $res_no = join( "", @line[ 22 .. 25 ] );
#      $res_no =~ s/\s+//g;
#      my $res_insert = $line[26];
#      my $x = join( "", @line[ 30 .. 37 ] );
#      $x =~ s/\s+//g;
#      my $y = join( "", @line[ 38 .. 45 ] );
#      $y =~ s/\s+//g;
#      my $z = join( "", @line[ 46 .. 53 ] );
#      $z =~ s/\s+//g;
#      my $atom_occu = join( "", @line[ 54 .. 59 ] );
#      $atom_occu =~ s/\s+//g;
#      my $atom_temp = join( "", @line[ 60 .. 65 ] );
#      $atom_temp =~ s/\s+//g;
#
#      #Protein/DNA/RNA
#      if ( !$chain or $chain_id ne $chain->chain_id() ) {
#
#        #new chain is needed
#        #$logger->debug("new chain: $chain_id");
#        $chain =
#          Bio::iPfam::Structure::Chain->new( "type", "undetermined", "chain_id",
#          $chain_id, "intChainId", "ASYM1.1.$chain_id" );
#        $self->add_chain($chain);
#      }
#
#      if ( !$residue or $res_no != $residue->residue_no ) {
#        $residue =
#          Bio::iPfam::Structure::Residue->new( "residue_no", $res_no, "type",
#          $res_name );
#        $chain->add($residue);
#      }
#
#      my $atom =
#        Bio::iPfam::Structure::Atom->new( "a_no", $atom_no, "a_type",
#        $atom_type, "a_temperature", $atom_temp );
#      $atom->xyz( ( $x, $y, $z ) );
#      $residue->add($atom);
#
#    }
#    elsif ( $record eq "HETATM" ) {
#      my $atom_no = join( "", @line[ 6 .. 10 ] );
#      $atom_no =~ s/\s+//g;
#      my $atom_type = join( "", @line[ 12 .. 15 ] );
#      $atom_type =~ s/\s+//g;
#      my $alt_char = $line[16];
#      my $res_name = join( "", @line[ 17 .. 19 ] );
#      $res_name =~ s/\s+//g;
#      my $chain_id = $line[21];
#      $chain_id = "null" if ( $chain_id eq " " );
#      my $res_no = join( "", @line[ 22 .. 25 ] );
#      $res_no =~ s/\s+//g;
#      my $res_insert = $line[26];
#      my $x = join( "", @line[ 30 .. 37 ] );
#      $x =~ s/\s+//g;
#      my $y = join( "", @line[ 38 .. 45 ] );
#      $y =~ s/\s+//g;
#      my $z = join( "", @line[ 46 .. 53 ] );
#      $z =~ s/\s+//g;
#      my $atom_occu = join( "", @line[ 54 .. 59 ] );
#      $atom_occu =~ s/\s+//g;
#      my $atom_temp = join( "", @line[ 60 .. 65 ] );
#      $atom_temp =~ s/\s+//g;
#
#      if ( $res_name ne "HOH" ) {
#        if ( !$ligand or $chain_id ne $ligand->chain_id() ) {
#
#          #new chain is needed
#          $ligand =
#            new_ligand( $res_no, $res_name, $chain_id, \%lig_head_info );
#          $self->add_ligand($ligand);
#        }
#        elsif ( $res_no != $ligand->ligand_no ) {
#          $ligand =
#            new_ligand( $res_no, $res_name, $chain_id, \%lig_head_info );
#          $self->add_ligand($ligand);
#        }
#
#        my $atom =
#          new_atom( "hetatm", 1, $atom_type, $atom_no, $atom_temp,
#          ( $x, $y, $z ) );
#        $ligand->add($atom);
#      }
#      else {
#
#        #warn "Ignoring water";
#      }
#    }
#    elsif ( $record eq "HET" ) {
#      my $cmpd = join( "", @line[ 7 .. 9 ] );
#      $cmpd =~ s/\s+//g;    #Remove all white space
#
#      #print "Compound=$cmpd\n";
#      $lig_head_info{$cmpd}{'chain'}     = $line[12];
#      $lig_head_info{$cmpd}{'insert'}    = $line[17];
#      $lig_head_info{$cmpd}{'seqNum'}    = join( "", @line[ 13 .. 16 ] );
#      $lig_head_info{$cmpd}{'numHetAtm'} = join( "", @line[ 20 .. 24 ] );
#      $lig_head_info{$cmpd}{'info'}      = join( "", @line[ 30 .. 70 ] );
#    }
#    elsif ( $record eq "FORMUL" ) {
#
#      #This is the formula of the ligand
#      my $cmpd = join( "", @line[ 12 .. 14 ] );
#      $cmpd =~ s/\s+//g;    #Remove all white space
#
#      #There could be a continuation so add on.
#
#      if ( defined @line[ 19 .. 69 ] ) {
#        if ( $lig_head_info{$cmpd}{'formula'} ) {
#          $lig_head_info{$cmpd}{'formula'} .= join( "", @line[ 19 .. 69 ] );
#        }
#        else {
#          $lig_head_info{$cmpd}{'formula'} = join( "", @line[ 19 .. 69 ] );
#        }
#      }
#    }
#    elsif ( $record eq "HETNAM" ) {
#
#      #The name of HET
#      my $cmpd = join( "", @line[ 11 .. 13 ] );
#      $cmpd =~ s/\s+//g;    #Remove all white space
#      $lig_head_info{$cmpd}{'name'} .= join( "", @line[ 15 .. 69 ] );
#    }
#    elsif ( $record eq "HETSYN" ) {
#      my $cmpd = join( "", @line[ 11 .. 13 ] );
#      $cmpd =~ s/\s+//g;    #Remove all white space
#      $lig_head_info{$cmpd}{'syn'} .= join( "", @line[ 15 .. 69 ] );
#    }
#    elsif ( $record eq "HEADER" ) {
#      my $h = join( "", @line[ 6 .. 69 ] );
#      if ( $h =~ /^(.*)(\S{4})(\s+)(\d?)$/ ) {
#        $self->id($2);
#      }
#      $self->header($h);
#    }
#    elsif ( $record eq "CRYST1" ) {
#
#      #If there is such a field, then this must have been X-Ray crystal
#      $self->expt_method("X-RAY DIFFRACTION");
#    }
#    elsif ( $record eq "EXPDTA" ) {
#      my $h = join( "", @line[ 6 .. 69 ] );
#      $self->expt_method($h);
#    }
#    elsif ( $record eq "REMARK" ) {
#      my $remarkNo = join( "", @line[ 6 .. 9 ] );
#      if ( $remarkNo == 350 ) {
#
#        #$logger->debug("Getting biomolecule data");
#        my ( $matricesRef, $chAndComRef, $bmNo, $matNo );
#        for ( my $l = $i ; $l < scalar(@$pdb) ; $l++ ) {
#          my @line = split( //, $pdb->[$l] );
#          my $bit = join( "", @line[ 0 .. 9 ] );
#          if ( $bit ne "REMARK 350" ) {
#            $self->bioMolData( $matricesRef, $chAndComRef );
#            $i = $l - 1;
#            last;
#          }
#          my $info = join( "", @line[ 11 .. 69 ] );
#          if ( $info =~ /BIOMOLECULE: (\d+)/ ) {
#            $bmNo = $1;
#          }
#          elsif ( $info =~ /APPLY THE FOLLOWING TO CHAINS: (.*)/ ) {
#            my $chains = $1;
#            $chAndComRef->{$bmNo}->{chains} = $chains;
#          }
#          elsif ( $info =~ /BIOMT1/ ) {
#            $matNo = join( "", @line[ 20 .. 22 ] );
#            $matNo =~ s/\s+//;
#            $matricesRef->{$bmNo}->{$matNo}->{r11} =
#              join( "", @line[ 24 .. 33 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{r12} =
#              join( "", @line[ 34 .. 43 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{r13} =
#              join( "", @line[ 44 .. 53 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{tx} =
#              join( "", @line[ 54 .. 69 ] );
#          }
#          elsif ( $info =~ /BIOMT2/ ) {
#            $matNo = join( "", @line[ 20 .. 22 ] );
#            $matNo =~ s/\s+//;
#            $matricesRef->{$bmNo}->{$matNo}->{r21} =
#              join( "", @line[ 24 .. 33 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{r22} =
#              join( "", @line[ 34 .. 43 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{r23} =
#              join( "", @line[ 44 .. 53 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{ty} =
#              join( "", @line[ 54 .. 69 ] );
#          }
#          elsif ( $info =~ /BIOMT3/ ) {
#            $matNo = join( "", @line[ 20 .. 22 ] );
#            $matNo =~ s/\s+//;
#            $matricesRef->{$bmNo}->{$matNo}->{r31} =
#              join( "", @line[ 24 .. 33 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{r32} =
#              join( "", @line[ 34 .. 43 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{r33} =
#              join( "", @line[ 44 .. 53 ] );
#            $matricesRef->{$bmNo}->{$matNo}->{tz} =
#              join( "", @line[ 54 .. 69 ] );
#          }
#          elsif ( $info =~ /\S+/ && $bmNo ) {
#            chomp($info);
#            $chAndComRef->{$bmNo}->{comments} .= $info;
#          }
#        }
#      }
#    }
#  }
#
#  #$logger->debug("read PDB file\n");
#
#  #Here would be the best place to determine the chain types......
#  foreach my $chain ( @{ $self->each_chain } ) {
#    $chain->_guess_alphabet;
#  }
#
#  #$logger->debug("Guessed chain type file\n");
#}

#-------------------------------------------------------------------------------
#- plain functions -------------------------------------------------------------
#-------------------------------------------------------------------------------

#  sub new_chain {
#    my ( $type, $chain_id ) = @_;
#
#    my $chain = Bio::iPfam::Structure::Chain->new();
#
#    $chain->chain_id($chain_id);
#    $chain->type($type);
#    $chain->region_info(0);
#
#    return $chain;
#  }
#
#  sub new_atom {
#    my ( $chain_type, $first, $atom_type, $atom_no, $atom_temperature,
#      @atom_xyz )
#      = @_;
#
#    my $atom = Bio::iPfam::Structure::Atom->new(
#      'a_no'          => $atom_no,
#      'a_type'        => $atom_type,
#      'a_temperature' => $atom_temperature
#    );
#
#    $atom->xyz(@atom_xyz);
#
#    return $atom;
#  }
#
#  sub new_ligand {
#    my ( $lig_no, $lig_name, $chain_id, $lig_head_info ) = @_;
#
#    my $ligand = Bio::iPfam::Structure::Ligand->new_ligand(
#      'ligand_no' => $lig_no,
#      'type'      => $lig_name,
#      'chain'     => $chain_id
#    );
#
#    if ( defined $$lig_head_info{$lig_name}{numHetAtm} ) {
#      $ligand->numHetAtm( $$lig_head_info{$lig_name}{numHetAtm} );
#    }
#    if ( $$lig_head_info{$lig_name}{info} ) {
#      $ligand->info( $$lig_head_info{$lig_name}{info} );
#    }
#    if ( $$lig_head_info{$lig_name}{formula} ) {
#      $ligand->formula( $$lig_head_info{$lig_name}{formula} );
#    }
#    if ( $$lig_head_info{$lig_name}{name} ) {
#      $ligand->name( $$lig_head_info{$lig_name}{name} );
#    }
#    if ( $$lig_head_info{$lig_name}{syn} ) {
#      $ligand->synonym( $$lig_head_info{$lig_name}{syn} );
#    }
#
#    return $ligand;
#  }

=head1 COPYRIGHT

Copyright (c) 2008: Genome Research Ltd.

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
