
# PDBFactory.pm
# rdf/jt6 20080128 WTSI
#
# $Id: PDBFactory.pm,v 1.6 2008-03-03 13:16:46 rdf Exp $

=head1 NAME

PDBFactory - a factory for PDBs...

=cut

package Bio::iPfam::Structure::PDBFactory;

=head1 DESCRIPTION

This is a factory for generating an object representation of a PDB structure.

=cut

use strict;
use warnings;

use Carp;
use Data::Dump qw( dump );

use Bio::iPfam::Structure::Atom;
use Bio::iPfam::Structure::Monomer;
use Bio::iPfam::Structure::Ligand;
use Bio::iPfam::Structure::Chain;
use Bio::iPfam::Structure::Chainset;
use Bio::iPfam::Structure::Residue;
use Bio::iPfam::Structure::Base;
use Bio::iPfam::Structure::Entity;

use base qw( Class::Accessor::Fast );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw( chainset ) );
__PACKAGE__->mk_ro_accessors( qw( log ) );

# logging ! Gotta have it.
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 new 

Creates a new C<Chainset> object.

  $factory = Bio::iPfam::Structure::PDBFactory->new;  

=cut

sub new {
  my ( $caller, $source ) = shift;

  # construct the object
  my $class;
  if ( ref $caller ) {
    $class = ref $caller;
  }
  else {
    $class = $caller;
  }
  my $self = {};
  bless $self, $class;

  # get a logger
  $self->{log} = get_logger(__PACKAGE__);

  $self->log->debug( 'instantiated a PDBFactory object' );

  return $self;
}

#-------------------------------------------------------------------------------

=head2 parse

Generates a PDB object structure. Requires one argument, which can be either a
scalar that contains the name of the PDB file to read, or a reference to an
open filehandle from which the PDB file can be read.

  $chainset = $factory->parse( $filename );
  
  open( FILE, "<$pdb_filename" );
  $chainset = $factory->parse( \*FILE );

=cut

sub parse {
  my ( $self, $source ) = @_;
  
  unless ( defined $source ) {
    carp q(warning: need either a filename or a file handle);
    return;
  }
  
  # see which we got, filehandle or filename
  my $fh;
  if ( ref $source eq 'GLOB' ) {
    $fh = $source;
  }elsif( ref($source) eq "IO::Socket::INET" ) { 
    $self->log->info("Got sockect");
    $fh = $source;
  }else {

    if( -e $source ) {
      
      unless ( open( $fh, "<$source" ) ) {
        carp qq(warning: couldn't open file "$source"; $!);
        return;
      }
      
    }
    else {
      carp qq(warning: couldn't find file "$source"; $!);
      return;
    }

  }

  # read it...
  $self->_read( $fh );
  
  return $self->chainset;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head1 PRIVATE METHODS

=head2 _read

Reads a PDB file and constructs an object representation of the contents.
Requires a single argument, which must be a reference to an open filehandle, 
from which the file can be read. No return value; populates the factory object.

  $chainset = $factory->_read( $filehandle );

=cut

sub _read {
  my ( $self, $fh ) = @_;

  $self->log->debug( "reading from filehandle: |$fh|" );

  $self->chainset( Bio::iPfam::Structure::Chainset->new );

  # processing loop...
  LINE: while( my $record = <$fh> ) {
    
    # what flavour of line is this ?
    my $record_name = substr $record, 0, 6;
    $self->log->debug( "processing record name |$record_name|" );

    if   ( $record_name eq 'ATOM  ' or
           $record_name eq 'HETATM' ) { $self->_read_atom(   $record ) }
    elsif( $record_name eq 'HET   ' ) { $self->_read_het(    $record ) }    
    elsif( $record_name eq 'HETNAM' ) { $self->_read_hetnam( $record ) }    
    elsif( $record_name eq 'FORMUL' ) { $self->_read_formul( $record ) }
    elsif( $record_name eq 'REMARK' ) { $self->_read_remark( $record ) }

  } # end of loop "LINE"
  
  $self->log->debug( 'done parsing' );

    $self->_catalogue_chains;
  $self->_set_primaries;

  # merge the information on heterogens with the Ligand objects that we built
  $self->_catalogue_ligands;

}

#-------------------------------------------------------------------------------

=head2 _read_atom

Read an ATOM/HETATM record. We use two arrays and two hashes to keep track of
the monomers and chains as we build them. These are:

=over 4

=item $self->{chains_list}

=item $self->{chains_hash}

=item $self->{monomers_list}

=item $self->{monomers_hash}

=back

These are kept on the object and are intended for internal for book-keeping 
only. They shouldn't be accessed other than from this method. 

The class of each monomer is determined using a lookup table on
L<Entity|Bio::iPfam::Structure::Entity>, which assigned recognised amino-acids
and nucleotides as either L<Residue|Bio::iPfam::Structure::Residue> or
L<Base|Bio::iPfam::Structure::Base> respectively.

=cut

sub _read_atom {
  my ( $self, $record ) = @_;

  $self->log->debug( 'got an atom' );
  
  # get an Atom object...
  my $atom = Bio::iPfam::Structure::Atom->new( $record );
  
  my $chainID = $atom->chainID || ' ';
  $self->log->debug( "looking for chain with ID |$chainID|" );
  
  # see if we've already built a Chain object for this chain
  my $chain = $self->{chains_hash}->{ $chainID };
  unless ( defined $chain ) {
    
    # no; build it now and add it to the various tracking data structures.
    # We keep three references to this chain: @chains stores the chains 
    # in the order in which we encounter them; %chains hashes them for
    # easy lookup; $chainset is the Chainset that we hand back at the end of
    # all this
    $self->log->debug( 'creating a new chain' );
    $chain = Bio::iPfam::Structure::Chain->new( $record );
    
    $self->{chains_hash}->{ $chainID } = $chain;
    push @{ $self->{chains_list} }, $chain;

    $self->log->debug( "adding a new chain ($chainID)" );
    $self->chainset->add_chain( $chain );
    $self->log->debug( 'added new chain' );
  }      
  
  # and on to the monomer level. We need to take account of the three 
  # parameters that uniquely define each monomer, namely the chain ID, 
  # monomer sequence number and the insertion code. Make a "fingerprint"
  # for each monomer from those values. We assume that, at a minimum, the
  # atom data contains a monomer sequence number. If it doesn't, we're sort
  # of screwed
  my $monomer_fingerprint =   ( $atom->chainID || ' ' )
                            . ( $atom->resSeq ) 
                            . ( $atom->iCode || '' );
  $self->log->debug( "looking for monomer with fingerprint |$monomer_fingerprint|" );
  
  # see if we've already encountered this monomer...
  my $monomer = $self->{monomers_hash}->{ $monomer_fingerprint };
  unless ( defined $monomer ) {
    
    # no; build a new Monomer and keep track of it
    if ( $atom->hetatm ) {
      $self->log->debug( 'creating new Ligand' ); 
      $monomer = Bio::iPfam::Structure::Ligand->new( $record );
    }
    else {

      # there are two flavours of Monomer, Residue and Base. Look up the 
      # type and create the appropriate object
      my $monomer_type =
        $Bio::iPfam::Structure::Entity::monomer_types->{ $atom->resName };

      # is this a recognised amino-acid or base ? Yes; make a Residue or Base      
      if ( defined $monomer_type ) {
        $self->log->debug( "creating new monomer, type |$monomer_type|" ); 
        $monomer = $monomer_type->new( $record );
      }
      # no; make this a generic Monomer
      else {
        $self->log->debug( 'creating new generic Monomer' ); 
        $monomer = Bio::iPfam::Structure::Monomer->new( $record );
      }
    }
    
    $self->{monomers_hash}->{ $monomer_fingerprint } = $monomer;
    push @{ $self->{monomers_list} }, $monomer;
    $chain->add_monomer( $monomer );
  }      
  
  # finally, add the Atom to the Monomer 
  $monomer->add_atom( $atom );

}

#-------------------------------------------------------------------------------

=head2 _read_het

Parses HET records and builds a list of the heterogens in the PDB file.

=cut

sub _read_het {
  my ( $self, $record ) = @_;
  
  $self->log->debug( 'got a HET' );

  my $entity = Bio::iPfam::Structure::Entity->new( $record, { 'HET   ' => 1 } );
  
  my $het_fields  = $entity->unassigned_fields;
  my $hetID       = $het_fields->{hetID};
                    
  unless ( exists $self->{hets}->{$hetID} ) {
    $self->log->debug( "didn't find hetID |$hetID|; creating now" );
    
    $self->{hets}->{$hetID} = { instances => [],
                                text      => '',
                                formula   => '' };
    
  }

  push @{ $self->{hets}->{$hetID}->{instances} }, $het_fields;
   
   $self->log->debug( 'we now have |'
                      . scalar @{ $self->{hets}->{$hetID}->{instances} }
                      . "| instances of |$hetID|" );    
}

#-------------------------------------------------------------------------------

=head2 _read_het

Parses HETNAM records and records information on each type of heterogen in the
PDB file.

=cut

sub _read_hetnam {
  my ( $self, $record ) = @_;
  
  $self->log->debug( 'got a HETNAM' );

  my $entity = Bio::iPfam::Structure::Entity->new( $record, { 'HETNAM' => 1 } );
   
  my $het_fields  = $entity->unassigned_fields;
  my $hetID       = $het_fields->{hetID};
                    
  # ignore water
  return if $hetID eq 'HOH';

  unless ( exists $self->{hets}->{$hetID} ) {
    warn qq(didn't find a pre-existing heterogen "$hetID"; skipping this HETNAM);
    return;
  }

  if ( $het_fields->{continuation} ) {
    $self->log->debug( 'this line is a continuation of a previous HETNAM' );    
    $self->{hets}->{$hetID}->{description} .= ' ' . $het_fields->{text};
  }
  else {
    $self->{hets}->{$hetID}->{description}  = $het_fields->{text};
  }

   $self->log->debug(   'heterogen description is now |'
                      . $self->{hets}->{$hetID}->{description} . '|' );
}

#-------------------------------------------------------------------------------

=head2 _read_het

Parses HETNAM records and records the chemical formula for each type of 
heterogen in the PDB file.

=cut

sub _read_formul {
  my ( $self, $record ) = @_;
  
  $self->log->debug( 'got a FORMUL' );

  my $entity = Bio::iPfam::Structure::Entity->new( $record, { 'FORMUL' => 1 } );
  
  my $het_fields  = $entity->unassigned_fields;
  my $hetID       = $het_fields->{hetID};
                    
  # ignore water
  return if $hetID eq 'HOH';

  unless ( exists $self->{hets}->{$hetID} ) {
    warn qq(didn't find a pre-existing heterogen "$hetID"; skipping this FORMUL);
    return;
  }

  if ( $het_fields->{continuation} ) {
    $self->log->debug( 'this line is a continuation of a previous FORMUL' );    
    $self->{hets}->{$hetID}->{formula} .= ' ' . $het_fields->{text};
  }
  else {
    $self->{hets}->{$hetID}->{formula}  = $het_fields->{text};
  }

   $self->log->debug( "formula for |$hetID| is now |"
                      . $self->{hets}->{$hetID}->{formula} . '|' );
}


#-------------------------------------------------------------------------------

=head2 _set_primaries


=cut

sub _set_primaries{
  my $self = shift;
  
  foreach my $chain (@{$self->chainset->chains}){
    $self->log->debug("Setting primary atom for chain $chain");
    if(($chain->type) eq 'protein'){
      foreach my $residue (@{$chain->monomers}){
        $self->log->debug("Setting primary atom for residue $residue");
        #foreach my $atom (@{$residue->atoms}){
        #  $atom->primary(1) if($atom->realName);
        #}
        my $atom = $residue->get_atom('CA');
        if($atom){
          $residue->primary($atom);
        }else{
          $residue->primary($residue->atoms->[0]);
        }
#        if($atom){
#          $self->log->debug("Setting primary atom");
#          $atom->primary(1);
#        }else{
#          $self->log->debug("failed to find CA atom in residue"); 
#        }
      } 
    }else{
      foreach my $monomer (@{$chain->monomers}){
        $monomer->primary($monomer->atoms->[0]);
      } 
    } 
  } 
}




#-------------------------------------------------------------------------------

=head2 _catalogue_ligands

As we parse the PDB file, we generate new 
L<Ligands|Bio::iPfam::Structure::Ligand> from the HETATM lines and we store 
the information on heterogens that we find in HET, HETNAM and FORMUL records.
This method walks through the list of heterogens that we found and tries to 
assign the information from the HET, HETNAM and FORMUL records to the Ligand
objects.

=cut

sub _catalogue_ligands {
  my $self = shift;

  foreach my $hetID ( keys %{ $self->{hets} } ) {
    $self->log->debug( "hetID: |$hetID|" );
    $self->log->debug( '  formula:     |' . $self->{hets}->{$hetID}->{formula} . '|' );
    $self->log->debug( '  description: |' . $self->{hets}->{$hetID}->{description} . '|' );
    foreach my $het ( @{ $self->{hets}->{$hetID}->{instances} } ) {
      $self->log->debug( '  chainID:     |' . $het->{chainID} . '|' );
      $self->log->debug( '  seqNum:      |' . $het->{seqNum} . '|' );
      $self->log->debug( '  iCode:       |' . $het->{iCode} . '|' );
      $self->log->debug( '  numHetAtoms: |' . $het->{numHetAtoms} . '|' );
      my $ligand = $self->chainset->get_monomer( { chainID => $het->{chainID},
                                                   resSeq  => $het->{seqNum},
                                                   iCode   => $het->{iCode} } );
      #$self->log->info(dump $ligand);
      $ligand->hetID( $hetID );
      $ligand->formula( $self->{hets}->{$hetID}->{formula} );
      $ligand->description( $self->{hets}->{$hetID}->{description} );
      
      # check that the ligand has the expected number of constituent atoms
      $ligand->numHetAtoms( $het->{numHetAtoms} );
      unless ( $ligand->atom_count == $het->{numHetAtoms} ) {
        carp 'warning: ligand residue "'
             . $het->{chainID} . $het->{seqNum} . $het->{iCode}
             . '" does not have the expected number of constituent atoms (has '
             . $ligand->atom_count . ', expected ' .$het->{numHetAtoms} . ')';
      }
    }
  }
}

sub _catalogue_chains {
  my $self = shift;
  
  CHAIN:
  foreach my $c (@{$self->chainset->chains}){
    if(!$c->type){
      foreach my $monomer (@{$c->monomers}){ 
        if(ref($monomer) eq 'Bio::iPfam::Structure::Residue'){
          $c->type('protein');
          next CHAIN;
        }elsif(ref($monomer) eq 'Bio::iPfam::Structure::Ligand'){
          $c->type('ligand');
          next CHAIN;
        }elsif(ref($monomer) eq 'Bio::iPfam::Structure::Base'){ 
          if($monomer->get_atom("O2'")){
            $c->log->info("Setting to RNA");
            $c->type('RNA');
            next CHAIN;
          }else{
            $c->log->info("Setting to DNA, ");
            $c->type('DNA');
            next CHAIN;
          }
        }    
      }
      if(!$c->type){
        $c->type('undetermined');
        $c->log->info("Setting chain type to an undetermined");
      }
    }    
  }
}
#-------------------------------------------------------------------------------

=head2 _read_remark

PDB "REMARK" records are used to store most of the meta-data for an entry. This
methods decides which REMARK flavours we care about and hands off to other 
private methods to actually parse them.

=cut

sub _read_remark {
  my ( $self, $record ) = @_;

  # get the remark number...
  $record =~ m/^REMARK\s+(\d+)/;
  my $read_method = "_read_remark$1";

  $self->log->debug( "looking for read method |$read_method|..." );
  $self->$read_method( $record ) if $self->can( $read_method );  
}

#-------------------------------------------------------------------------------

=head2 _read_remark350

This method gradually captures the contents of REMARK 350 into a data structure.
The final structure should look something like:

  {
    1 => {
           chains   => ["A", "B"],
           matrices => {
                         1 => [
                                ["1.000000", "0.000000", "0.000000", "0.00000"],
                                ["0.000000", "1.000000", "0.000000", "0.00000"],
                                ["0.000000", "0.000000", "1.000000", "0.00000"],
                              ],
                         2 => [
                                ["-1.000000", "0.000000", "0.000000", "26.41000"],
                                ["0.000000", "-1.000000", "0.000000", "0.00000"],
                                ["0.000000", "0.000000", "1.000000", "65.53000"],
                              ],
                       },
         },
    2 => {
           chains   => ["C", "D"],
           matrices => {
                         1 => [
                                ["-1.000000", "0.000000", "0.000000", "0.00000"],
                                ["0.000000", "1.000000", "0.000000", "33.84500"],
                                ["0.000000", "0.000000", "-1.000000", "65.53000"],
                              ],
                       },
         },
  }

The keys in the outermost hash are the biomolecule numbers, taken straight from
the PDB file. Similarly, the matrix numbers are taken from the PDB file. 
B<Note> that they're hash keys, not array element indices, because biomolecules 
and biomts are numbered starting from 1, not 0.

=cut

sub _read_remark350 {
  my ( $self, $record ) = @_;

  $self->log->debug( 'processing remark 350' );

  # store the raw records
  $self->{remark350} .= $record;

  # keep track of the current biomolecule
  if ( $record =~ m/^REMARK 350 BIOMOLECULE: (\d+)/ ) {
    $self->log->debug( "setting current biomolecule = |$1|" );
    $self->{current_biomolecule} = $1;
  }

  # let's make a shortcut into this monstrous data structure...
  my $biomol;
  if ( defined $self->{current_biomolecule} ) {
    $self->log->debug( 'working on biomolecule |'
                       . $self->{current_biomolecule} . '|' );

    # this is the shortcut...
    $biomol = 
      $self
        ->{chainset}
          ->{biomolecule}
            ->{ $self->{current_biomolecule} };

    # but, in order for the shortcut to work, we need to make sure there's a
    # real hashref at the end of it
    if ( not defined $biomol ) {
      $self
        ->{chainset}
          ->{biomolecule}
            ->{ $self->{current_biomolecule} } = {};
    }
  }
  
  # get the list of chains that make up the current biomolecule
  if ( $record =~ m/^REMARK 350 APPLY THE FOLLOWING TO CHAINS: (.*)/ ) {
    ( my $chains = $1 ) =~ s/^\s*(.*?)\s*$/$1/;
    $self->log->debug( "chains list: |$chains|" );

    my @chains = split /,\s*/, $chains;
    $biomol->{chains} = \@chains;
  }

  # get the raw BIOMT matrices for the current biomolecule
  if ( $record =~ m/^REMARK 350   BIOMT(.*)/ ) {
    
    # split up the record into its constituents
    my @chars = split //, $1;

    # $1 is now something like:
    # 1   1  1.000000  0.000000  0.000000        0.00000
    # 0         1         2         3         4         5
    #  123456789 123456789 123456789 123456789 123456789 

    # NOTE: the PDB records the biomolecule, biomt and matrix line numbers 
    # starting at 1. So that there's no confusion over biomolecule and biomt
    # numbering, we stick with 1-based numbers there, but the matrices
    # themselves are put into arrays, so we make the line numbers zero-based
    # as we store them 
    
    my $line_number   = $chars[0] - 1;
    my $matrix_number = join '', @chars[  1 ..  4 ];
    my $r1            = join '', @chars[  5 .. 14 ];
    my $r2            = join '', @chars[ 15 .. 24 ];
    my $r3            = join '', @chars[ 25 .. 34 ];
    my $t             = join '', @chars[ 35 .. 49 ];

    # trim spaces for all but the line number
    s/^\s*(.*?)\s*$/$1/ for ( $matrix_number, $r1, $r2, $r3, $t );

    $self->log->debug( "line: |$line_number|" );
    $self->log->debug( "matN: |$matrix_number|" ); 
    $self->log->debug( "r1:   |$r1|" ); 
    $self->log->debug( "r2:   |$r2|" ); 
    $self->log->debug( "r3:   |$r3|" ); 
    $self->log->debug( "t:    |$t|" ); 
    
    # store the latest row of the latest matrix...
    $biomol
      ->{matrices}
        ->{ $matrix_number }
          ->[ $line_number ] = [ $r1, $r2, $r3, $t ];
    
    $self->log->debug( "storing matrix |$matrix_number|: ",
      dump(
        $biomol
          ->{matrices}
            ->{ $matrix_number }
      ) );
  }

  $self->log->debug( 'all current biomolecule information: ',
    dump(
      $self->{chainset}->{biomolecule}
    )
  );
  
}

#-------------------------------------------------------------------------------

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
