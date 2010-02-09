
# Chain.pm
# rdf/jt6 20080125 WTSI
#
# $Id: Chain.pm,v 1.10 2008-03-03 13:16:46 rdf Exp $

=head1 NAME

Chain - store chain details

=cut

package Bio::iPfam::Structure::Chain;

=head1 SYNOPSIS

=head1 DESCRIPTION

The Chain object contains the information on a single peptide structure.
This can be derived from the splitting of a chainset, a single peptide
structure or by the splitting of a chain into regions.  

$Id: Chain.pm,v 1.10 2008-03-03 13:16:46 rdf Exp $

=cut

use strict;
use warnings;

use Carp;
use Text::Wrap;

use Data::Dump qw( dump );

use Bio::iPfam::Structure::Atom;
use Bio::iPfam::Structure::Monomer;

use base qw( Bio::iPfam::Structure::Entity );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw( monomers
                               internal_chain_id
                               
                             ) );
__PACKAGE__->mk_ro_accessors( qw( log pfam_chain) );

# logging ! Log early, log often
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 add_monomer

Adds a L<Monomer|Bio::iPfam::Structure::Monomer> to this chain. Returns a 
reference to the added monomer.

  $chain->add_monomer( $monomer );

=cut
  
sub add_monomer {
  my ( $self, $monomer ) = @_;

  unless ( defined $monomer and 
           $monomer->isa( 'Bio::iPfam::Structure::Monomer' ) ) {
    carp q(warning: can't add monomer; not a Monomer object);
    return;
  }

  push @{ $self->{monomers} }, $monomer;
  


  if ( defined $monomer->resSeq ) {  
    # build a "fingerprint" for the monomer and store it in a hash using that
    # as the hash key
    my $fp =   ( $monomer->chainID || ' ' )
             . ( $monomer->resSeq )
             . ( $monomer->iCode || '' ); 
    $self->{monomers_hash}->{$fp} = $monomer;
  }
  else {
    carp q(warning: monomer does not have a sequence number; we will not be able to retrieve this monomer by resSeq);
  }
  
  return $monomer;
}

#-------------------------------------------------------------------------------

=head2 add_pfam_chain

Adds a L<Monomer|Bio::iPfam::Structure::PfamChain> to this chain. Returns a 
reference to the added PfamChain.

  $chain->add_pfam_chain( $pfam_chain);

=cut
  
sub add_pfam_chain {
  my ( $self, $pfc ) = @_;

  unless ( defined $pfc and 
           $pfc->isa( 'Bio::iPfam::Structure::PfamChain' ) ) {
    carp q(warning: can't add pfc; not a pfc object);
    return;
  }

  $self->{pfam_chain} = $pfc;
 
  return $pfc;
}






#-------------------------------------------------------------------------------

=head2 get_monomer

Returns a L<Monomer|Bio::iPfam::Structure::Monomer> based on the identifiers
provided. The argument to this method should be either a hashref, an arrayref
or a scalar. If the argument is a hashref, the hash should contain three 
keys:

=over 4

=item chainID: chain identifier (single character, defaults to ' ')

=item resSeq:  monomer number

=item iCode:   insertion code (non-mandatory)

=back

If the argument is an arrayref, the array should contain the same three values,
in the same order.

If the value is a scalar, it should contain a monomer "fingerprint" constructed
by concatenating these three values in the same order.

Returns the L<Monomer> or undef if that monomer is not found in this chain.

=cut
  
sub get_monomer {
  my ( $self, $identifier ) = @_;
  
  my $fp;
  if ( ref $identifier eq 'HASH' ) {
    $self->log->debug( 'retrieving a monomer using a hashref' );
    $fp =   ( $identifier->{chainID} || ' ' )
          . ( $identifier->{resSeq} )
          . ( $identifier->{iCode} );
  }
  elsif( ref $identifier eq 'ARRAY' ) {
    $self->log->debug( 'retrieving a monomer using a arrayref' );
    $fp =   ( $identifier->[0] || ' ' )
          . ( $identifier->[1] )
          . ( $identifier->[2] || '' );
  }
  elsif( not ref $identifier ) {
    $self->log->debug( 'retrieving a monomer using a fingerprint' );
    $fp = $identifier;
  } 

  unless ( defined $fp ) {
    carp q(warning: can't build a monomer monomer fingerprint);
    return;
  }

  return $self->{monomers_hash}->{$fp};
}

#-------------------------------------------------------------------------------

=head2 delete_monomer

Deletes a L<Monomer|Bio::iPfam::Structure::Monomer> to this chain. Returns a 
reference to the deleted monomer or undef if the monomer that was handed in
was not part of this chain.

  $chain->delete_monomer( $monomer );

=cut
  
sub delete_monomer {
  my ( $self, $monomer ) = @_;

  unless ( defined $monomer and 
           $monomer->isa( 'Bio::iPfam::Structure::Monomer' ) ) {
    carp q(warning: can't add monomer; not a Monomer object);
    return;
  }

  my $fp =   ( $monomer->chainID || ' ' )
           . ( $monomer->resSeq )
           . ( $monomer->iCode || '' ); 
  delete $self->{monomers_hash}->{$fp};

  for ( my $i = 0; $i < scalar @{ $self->{monomers} }; $i++ ) {
    return splice( @{ $self->{monomers} }, $i, 1 )
      if $monomer eq $self->{monomers}->[$i];
  }
  
  return undef;
}

#-------------------------------------------------------------------------------

=head2 chainID

If an argument is given, it's expected to be a single letter chain identifier,
which is set as the new chain ID for this Chain and for all constituent 
L<Monomers|Bio::iPfam::Structure::Monomer>. If setting succeeds, this method
returns the new chain ID, returns undef otherwise. Issues a warning if the 
chain ID was not valid.

=cut
  
sub chainID {
  my ( $self, $chainID ) = @_;

  if ( defined $chainID ) {

    unless ( length $chainID == 1 ) {
      carp "warning: not a valid chain ID ($chainID)";
      return undef;
    }
    
    $self->log->debug( "setting chain ID to |$chainID|" );
    $self->{chainID} = $chainID;

    foreach my $monomer ( @{ $self->{monomers} } ) {
      $monomer->chainID( $chainID );
    }

  }

  return $self->{chainID} || undef;
}

#-------------------------------------------------------------------------------

=head2 internal_chain_id 

Get/set the internal chain name. This is required for, for example, large 
assemblies such as viruses, where we run out of simple, single character
chain identifiers. The internal ID can be an arbitrarily complex string.
  
=cut

# auto-generated by mk_accessors

#-------------------------------------------------------------------------------

=head2 type

Get/set the chain type. Restricted to one of: protein, DNA, RNA, model,
undetermined.

  $chain->type( 'protein' );
  $chain_type = $chain->type;

=cut

sub type {
  my ( $self, $type ) = @_;

  if ( defined $type ) {
    if ( defined $Bio::iPfam::Structure::Entity::allowed_chain_types->{$type} ) {
      $self->{type} = $type;
    }
    else {
      carp 'warning: not an allowed chain type, '.$type;
    }
  }
  
  my $retval;
  $retval = $self->{type} if defined $self->{type};
  
  return $retval;
}

#-------------------------------------------------------------------------------

=head2 is_region

Get/set the property of the chain that shows whether it is a sub-region of a
larger chain.

  $chain->is_region( 1 );
  $is_region = $chain->is_region;

=cut

# auto-generated by mk_accessors

#-------------------------------------------------------------------------------

=head2 write

Write the constituent Monomers in PDB format. If a filehandle is supplied, 
writes to that, otherwise writes to STDOUT. No return value.

  open( PDBFILE, ">file.pdb" );
  $chain->write( \*PDBFILE );

=cut

sub write {
  my ( $self, $fh ) = @_;

  # dump to STDOUT unless we get a different filehandle
  $fh ||= *STDOUT;

  foreach my $monomer ( @{ $self->monomers } ) {
    $self->log->debug( 'writing monomer |' . ( $monomer->chainID || 'chain?' ) 
                                           . ( $monomer->resSeq || 'resSeq?' )
                                           . ( $monomer->iCode || 'iCode?' ) . '|' );
    $monomer->write( $fh );
  }
  print $fh 'TER', ' ' x 77, "\n";

  return undef;
}

#-------------------------------------------------------------------------------

=head2 atom_count

Returns the number of atoms that are currently part of this Chain. This will 
be 0 if there are no atoms.

  $atom_count = $chain->atom_count;

=cut

sub atom_count {
  my $self = shift;
  
  my $count = 0;
  foreach my $monomer ( @{ $self->{monomers} } ) {
    $count += $monomer->atom_count;
  }
  
  return $count;
}

#-------------------------------------------------------------------------------

=head2 monomer_count

Returns the number of monomers that are currently part of this Chain. This will 
be 0 if there are no monomers.

  $monomer_count = $chain->monomer_count;

=cut

sub monomer_count {
  my $self = shift;
  return scalar @{ $self->{monomers} };
}

#-------------------------------------------------------------------------------

=head2 transform

Transform the L<Atoms|Bio::iPfam::Structure::Atom> in this Chain according to
the supplied matrix. If the transformation succeeds, the method returns this 
Chain, or undef if there's a problem with the transformation matrix. The matrix 
should be of the form:

  [ [ r11, r12, r13, t1 ],
    [ r21, r22, r23, t2 ],
    [ r31, r32, r33, t3 ] ]

=cut

sub transform {
  my ( $self, $matrix ) = @_;
  
  foreach my $monomer ( @{ $self->monomers } ) {
    return undef unless $monomer->transform( $matrix );
  }

  return $self;
}

#-------------------------------------------------------------------------------

=head2 chain2fasta

Writes out the sequences of the chain in FASTA format, 60 chars to a line.
It does not care what the chain is, nucleic acid or protein. If the argument is 
a reference to a filehandle, there is no return value, but the sequence is 
written to the filehandle. If there is no argument, the return value is the
FASTA-format sequence.

  $chain->chain2fasta( \*FILEHANDLE );
  $sequence = $chain->chain2fasta;

=cut

sub chain2fasta {
  my ( $self, $fasta_out ) = @_;

  # get a shorter handle on the mapping data structure...
  my $mapping = $Bio::iPfam::Structure::Entity::one_letter_mapping;

  my $sequence = '';
  foreach my $monomer ( @{ $self->monomers } ) {
    $self->log->debug("Doing look up for ".$monomer->resName);
    $sequence .= $mapping->{ $monomer->resName } || 'X';
  }
 
  if( $fasta_out ) {
    $Text::Wrap::columns = 60;
    print $fasta_out '>Chain ' . $self->chainID . "\n";
    print $fasta_out wrap( '', '', "$sequence\n" );
  }
  else {
    return $sequence;
  }

  return undef;
}

#-------------------------------------------------------------------------------
#- private methods -------------------------------------------------------------
#-------------------------------------------------------------------------------

=head2 _parse_record

Private method. Overrides default version from 
L<Entity|Bio::iPfam::Structure::Entity> so that we can add some atom-specific
items.

=cut

sub _parse_record {
  my ( $self, $record ) = @_;

  # the overridden parse_record method will return undef unless it successfully
  # parses the record
  return unless $self->SUPER::_parse_record( $record );
    
  # by default, set the internal chain ID to show that this is a chain from the 
  # asymmetric unit. When generating a new Chain by applying a rotation matrix
  # such as a biomolecule BIOMT, this will be over-written
  $self->internal_chain_id( 'asym.' . $self->chainID . '.1.1' ); 
  
  return $self; 
}

#-------------------------------------------------------------------------------
#- stuff awaiting cargo culting ------------------------------------------------
#-------------------------------------------------------------------------------

#=head2 contact
#
#Estimates which monomers of Chain B are in contact with Chain A.  Calculates 
#a CA::CA distance between amino acids.  This method returns a list of monomers
#from Chain B that are within 10A of Chain A. 
#
#@list = $chainA->contact(chainB);
#
#To alter the 10A contact, give it as an extra variable.
#
#@list = $chainA->contact(chainB, 'contact distance e.g. 7');
#
#=cut
#
#
#sub contact {
#  
#  my $self = shift;
#  my $chain2 = shift;
#  my $contact_distance = shift;
#  my $n = 0; 
#  my @contact_monomers;
#  if (!defined $contact_distance){
#    $contact_distance = 10.0;
#  }
#  foreach my $monomer1 (@{$self->each}){
#    foreach my $monomer2 (@{$chain2->each}){
#      my $distance = $monomer1->distance($monomer2);
#        if (($distance <= $contact_distance) && ($distance > 0)){
#          #print  "CONTACT\n";
#          #print "\n".$monomer1->monomer_no()." of "
#          #.$self->chain_id()." is ".$distance." A away".
#          #" from ".$monomer2->monomer_no()." of "
#          #.$chain2->chain_id()."\n";
#          my @data = ($monomer1, $monomer2);
#          push (@contact_monomers, \@data);
#          $n++;
#          }
#      }
#    }
#  
#  if ($n > 0){
#    return @contact_monomers;
#  }
#}

##################
## 2ry_structure #
##################
#
#=head2 2ry_structure
#
#write out sequences with structural mask 
#
#
#=cut
#
#sub transform_chain{
#  my $self = shift;
#  my $trans = shift;
#  foreach my $monomer (@{$self->each}){
#    $monomer->transform_monomer($trans);
#  }
#}
#
#=head2 _guess_alphabet
#
#  Title    : _guess_alphabet
#  Usage    : $chain->_guess_alphabet 
#  Function : This method tries to work out whether we are dealing with DNA, RNA or proteins.
#           : Once it has made it's guess, it sets the primary atom for each Monomer or Base
#  Args     : None
#  Returns  : Nothing
#  
#=cut
#
#sub _guess_alphabet {
#  my $self = shift;
#  my $str = $self->chain2fasta();
#  my $type= "unknown";
#  
#  my $total = CORE::length($str);
#  if( $total == 0 ) {
#    $self->throw("Got a sequence with no letters in - ".
#     "cannot guess alphabet [$str]");
#  }
#  $logger->debug("is it DNA?");
#  my $u = ($str =~ tr/Uu/G/);
#  my $atgc = ($str =~ tr/ATGCNatgcn//);
#   
#  if( ($atgc / $total) > 0.85 ) {
#    if(!$u){
#      $type = 'DNA';
#      #Okay, could still be RNA, look for O2*
#    BASE:
#      foreach my $base (@{$self->each}){
#       foreach my $atom (@{$base->each}){
#         if ($atom->type =~ /O2\*/){
#           print "ATOM".$atom->type."\n";
#           $type = "RNA";
#           last BASE;
#         }
#       }
#      }
#    }else{
#      $type = 'RNA';
#    }
#  }else{
#    $type = "protein";
#  }
#  $self->type($type);
#  $logger->debug("Set type to $type");
#  if ($type eq 'protein'){
#    foreach my $res (@{$self->each}){
#      foreach my $atom (@{$res->each}){
#         if ($atom->type eq "CA"){
#           $atom->primary(1);
#         }else{
#           $atom->primary(0); 
#         }
#      }
#    }
#  }else{
#    foreach my $mono (@{$self->each}){
#      my $pset = 1;
#      foreach my $atom (@{$mono->each}){
#        if($pset){
#          $atom->primary(1);
#        }else{
#           $atom->primary(0); 
#        }
#      }
#    }
#  }
#  $logger->debug("Set primaries");
#}

#-------------------------------------------------------------------------------

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

