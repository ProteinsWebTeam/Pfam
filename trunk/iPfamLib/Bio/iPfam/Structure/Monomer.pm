
# Monomer.pm
# rdf/jt6 20080131 WTSI
#
# $Id: Monomer.pm,v 1.5 2008-03-03 13:16:46 rdf Exp $

=head1 NAME

Monomer - a base class for the components of a polymer chain

=cut

package Bio::iPfam::Structure::Monomer;

=head1 SYNOPSIS


=head1 DESCRIPTION


$Id: Monomer.pm,v 1.5 2008-03-03 13:16:46 rdf Exp $

=cut

use strict;
use warnings;

use Carp;

use Bio::iPfam::Structure::Atom;

use base qw( Bio::iPfam::Structure::Entity );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw( resName
                               chainID
                               resSeq
                               iCode
                               atoms
                               modified
                               primary  ) );
__PACKAGE__->mk_ro_accessors( qw( log ) );

# logging ! One of those natural laws
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 add_atom

Add an L<Atom|Bio::iPfam::Structure::Atom> to the list of Atoms in this 
monomer. Returns a reference to the added atom.

  $monomer->add_atom( $atom );

If an atom with the same name already exists in this monomer a warning is
issued, but the new L<Atom|Bio::iPfam::Structure::Atom> is added anyway.

=cut

sub add_atom {
  my ( $self, $atom ) = @_;

  unless ( defined $atom and 
           $atom->isa( 'Bio::iPfam::Structure::Atom' ) ) {
    carp q(warning: can't add atom; not an Atom object);
    return;
  }
  
  # we'll warn if the atom already exists and refuse to add it
  if ( $self->{atoms_hash}->{$atom->realName} ) {
    carp 'warning: atom "' . $atom->realName . '" already exists in this monomer';
    return;
  }

  push @{ $self->{atoms} }, $atom;

  if ( defined $atom->realName ) {
    $self->{atoms_hash}->{$atom->realName} = $atom;
  }
  else {
    carp q(warning: atom does not have a name; we will not be able to retrieve this atom by name);
  }

  return $atom;  
}

#-------------------------------------------------------------------------------

=head2 get_atom

Returns the L<Atom|Bio::iPfam::Structure::Atom> with the specified name. Returns
undef if no atom name is supplied or if there is no Atom with that name in this
monomer.

  $atom_N = $monomer->get_atom( 'N' );

=cut

sub get_atom {
  my ( $self, $atom_name ) = @_;

  unless ( defined $atom_name ) {
    carp q(warning: no atom name given);
    return;
  }

  return $self->{atoms_hash}->{$atom_name};
}

#-------------------------------------------------------------------------------

=head2 delete_atom

Deletes an L<Atom|Bio::iPfam::Structure::Atom> from the list of Atoms in this 
monomer and returns the deleted Atom. This method accepts either an Atom
object or the name of the atom to delete. Returns a reference to the deleted
Atom object, or undef if no Atom was deleted

  $deleted_atom_object = $monomer->delete_atom( $atom_object );
  $deleted_atom_object = $monomer->delete_atom( ' N  ' );

=cut

sub delete_atom {
  my ( $self, $atom ) = @_;

  unless ( defined $atom ) {
    carp q(warning: can't delete atom; no Atom details supplied);
    return undef;
  }

  unless ( $atom->isa( 'Bio::iPfam::Structure::Atom' ) ) { 
    $atom = $self->get_atom( $atom );
  }

  unless ( defined $atom and
           $atom->isa( 'Bio::iPfam::Structure::Atom' ) ) {
    carp q(warning: can't delete atom; not an Atom object);
    return undef;
  }

  delete $self->{atoms_hash}->{$atom->realName};

  for ( my $i = 0; $i < scalar @{ $self->{atoms} }; $i++ ) {
    return splice( @{ $self->{atoms} }, $i, 1 )
      if $atom eq $self->{atoms}->[$i];
  }

  return undef;
}

#-------------------------------------------------------------------------------

=head2 atom_count

Returns the number of atoms that are currently part of this monomer:
This will be 0 if there are no atoms.

  $atom_count = $monomer->atom_count;

=cut

sub atom_count {
  my $self = shift;
  return scalar @{ $self->{atoms} };
}

#-------------------------------------------------------------------------------

=head2 chainID

If an argument is given, it's expected to be a single letter chain identifier,
which is set as the new chain ID for this Monomer and for all constituent 
L<Atoms|Bio::iPfam::Structure::Atom>. If setting succeeds, this method
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

    $self->{chainID} = $chainID;

    foreach my $atom ( @{ $self->{atoms} } ) {
      $atom->chainID( $chainID );
    }

  }

  return $self->{chainID} || undef;
}

#-------------------------------------------------------------------------------

=head2 write 

Write the constituent Atoms in PDB format. If a filehandle is supplied, writes
to that, otherwise writes to STDOUT. No return value.

  open( PDBFILE, ">file.pdb" );
  $monomer->write( \*PDBFILE );

=cut

sub write {
  my ( $self, $fh ) = @_;
  
  # dump to STDOUT unless we get a different filehandle
  $fh ||= *STDOUT;

  foreach my $atom ( @{ $self->atoms } ) {
    $self->log->debug( 'writing atom number |' . $atom->serial . '|' );
    $atom->write( $fh );
  }

#  $_->write( $fh ) for @{ $self->atoms };
}

#-------------------------------------------------------------------------------

=head2 primary

Takes a monomers object and returns the primary atom from the list of atoms 
objects contained in the monomer object. If there is no primary atom, returns
undef.

  $monomer->primary();

=cut

sub get_primary_atom {
  my $self = shift;
  return $self->primary;
}

#-------------------------------------------------------------------------------

=head2 distance

Calaculates the approximate distance between the two monomers by calculating 
the distance between the two primary atoms.

=cut

sub distance {
  my ( $self, $other_res ) = @_;
  
  unless ( defined $other_res and 
           $other_res->isa( 'Bio::iPfam::Structure::Monomer' ) ) {
    carp q(warning: can't calculate distance; not a Monomer object);
    return;
  }
  
  my $primary_atom_1 = $self->get_primary_atom;
  my $primary_atom_2 = $other_res->get_primary_atom;
  
  unless ( defined $primary_atom_1 and 
           defined $primary_atom_2 ) {
    carp q(warning: couldn't get both primary atoms);
    return;
  }

  return $primary_atom_1->distance( $primary_atom_2 );
}

#-------------------------------------------------------------------------------

=head2 transform

Transform the L<Atoms|Bio::iPfam::Structure::Atom> in this Monomer according to
the supplied matrix. If the transformation succeeds, the method returns this 
monomer, or undef if there's a problem with the transformation matrix. The 
matrix should be of the form:

  [ [ r11, r12, r13, t1 ],
    [ r21, r22, r23, t2 ],
    [ r31, r32, r33, t3 ] ]

=cut

sub transform {
  my ( $self, $matrix ) = @_;
  
  foreach my $atom ( @{ $self->atoms } ) {
    return undef unless $atom->transform( $matrix );
  }

  return $self;
}

#-------------------------------------------------------------------------------

sub get_carbon{	
  my($self, $no) = @_;
  my $c;
  my %atoms = map{$_->serial => $_ }@{$self->atoms};
  if($atoms{($no-1)} and $atoms{($no-1)}->realName =~ /C/i){
    $c = $atoms{($no-1)};
  }elsif($atoms{($no+11)} and $atoms{($no+1)}->realName =~ /C|O/i){
    $c = $atoms{($no+1)};
  }elsif($atoms{($no-2)} and $atoms{($no-2)}->realName =~ /C/i){
    $c = $atoms{($no-2)};
  } 
 
  if(!$c){
    $self->log->debug(" Got no carbon $no, $self"); 
  }
  return ($c);
}


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
