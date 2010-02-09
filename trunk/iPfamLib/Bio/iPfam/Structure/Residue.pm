
# Residue.pm
# rdf/jt6 20080124 WTSI
#
# $Id: Residue.pm,v 1.8 2008-02-01 16:25:39 rdf Exp $

=head1 NAME

Residue - store residue details

=cut

package Bio::iPfam::Structure::Residue;

=head1 SYNOPSIS

  use Bio::iPfam::Structure::Residue;

  $residue = Bio::iPfam::Structure::Residue->new;
  $residue = Bio::iPfam::Structure::Residue->new( $atom_record );

  use Bio::iPfam::Structure::Atom;
  $atom = Bio::iPfam::Structure::Atom->new( $atom_record );
  $residue->add_atom( $atom );

  $deleted_atom = $residue->delete_atom( $atom );

  $atom_count = $residue->atom_count;

  $residue->write; # print ATOM record to STDOUT
  $residue->write( \*FILEHANDLE );

  # residue property getters and setters
  $residue->resName( 'ALA' );
  $residue_name = $residue->resName;

  $residue->chainID( 'A' );
  $chain_id = $residue->chainID;

  $residue->resSeq( 123 );
  $residue_number = $residue->resSeq;

  $residue->iCode( 'A' );
  $insertion_code = $residue->iCode;

  $residue->secondary( 'E' );
  $secondary_struction = $residue->secondary;

  $residue->modified( 1 );
  $is_modified = $residue->modified;

  $primary_atom = $residue->get_primary_atom;

=head1 DESCRIPTION

An object to store residue information. Essentially a bag of 
L<Atoms|Bio::iPfam::Structure::Atom>.

$Id: Residue.pm,v 1.8 2008-02-01 16:25:39 rdf Exp $

=cut

use strict;
use warnings;

use Carp;
use Math::Trig qw( :pi );

use Bio::iPfam::Structure::Atom;

use base qw( Bio::iPfam::Structure::Monomer );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw( secondary ) );

# logging ! Who could ask for anything more ?
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 dihedral;
 
Calculates the phi, psi and omega dihedrals.
 
Phi is defined as the dihedral angle composed of the four atoms C(i-1) -
N(i) - CA(i) - C(i). Psi is composed of the four atoms N(i) - CA(i) - C(i)
- N(i+1). Omega is composed of the four atoms across the peptide bond CA(i) -
C(i) - N(i+1) - CA(i+1). The magnitude and sign are calculated by the 
cross products.
 
    N - CA
   /     \
  C       C
 
With all atoms in a plane N-C X N-CA is identical in direction to CA-N X
CA-C, so the dihedral angle (the angle between the cross products) is 0.
 
           C
         /
    N - CA
   /
  C
 
With all atoms in a plane N-C X N-CA is opposite in direction to CA-N X
CA-C, so the dihedral angle is 180.
 
A positive rotation is defined by the right hand rule. With your right
thumb pointing along the central bond, a positive rotation is in the
direction of your curled fingers. For example, from the first figure in
which Phi is 0, rotating the right C down (into the screen) is positive.
Rotating it up (out of the screen) is negative. Remember that a positive
rotation of 10 degrees from 175 is -175.

Returns an array of phi, psi and omega.
 
  @phi_psi_omega_angles = $residue->dihedral('residue_i-1', 'residue_i+1');
 
=cut
 
sub dihedral {
  my ( $self, $residue1, $residue3 ) = @_;
  
  unless(defined $residue1 and $residue1->isa( 'Bio::iPfam::Structure::Residue' ) ) {
    carp qq(warning: can't calculate dihedral angle; residue 1 is not a Residue object);
    return;
  }
  unless(defined $residue3 and $residue3->isa( 'Bio::iPfam::Structure::Residue' ) ) {
    carp qq(warning: can't calculate dihedral angle; residue 2 is not a Residue object);
    return;
  }
  
  #setup phi atoms c-n-ca-c, psi atoms n-ca-c-n, omega atoms, ca-c-n-ca
  my (@phiAtoms, @psiAtoms, @w);
  foreach my $a ($residue1->atoms){
    if($a->realName eq "C"){
      $phiAtoms[0] = $a;
      last;
    } 
  }
  
  foreach my $a ($self->atoms){
     if ($a->realName eq "N"){
       $phiAtoms[1] = $a;
       $psiAtoms[0] = $a;
     }elsif ($a->realName eq "CA"){
       $phiAtoms[2] = $a;
       $psiAtoms[1] = $a;
       $w[0] = $a;
     }elsif($a->realName eq "C"){
       $phiAtoms[3] = $a;
       $psiAtoms[2] = $a;
       $w[1] = $a;
     }
  }
  
  foreach my $a ($residue3->atoms){
    if($a->realName  eq "N"){
       $psiAtoms[3] = $a;
       $w[2] =  $a;
    }elsif ($a->realName eq 'CA'){
       $w[3]= $a;
    }
  }
 
  my ( @ppw, @xyz1, @xyz2, @xyz3, @xyz4);
  
  foreach my $atoms (\@phiAtoms, \@psiAtoms,  \@w){
   
    my $dx12 = $atoms->[0]->x - $atoms->[1]->x;
    my $dy12 = $atoms->[0]->y - $atoms->[1]->y;
    my $dz12 = $atoms->[0]->z - $atoms->[1]->z; 
     
    my $dx23 = $atoms->[1]->x - $atoms->[2]->x;
    my $dy23 = $atoms->[1]->y - $atoms->[2]->y;
    my $dz23 = $atoms->[1]->z - $atoms->[2]->z;
 
    my $dx43 = $atoms->[3]->x - $atoms->[2]->x;
    my $dy43 = $atoms->[3]->y - $atoms->[2]->y;
    my $dz43 = $atoms->[3]->z - $atoms->[2]->z;
 
    my $px1 = $dy12*$dz23-$dy23*$dz12;
    my $py1 = $dz12*$dx23-$dz23*$dx12;
    my $pz1 = $dx12*$dy23-$dx23*$dy12;
      
    my $np1 = sqrt($px1*$px1+$py1*$py1+$pz1*$pz1);
 
    $px1 /= $np1;
    $py1 /= $np1;
    $pz1 /= $np1;
 
    my $px2=$dy43*$dz23-$dy23*$dz43;
    my $py2=$dz43*$dx23-$dz23*$dx43;
    my $pz2=$dx43*$dy23-$dx23*$dy43;
     
    my $np2=sqrt($px2*$px2+$py2*$py2+$pz2*$pz2);
  
    $px2/=$np2;
    $py2/=$np2;
    $pz2/=$np2;
   
    my $dp12=$px1*$px2+$py1*$py2+$pz1*$pz2;
 
    my $angle= pip2 - atan2($dp12,sqrt(1.0-$dp12*$dp12));
 
    my $px3=$py1*$pz2-$py2*$pz1;
    my $py3=$pz1*$px2-$pz2*$px1;
    my $pz3=$px1*$py2-$px2*$py1;
     
    my $dp233=$px3*$dx23+$py3*$dy23+$pz3*$dz23;
   
    if ($dp233>0.0) {
      $angle=-$angle;
    }
 
    my $angle_d = rad2deg($angle);
    push (@ppw, $angle_d);
  }
  return @ppw;
}

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
