
# Atom.pm
# rdf/jt6 20080124 WTSI
#
# $Id: Atom.pm,v 1.15 2008-03-03 13:16:46 rdf Exp $

=head1 NAME

Atom - store atom details

=cut

package Bio::iPfam::Structure::Atom;

=head1 SYNOPSIS

  use Bio::iPfam::Structure::Atom;

  $atom = Bio::iPfam::Structure::Atom->new();
  $atom = Bio::iPfam::Structure::Atom->new( $atom_record );

  # atom name including PDB column formatting
  $atom->name( ' N  ' );
  $name_with_spaces = $atom->name;

  # atom name without formatting
  $atom->realName( 'N' );
  $name_without_spaces = $atom_realName;

  $atom->xyz( 1.0, 2.0, 3.0 );
  @xyz = $atom->xyz;

  $atom->primary( 1 );
  $is_primary = $atom->primary;

  # atom property getters and setters
  $atom->serial( 456 );
  $atom_number = $atom->serial;

  $atom->altLoc( 'A' );
  $alternate_location = $atom->altLoc;

  $atom->resName( 'ALA' );
  $residue_name = $atom->resName;

  $atom->chainID( 'A' );
  $chain_id = $atom->chainID;

  $atom->resSeq( 123 );
  $residue_number = $atom->resSeq;

  $atom->iCode( 'A' );
  $insertion_code = $atom->iCode;

  $atom->x( 123.456 );
  $x_coordinate = $atom->x;

  $atom->y( 234.567 );
  $y_coordinate = $atom->y;

  $atom->x( 345.678 );
  $z_coordinate = $atom->z;

  $atom->occupancy( 0.5 );
  $occupancy = $atom->occupancy;

  $atom->tempFactor( 20.00 );
  $temperature_factor = $atom->tempFactor;

  $atom->element( 'N' );
  $element = $atom->element;

  $atom->charge( '+1' );
  $charge = $atom->charge;

=head1 DESCRIPTION

An object to store information about an atom. Handles all of the information
that can be supplied in a PDB ATOM record. 

$Id: Atom.pm,v 1.15 2008-03-03 13:16:46 rdf Exp $

=cut

use strict;
use warnings;

use Math::Trig;
use Carp;

use base qw( Bio::iPfam::Structure::Entity );

# getters and setters for simple object properties
__PACKAGE__->mk_accessors( qw( hetatm
                               realName
                               primary
                               serial     
                               name       
                               altLoc     
                               resName    
                               chainID    
                               resSeq     
                               iCode      
                               x          
                               y          
                               z          
                               occupancy  
                               tempFactor 
                               element    
                               charge ) );
__PACKAGE__->mk_ro_accessors( qw( log ) );

# logging, lovely logging
use Log::Log4perl qw( get_logger );

#-------------------------------------------------------------------------------

=head1 ATOM record format

The PDB ATOM record is described by the PDB specification as follows:

          1         2         3         4         5         6         7         8
 12345678901234567890123456789012345678901234567890123456789012345678901234567890
  ATOM    145  N   VAL A  25      32.433  16.336  57.540  1.00 11.92           N  
 
 COLUMNS      DATA TYPE        FIELD      DEFINITION
 ------------------------------------------------------
  1 -  6      Record name      "ATOM    "
  7 - 11      Integer          serial     Atom serial number.
 13 - 16      Atom             name       Atom name.
 17           Character        altLoc     Alternate location indicator.
 18 - 20      Residue name     resName    Residue name.
 22           Character        chainID    Chain identifier.
 23 - 26      Integer          resSeq     Residue sequence number.
 27           AChar            iCode      Code for insertion of residues.
 31 - 38      Real(8.3)        x          Orthogonal coordinates for X in 
                                          Angstroms
 39 - 46      Real(8.3)        y          Orthogonal coordinates for Y in 
                                          Angstroms
 47 - 54      Real(8.3)        z          Orthogonal coordinates for Z in 
                                          Angstroms
 55 - 60      Real(6.2)        occupancy  Occupancy.
 61 - 66      Real(6.2)        tempFactor Temperature factor.
 77 - 78      LString(2)       element    Element symbol, right-justified.
 79 - 80      LString(2)       charge     Charge on the atom.

B<NOTE>: column numbers in this description are one-based, not zero-based 
(i.e. the first column is column 1, not 0) as is usual in perl

The PDB format allows four characters for the atom name, but the first column
is commonly left blank, so that the name begins in the second column. This
has implications for the final output format of the atom when setting the 
atom name. See L<name|Bio::iPfam::Structure::Atom::name>.

=cut

#------------------------------------------------------------------------------- 

=head1 METHODS

=head2 name

The PDB specification allows four columns for atom names, but there are some
rules about which column the name starts in:

=over 4

=item a single-letter atom name is placed in the second of those four columns, 
like "_N__"

=item longer names are left justified relative to that column, e.g. "_CA_" or 
"_NH2"

=item a few atom names are specifically shifted to the left, e.g. "MG__"

=back

In order to accommodate these different placements, we store both the atom name,
which contains any spaces that were present when the name was set, and the 
real name, which is stripped of spaces. 

The "set" method for atom name assumes that, if the supplied value contains 
spaces, it is a "name". This method stores both the name and the real name:

  $atom->name( ' N  ' );
  $real_name = $atom->name;   # ' N  '
  $atom->realName( 'N' );
  $name = $atom->name;        # 'N'

If the supplied atom name does not contain spaces, the setter assumes that the 
atom name should start in the second column and warns the user. In this case,
the real name is exactly what the user supplied.

=cut

sub name {
  my( $self, $name ) = @_;

  return $self->{name} || '' unless( defined $name and
                                     $name ne '' );
  
  if ( defined $name ) {
    if ( $name eq '' ) {
      carp q(warning: can't set atom name with an empty string);
    }
    elsif ( $name =~ m/\s+/ ) {
      $self->log->debug( "got an atom name with spaces: |$name|" );
      $self->{name} = $name;
      ( $self->{realName} ) = $name =~ m/^\s*(.*?)\s*$/;
    }
    elsif ( length $name == 4 ) {
      $self->log->debug( "got a 4-character atom name: |$name|" );
      $self->{name} = $self->{realName} = $name;
    }
    else {
      $self->log->debug( "got an atom name with NO spaces: |$name|" );
      carp 'warning: setting a "raw" atom name; placing in column 14 of ATOM record';
      $self->{name}     = substr( " $name   ", 0, 4 ); # trim to only 4 chars
      $self->{realName} = $name;
    }
  }

  return $self->{name} || '';
}

#-------------------------------------------------------------------------------

=head2 xyz

Gets/sets all atomic coordinates. If setting, all three values (X, Y and Z)
must be specified. Warns if less than three coordinates are given.

  $atom->xyz( $x, $y, $z );
  $atom->xyz( 1.0, 2.0, 3.0 );

=cut   

sub xyz {
  my $self = shift;
  if ( scalar @_ ) {
    if ( scalar @_ == 3 ) {
      $self->x( shift );
      $self->y( shift );
      $self->z( shift );
    } else {
      carp q(warning: can't set atom coordinates; must supply X-, Y- and Z-coords);
    }
  }
  return ( $self->x, $self->y, $self->z );
}

#-------------------------------------------------------------------------------

=head2 write

Write this atom as a PDB "ATOM" record. If a filehandle is supplied, writes to
that, otherwise writes to STDOUT.

  open( PDBFILE, '>file.pdb' );
  $atom->write( \*PDBFILE );

=cut

sub write {
  my ( $self, $fh ) = @_;

  # dump to STDOUT unless we get a different filehandle
  $fh ||= *STDOUT;
  #Directly access the object as this is quite time consuming.
  print  $fh $self->hetatm ?   'HETATM' : 'ATOM  '; # record name
  printf $fh '%5d ',            $self->{serial};      # serial number
  printf $fh '%4s',             $self->{name};        # atom name
  printf $fh '%1s',             $self->{altLoc};      # alternate location
  printf $fh '%3s',             $self->{resName};     # residue name
  printf $fh ' %1s',            $self->{chainID};     # chain ID
  printf $fh '%4d',             $self->{resSeq};      # residue number
  printf $fh '%1s   ',          $self->{iCode};       # insertion code
  printf $fh '%8.3f',           $self->{x};           # \
  printf $fh '%8.3f',           $self->{y};           #  } coordinates
  printf $fh '%8.3f',           $self->{z};           # /
  printf $fh '%6.2f',           $self->{occupancy};   # occupancy
  printf $fh '%6.2f          ', $self->{tempFactor};  # B-factor
  printf $fh '%2s',             $self->{element};     # element symbol
  printf $fh '%2s',             $self->{charge};      # element symbol
  print  $fh "\n";
   
}

#-------------------------------------------------------------------------------

=head2 distance

Calculates the distance between this L<Atom|Bio::iPfam::Structure::Atom> and 
the supplied Atom object.

  $atom1->distance( $atom2 );

=cut

sub distance {
  my ( $self, $atom2 ) = @_;
  my $dx = $self->{x} - $atom2->{x};
  my $dy = $self->{y} - $atom2->{y};
  my $dz = $self->{z} - $atom2->{z};
  return sqrt( $dx*$dx + $dy*$dy + $dz*$dz );
}

#-------------------------------------------------------------------------------

=head2 angle

Calculation of the angle between three L<Atoms|Bio::iPfam::Structure::Atom>:

  2   3
   \ /
    1

Returns the angle in degrees.

  $angle = $atom1->angle( $atom2, $atom3 );

=cut

sub angle {
  my $self = shift;
  my ( $atom2, $atom3 ) = @_;

  # if any of the point exist in a negtaive dimension, reset the relative axis.
  
  my $d12 = $self->distance( $atom2 );
  my $d13 = $self->distance( $atom3 );
  my $d23 = $atom2->distance( $atom3 );
  
  my $angle = acos( ( $d12*$d12 + $d13*$d13 - $d23*$d23 ) / ( 2 * $d12*$d13 ) );

  # convert to degrees on Math::Trig
  return rad2deg( $angle );
}  
  
#-------------------------------------------------------------------------------

=head2 transform

Transform the atomic coordinates by the supplied matrix. If the transformation
succeeds, the method returns this atom, or undef if there's a problem with the 
transformation matrix. The matrix should be of the form:

  [ [ r11, r12, r13, t1 ],
    [ r21, r22, r23, t2 ],
    [ r31, r32, r33, t3 ] ]

=cut

sub transform {
  my ( $self, $trans ) = @_;
  
  # check that the matrix is properly defined
  for ( my $i = 0; $i <= 2; $i++ ) {
    for ( my $j = 0; $j <= 3; $j++ ) {
      unless ( defined $trans->[$i]->[$j] ) {
        carp "warning: transformation matrix incomplete; found no value at $i, $j in the array";
        return undef;
      }
    } 
  }

  # now apply the transformation to the coordinates    
  my $new_x = sprintf '%.3f',   ( $trans->[0]->[0] * $self->{x} ) 
                              + ( $trans->[0]->[1] * $self->{y} )
                              + ( $trans->[0]->[2] * $self->{z} ) 
                              +   $trans->[0]->[3];
  my $new_y = sprintf '%.3f',   ( $trans->[1]->[0] * $self->{x} )
                              + ( $trans->[1]->[1] * $self->{y} )
                              + ( $trans->[1]->[2] * $self->{z} )
                              +   $trans->[1]->[3];
  my $new_z = sprintf '%.3f',   ( $trans->[2]->[0] * $self->{x} )
                              + ( $trans->[2]->[1] * $self->{y} )
                              + ( $trans->[2]->[2] * $self->{z} )
                              +   $trans->[2]->[3];
  
  $self->xyz( $new_x, $new_y, $new_z );
  
  return $self;
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

  # flag HETATMs and default to setting this as a non-primary atom
  $self->hetatm( $record =~ m/^HETATM/ ? 1 : 0 );
  $self->primary( 0 );

  return $self; # otherwise an ATOM will always return false (the value of
                # the previous line) 
}


sub cal_vector {
  my($self, $a2) = @_;
  if($a2){
    $self->log->debug("Got atoms $self, $a2");
    my %v = (	x => ($a2->{x} - $self->{x}),
		          y => ($a2->{y} - $self->{y}),
		          z => ($a2->{z} - $self->{z}) );
    
    return (\%v);
  }
}
#-------------------------------------------------------------------------------
#- stuff awaiting cargo culting ------------------------------------------------
#-------------------------------------------------------------------------------

# =head2 write
# 
# Write out the properties of atom in no particular format. If a filehandle is
# supplied, writes to that, otherwise to STDOUT.
# 
#   $atom_info = $atom->write;
#   open( FILE, '>atom.txt' );
#   $atom->write( \*FILE );
# 
# =cut
# 
# sub write {
#   my ( $self, $fh ) = @_;
# 
#   # dump to STDOUT unless we get a different filehandle
#   $fh ||= *STDOUT;
# 
#   print $fh 'ATOM:' 
#             . $self->serial . ' ' 
#             . $self->name   . ' '
#             . $self->x      . ' '
#             . $self->y      . ' ' 
#             . $self->z 
#             . ' Temperature factor: ' . $self->tempFactor
#             . "\n";
# }
# 
# 
#-------------------------------------------------------------------------------

#sub bond_plane_angle {
#  #get the base and the three atoms that define the plane of the bond
#  my ($base, $a1,$a2,$a3, %base_plane_coos) = @_;
#  my @atom_list1 = &{$base_plane_coos{$base->type()}}($base);
#  my ( $px1, $py1, $pz1 )= points2plane(@atom_list1);
#  my @atom_list2 = ($a1,$a2,$a3);
#  my ( $px2, $py2, $pz2 ) = points2plane(@atom_list2);
#  my $dp12=$px1*$px2+$py1*$py2+$pz1*$pz2;
#  my $costheta = $dp12/(sqrt($px1*$px1+$py1*$py1+$pz1*$pz1)*sqrt($px2*$px2+$py2*$py2+$pz2*$pz2)) ;
#  my $angle = rad2deg(&acos($costheta));  
#  $angle -= 180 if ($angle > 90);
#  #print "$dp\n";
#  #my $angle = rad2deg(atan($dp));
#  return($angle);
#}
#
#
#
#sub points2plane{
#  my @atom_list = @_;
#  my @xyz1 = $atom_list[0]->xyz();
#  my @xyz2 = $atom_list[1]->xyz();
#  my @xyz3 = $atom_list[2]->xyz();
#  
#  # The standard equation of the plane is Ax + By + Cz = -D;
#  # A B C and D are calculated in the following way
#
#  # A = y1(z2-z3) + y2(z3-z1) + y3(z1-z2);
#
#  my $a = $xyz1[1]*($xyz2[2] - $xyz3[2]) + $xyz2[1]*($xyz3[2] - $xyz1[2]) + $xyz3[1]*($xyz1[2] - $xyz2[2]);
#
#  # B = z1(x2-x3) + z2(x3-x1) + z3(x1-x2);
#  
#  my $b = $xyz1[2]*($xyz2[0] - $xyz3[0]) + $xyz2[2]*($xyz3[0] - $xyz1[0]) + $xyz3[2]*($xyz1[0] - $xyz2[0]);
#
#  # C = x1(y2-y3) + x2(y3-y1) + z3(y1-y2);
#  
#  my $c = $xyz1[0]*($xyz2[1] - $xyz3[1]) + $xyz2[0]*($xyz3[1] - $xyz1[1]) + $xyz3[0]*($xyz1[1] - $xyz2[1]);
#
#  # D = x1(y2z3-y3z2) + x2(y3z1-y1z3) + x3(y1z2-y2z1);
#  
#  my $d =($xyz1[0]*($xyz2[1]*$xyz3[2] - $xyz3[1]*$xyz2[2]) + $xyz2[0]*($xyz3[1]*$xyz1[2] - $xyz1[1]*$xyz3[2]) +  $xyz3[0]*($xyz1[1]*$xyz2[2] - $xyz2[1]*$xyz1[2]));
#  
#  # Change into Hessian Normal Form
#  
#  #Normalise 
#  my ($np1, $ax, $by, $cz, $p);
#  eval{
#    $np1 = sqrt($a*$a+$b*$b+$c*$c);
#    
#    $ax = $a/$np1;
#    $by = $b/$np1;
#    $cz = $c/$np1;
#    $p  = $d/$np1;
#  };
#
#  #print "$ax, $by, $cz, $p\n";
#  return($ax, $by, $cz, $np1, $p);
#}
#
#sub planer_bases {
#  my $base1 = shift;
#  my $base2 = shift;
#  my %base_plane_coos = @_;
#  my @atom_list1 = &{$base_plane_coos{$base1->type()}}($base1);
#  my ($px1, $py1, $pz1, $np1, $p1) = points2plane( @atom_list1);
#  my @atom_list2 = &{$base_plane_coos{$base2->type()}}($base2);
#  
#  my ($px2, $py2, $pz2, $np2, $p2) = points2plane( @atom_list2);
#
#  #Now work out the line of intersect
#
#
#
#
#
#  # This works out the inner product of the two planes
#
#  my $dp12=$px1*$px2+$py1*$py2+$pz1*$pz2;
#
#  my $costheta = $dp12/(sqrt($px1*$px1+$py1*$py1+$pz1*$pz1)*sqrt($px2*$px2+$py2*$py2+$pz2*$pz2)) ;
#  
#  #print "$costheta\n";
#  #print "$p1, $p2";
#  my $angle = rad2deg(&acos($costheta));
#
#  #print "angle = $angle\n";
#  return "$angle";
#}
#
#
#sub bond_angle{
#  # atom1, donor/acceptor, atom2, donor/acceptor, residue1, residue2
#  my ($a1, $h1, $a2, $h2, $r1, $r2) = @_;
#  #vector1 A->D
#  my (%v1, %v2, $bond_plane_angle);
#  #vector2 A->C
#  if ($h1 eq "A"){
#    my $c = get_carbon($a1->number(),$r1);
#    %v2 = cal_vector($a1, $c);
#    %v1 = cal_vector($a1, $a2);
#    #see if the bond is in the same plane
#    #The bond plane is defined by ($a1,4c,$a2);
#    $bond_plane_angle = &bond_plane_angle($r1, $a1, $c, $a2 );
#  }
#  elsif($h2 eq "A"){
#    my $c = get_carbon($a2->number(),$r2);
#    %v2 = cal_vector($a2, $c);
#    %v1 = cal_vector($a2, $a1);
#    #see if the bond is in the same plane
#    #The bond plane is defined by ($a1,4c,$a2);
#    $bond_plane_angle = &bond_plane_angle($r2, $a1, $c, $a2 );
#  }
#  else{
#    print "no donor\n";
#  }
#  
#  #print "$bond_plane_angle\n";
#  if (%v1 && %v2){ 
#    # Then need to find the angle between these two vectors.
#    # Uses the dot product equation a.b = |a||b| cos THETA
#    #|a| -> sqroot of X2 + Y2 + Z2
#    my $mod_v1 = &mod_vector(%v1);
#    my $mod_v2 = &mod_vector(%v2);
#    #a.b -> X x X + Y x Y + Z X Z
#    my $vp = vector_product(\%v1, \%v2);
#    my $angle = rad2deg(acos($vp/($mod_v1*$mod_v2)));
#    return($angle,$bond_plane_angle );
#  }
#  else{
#    warn "no angle determined\n";
#    return("0");
#  }
#}
#
#sub mod_vector {
#  #|a| -> sqroot of X2 + Y2 + Z2
#  my %
#v = @_;
#  my $mod_v;
#  foreach (keys %v){
#    $mod_v += $v{$_}*$v{$_};
#  }
#  return(sqrt($mod_v));
#}
#
#sub vector_product{
#  my ($v1, $v2 ) = @_;
#  my $prod;
#  foreach (keys %{$v1}){
#    $prod += $$v1{$_}*$$v2{$_};
#  }
#  return($prod);
#}
#
#sub get_carbon{  
#  my($no, $r) = @_;
#  $no--;
#  my $c;
#  foreach $a ($r->each()){
#    $c = $a if (($a->number == $no) && ($a->type =~ /C/i));
#  }  
#  if (!$c){
#    $no = $no + 2;
#    foreach $a ($r->each()){
#      $c = $a if (($a->number == $no) && ($a->type =~ /C/i));
#    }
#  }
#  return ($c);
#}
#
#
#sub cal_vector {
#  my($a1, $a2) = @_;
#  my @a1xyz = $a1->xyz();
#  my @a2xyz = $a2->xyz();
#  my @xyz;
#  for (my $n=0; $n < @a1xyz; $n++){
#   my $coos = $a2xyz[$n] - $a1xyz[$n];
#   push(@xyz, $coos);
#  }
#    
#  my %v ={  x => $xyz[0],
#            y => $xyz[1],
#            z => $xyz[2] };
#    
#  return (%v);
#}

#-------------------------------------------------------------------------------

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
