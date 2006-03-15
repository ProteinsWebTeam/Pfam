=head1 NAME

Atom

=head1 DESCRIPTION

The Atom object contains atom structures and allows the
the extraction and manipulation of a structural information

=head1 AUTHOR

B<Robert Finn> Email rdf@sanger.ac.uk

=cut

#
#Perl module for PDB parsing and structural calculations
#
#Hacked up by Robert Finn <rdf@sanger.ac.uk>
#

package Bio::Pfam::Structure::Atom;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use strict;
#use Chainset;
#use Chain;
#use Residue;
use Math::Trig;
#
#Exporter get all the functions/variables
#

@EXPORT_OK = qw();

#
#@ISA has our inheritance
#
@ISA = ( 'Exporter');

#######
# new #
#######

=head1 new

Create a new atom object.  The Object (although undefined)
is primed to contain the atom type,
its numer, position in space (xyz) and a temperature factor.

	$atom = Atom->new();

=cut
 
sub new
	{
	my $class = shift;
	my $self = {};
	$self = {
		a_type => undef,
		a_no => undef,
		a_x => undef,
		a_y => undef,
		a_z => undef,
		a_temperature => undef,
		primary => undef,
		};
	bless ($self, $class);
	return $self;
	}

##########
# number #
##########

=head1 number

A getset method for atom number

$atom->('a number');

=cut

sub number {
	my $self = shift;
	my $a_no = shift;
	if ($a_no)
		{
		$self->{a_no} = $a_no;
		return $self;
		}
	else
		{
		$_ = $self->{a_no};
		return $_;
		}
	}	

########
# type #
########

=head1 type

Getset the atom type

$atom->('an atom type');

=cut

sub type 
	{
	my $self = shift;
	my $type = shift;
		
	#should make sure that the type corresponds to a true type.....
	
	if (defined $type) 
		{
		$self->{a_type} = $type;
		return $self;
		}
	else
		{
		$_ = $self->{a_type};
		return $_;
		}
	}

#######
# xyz #
#######

=head1 xyz

getset the atom positional information, x, y and z
Assumes that x,y and z are in an array, with x at position
[0], y at [1] and z at [2]

$atom->xyz(@_);

=cut 	

sub xyz 
	{
	my $self = shift;
	my @xyz = @_;
	if (@xyz)
		{
		$self->{a_x} = $xyz[0];
		$self->{a_y} = $xyz[1];
		$self->{a_z} = $xyz[2];
		return $self;
		} 
		else 
		{
		@_ =();
		@_ = ($self->{a_x},$self->{a_y},$self->{a_z});
		return @_;
		}
	}

###############
# temperature #
###############

=head1 temperature

get set the temperature factor if the structure has been solved by X-ray

$atom->temperature('the temperature factor');


=cut


sub temperature 
	{
	my $self = shift;
	my $temp_factor = shift;
	if ($temp_factor)
		{
		$self->{a_temperature} = $temp_factor;	
		return $self;
		}
		else
		{
		return $self->{a_temperature};
		}
	}

#########
# write #
#########

=head1 write

write out the properties of atom to a file

$atom->write(\*FILEHANDLE);

=cut

sub write 
	{
	my $self = shift;
	my $FILEHANDLE = shift;
	print $FILEHANDLE "        ATOM::".$self->{a_no}." ".$self->{a_type}." "
			.$self->{a_x}." ".$self->{a_y}." ".$self->{a_z}." Temperature factor = "
			.$self->{a_temperature}." Index:".$self->primary()."\n";
			
	}


sub write_pdb_atom {
  my $self = shift;
  my ($FILEHANDLE, $chain_id, $residue, $residue_no) =@_;
  $chain_id = " " if ($chain_id eq "null");
  #print "-$chain_id-\n";
  $chain_id = " ";
  
  print $FILEHANDLE "ATOM  "; # Record name
  print $FILEHANDLE sprintf("%5d ", $self->{a_no}); # Atom number
  print $FILEHANDLE sprintf(" %-4s", $self->{a_type}); # Atom name
  #print $FILEHANDLE " "; # This is the alternate location indicator
  $residue =~ s/\s+//;
  print $FILEHANDLE sprintf("%-3s", $residue); #Residue name
  print $FILEHANDLE sprintf(" %1s", $chain_id); # space and chain id
  print $FILEHANDLE sprintf("%4d", $residue_no); 
  print $FILEHANDLE " "; # This is the insert code
  print $FILEHANDLE "   "; # This is the spacer
  print $FILEHANDLE sprintf(" %7.3f %7.3f %7.3f", $self->{a_x}, $self->{a_y}, $self->{a_z});
  print $FILEHANDLE sprintf("%6.2f", "1.00"); # Occupancy
  print $FILEHANDLE sprintf(" %5.2f\n", $self->{a_temperature}); #Temperature factor
  
}

sub write_pdb_hetatm {
  my $self = shift;
  my ($FILEHANDLE, $chain_id, $lig_name, $lig_no) = @_;
  $chain_id = " " if ($chain_id eq "null");
  #print "-$chain_id-\n";
  $chain_id = " ";
  
  print $FILEHANDLE "HETATM"; # Record name
  print $FILEHANDLE sprintf("%5d ", $self->{a_no}); # Atom number
  print $FILEHANDLE sprintf(" %-4s", $self->{a_type}); # Atom name
  #print $FILEHANDLE " "; # This is the alternate location indicator
  #$residue =~ s/\s+//;
  print $FILEHANDLE sprintf("%-3s", $lig_name); #Residue name
  print $FILEHANDLE sprintf(" %1s", $chain_id); # space and chain id
  print $FILEHANDLE sprintf("%4d", $lig_no); 
  print $FILEHANDLE " "; # This is the insert code
  print $FILEHANDLE "   "; # This is the spacer
  print $FILEHANDLE sprintf(" %7.3f %7.3f %7.3f", $self->{a_x}, $self->{a_y}, $self->{a_z});
  print $FILEHANDLE sprintf("%6.2f", "1.00"); # Occupancy
  print $FILEHANDLE sprintf(" %5.2f\n", $self->{a_temperature}); #Temperature factor

}


###########
# Primary #
###########

=head1 primary 

getset primary atoms i.e. CA, to true

$atom->primary('a number');
 or 
$atom->primary();

=cut

sub primary
	{
	my $self = shift;
	my $primary_atom_index = shift;
	if (defined $primary_atom_index)
		{
		$self->{primary} = $primary_atom_index;
		return $self;
		}
	else
		{
		$_ = $self->{primary};
		return $_;
		}	
	}

############
# distance #
############

=head1 distance

Calculates the distance between any two atoms.

$atom1->distance($atom2);

=cut

sub distance {
	my $self =  shift;
	my $atom2 = shift;
	my @xyz1 = $self->xyz();
	my @xyz2 = $atom2->xyz();
	my $x = ($xyz1[0] - $xyz2[0]);
	my $y = ($xyz1[1] - $xyz2[1]);
	my $z = ($xyz1[2] - $xyz2[2]);
	#print "the x,y,z difference is $x,$y,$z \n"; 
	my $distance = sqrt(($x*$x)+($y*$y)+($z*$z));
	return ($distance);
}

#########
# angle #
#########

=head1 angle

Calculation of the angle between three atoms;

	2   3
	 \ /
	  1

Returns the angle in degrees. 

$angle = $atom1->angle($atom2,$atom3);

=cut



sub angle {
	my $self = shift;
	my $atom2 = shift;
	my $atom3 = shift;
	my $pi = 3.1415927;
	# if any of the point exist in a negtaive dimension, reset the relative axis.
	
	my $d12 = $self->distance($atom2);
	my $d13 = $self->distance($atom3);
	my $d23 = $atom2->distance($atom3);
	
	my $angle = acos(($d12*$d12+$d13*$d13-$d23*$d23)/(2*$d12*$d13));
	my $angle_d = $angle/$pi*180.0;
}	
	
sub transform_atom {
  my $self = shift;
  my $trans = shift;
  my %trans = %{$trans};
  my @xyz = $self->xyz();
  my @trans_xyz;
      
      
  $trans_xyz[0] = sprintf("%.3f", $trans{"X2"}{"X1"} * $xyz[0] + $trans{"X2"}{"Y1"} * $xyz[1] + $trans{"X2"}{"Z1"} * $xyz[2] + $trans{"X2"}{"CON"});
  $trans_xyz[1] = sprintf("%.3f", $trans{"Y2"}{"X1"} * $xyz[0] + $trans{"Y2"}{"Y1"} * $xyz[1] + $trans{"Y2"}{"Z1"} * $xyz[2] + $trans{"Y2"}{"CON"});
  $trans_xyz[2] = sprintf("%.3f", $trans{"Z2"}{"X1"} * $xyz[0] + $trans{"Z2"}{"Y1"} * $xyz[1] + $trans{"Z2"}{"Z1"} * $xyz[2] + $trans{"Z2"}{"CON"});
  $self->xyz(@trans_xyz);
}


sub bond_plane_angle {
  #get the base and the three atoms that define the plane of the bond
  my ($base, $a1,$a2,$a3, %base_plane_coos) = @_;
  my @atom_list1 = &{$base_plane_coos{$base->type()}}($base);
  my ( $px1, $py1, $pz1 )= points2plane(@atom_list1);
  my @atom_list2 = ($a1,$a2,$a3);
  my ( $px2, $py2, $pz2 ) = points2plane(@atom_list2);
  my $dp12=$px1*$px2+$py1*$py2+$pz1*$pz2;
  my $costheta = $dp12/(sqrt($px1*$px1+$py1*$py1+$pz1*$pz1)*sqrt($px2*$px2+$py2*$py2+$pz2*$pz2)) ;
  my $angle = rad2deg(&acos($costheta));  
  $angle -= 180 if ($angle > 90);
  #print "$dp\n";
  #my $angle = rad2deg(atan($dp));
  return($angle);
}



sub points2plane{
  my @atom_list = @_;
  my @xyz1 = $atom_list[0]->xyz();
  my @xyz2 = $atom_list[1]->xyz();
  my @xyz3 = $atom_list[2]->xyz();
	
  # The standard equation of the plane is Ax + By + Cz = -D;
  # A B C and D are calculated in the following way

  # A = y1(z2-z3) + y2(z3-z1) + y3(z1-z2);

  my $a = $xyz1[1]*($xyz2[2] - $xyz3[2]) + $xyz2[1]*($xyz3[2] - $xyz1[2]) + $xyz3[1]*($xyz1[2] - $xyz2[2]);

  # B = z1(x2-x3) + z2(x3-x1) + z3(x1-x2);
  
  my $b = $xyz1[2]*($xyz2[0] - $xyz3[0]) + $xyz2[2]*($xyz3[0] - $xyz1[0]) + $xyz3[2]*($xyz1[0] - $xyz2[0]);

  # C = x1(y2-y3) + x2(y3-y1) + z3(y1-y2);
  
  my $c = $xyz1[0]*($xyz2[1] - $xyz3[1]) + $xyz2[0]*($xyz3[1] - $xyz1[1]) + $xyz3[0]*($xyz1[1] - $xyz2[1]);

  # D = x1(y2z3-y3z2) + x2(y3z1-y1z3) + x3(y1z2-y2z1);
  
  my $d =($xyz1[0]*($xyz2[1]*$xyz3[2] - $xyz3[1]*$xyz2[2]) + $xyz2[0]*($xyz3[1]*$xyz1[2] - $xyz1[1]*$xyz3[2]) +  $xyz3[0]*($xyz1[1]*$xyz2[2] - $xyz2[1]*$xyz1[2]));
  
  # Change into Hessian Normal Form
  
  #Normalise 
  my ($np1, $ax, $by, $cz, $p);
  eval{
    $np1 = sqrt($a*$a+$b*$b+$c*$c);
    
    $ax = $a/$np1;
    $by = $b/$np1;
    $cz = $c/$np1;
    $p  = $d/$np1;
  };

  #print "$ax, $by, $cz, $p\n";
  return($ax, $by, $cz, $np1, $p);
}

sub planer_bases {
	my $base1 = shift;
	my $base2 = shift;
	my %base_plane_coos = @_;
	my @atom_list1 = &{$base_plane_coos{$base1->type()}}($base1);
	my ($px1, $py1, $pz1, $np1, $p1) = points2plane( @atom_list1);
	my @atom_list2 = &{$base_plane_coos{$base2->type()}}($base2);
	
	my ($px2, $py2, $pz2, $np2, $p2) = points2plane( @atom_list2);

	#Now work out the line of intersect





	# This works out the inner product of the two planes

	my $dp12=$px1*$px2+$py1*$py2+$pz1*$pz2;

	my $costheta = $dp12/(sqrt($px1*$px1+$py1*$py1+$pz1*$pz1)*sqrt($px2*$px2+$py2*$py2+$pz2*$pz2)) ;
	
	#print "$costheta\n";
	#print "$p1, $p2";
	my $angle = rad2deg(&acos($costheta));

	#print "angle = $angle\n";
	return "$angle";
}


sub bond_angle{
  # atom1, donor/acceptor, atom2, donor/acceptor, residue1, residue2
  my ($a1, $h1, $a2, $h2, $r1, $r2) = @_;
  #vector1 A->D
  my (%v1, %v2, $bond_plane_angle);
  #vector2 A->C
  if ($h1 eq "A"){
    my $c = get_carbon($a1->number(),$r1);
    %v2 = cal_vector($a1, $c);
    %v1 = cal_vector($a1, $a2);
    #see if the bond is in the same plane
    #The bond plane is defined by ($a1,4c,$a2);
    $bond_plane_angle = &bond_plane_angle($r1, $a1, $c, $a2 );
  }
  elsif($h2 eq "A"){
    my $c = get_carbon($a2->number(),$r2);
    %v2 = cal_vector($a2, $c);
    %v1 = cal_vector($a2, $a1);
    #see if the bond is in the same plane
    #The bond plane is defined by ($a1,4c,$a2);
    $bond_plane_angle = &bond_plane_angle($r2, $a1, $c, $a2 );
  }
  else{
    print "no donor\n";
  }
  
  #print "$bond_plane_angle\n";
  if (%v1 && %v2){ 
    # Then need to find the angle between these two vectors.
    # Uses the dot product equation a.b = |a||b| cos THETA
    #|a| -> sqroot of X2 + Y2 + Z2
    my $mod_v1 = &mod_vector(%v1);
    my $mod_v2 = &mod_vector(%v2);
    #a.b -> X x X + Y x Y + Z X Z
    my $vp = vector_product(\%v1, \%v2);
    my $angle = rad2deg(acos($vp/($mod_v1*$mod_v2)));
    return($angle,$bond_plane_angle );
  }
  else{
    warn "no angle determined\n";
    return("0");
  }
}

sub mod_vector {
  #|a| -> sqroot of X2 + Y2 + Z2
  my %
v = @_;
  my $mod_v;
  foreach (keys %v){
    $mod_v += $v{$_}*$v{$_};
  }
  return(sqrt($mod_v));
}

sub vector_product{
  my ($v1, $v2 ) = @_;
  my $prod;
  foreach (keys %{$v1}){
    $prod += $$v1{$_}*$$v2{$_};
  }
  return($prod);
}

sub get_carbon{	
  my($no, $r) = @_;
  $no--;
  my $c;
  foreach $a ($r->each()){
    $c = $a if (($a->number == $no) && ($a->type =~ /C/i));
  }	
  if (!$c){
    $no = $no + 2;
    foreach $a ($r->each()){
      $c = $a if (($a->number == $no) && ($a->type =~ /C/i));
    }
  }
  return ($c);
}


sub cal_vector {
    my($a1, $a2) = @_;
    my @a1xyz = $a1->xyz();
    my @a2xyz = $a2->xyz();
    my @xyz;
    for (my $n=0; $n < @a1xyz; $n++){
	my $coos = $a2xyz[$n] - $a1xyz[$n];
	push(@xyz, $coos);
    }
    
    my %v ={	x => $xyz[0],
		y => $xyz[1],
		z => $xyz[2],
	    };
    
  return (%v);
}



1;

	
