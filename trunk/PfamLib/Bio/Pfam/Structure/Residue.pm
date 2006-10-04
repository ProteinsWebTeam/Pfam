=head1 NAME

Residue

=head1 DESCRIPTION

The Residue object contains information on a particular
residue from a chain.  It also contains a list of atom 
objects. 

=head1 AUTHOR

B<Robert Finn> Email rdf@sanger.ac.uk

=cut

#
#Perl module for PDB file manipulation
#
#Hacked up by Robert Finn <rdf@sanger.ac.uk>
#

package Bio::Pfam::Structure::Residue;


use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use strict;
use Bio::Pfam::Structure::Atom;



@EXPORT_OK = qw();

@ISA = ( 'Exporter');




#######
# new # 
#######

=head1 new

create a new residue object.

$residue = Residue->new();

=cut

sub new {
	my $class = shift;
	my $self = {};
	$self = {
		residue_no => undef,
		type => undef,
		atoms => [],
		secondary => undef,
		modified => undef,
	};
	bless ($self, $class);
	return $self;
	}

##############
# residue_no #
##############

=head1 residue_no

getset the residue number

to set

$residue->residue_no('the residue number');

to get 

$residue->residue_no();

=cut

sub residue_no {
  my $self = shift;
  my $residue_no = shift;
  if (defined $residue_no){
    $self->{residue_no} = $residue_no;
  }else{
    return $self->{residue_no};
  }
}

#######
# add #
#######

=head1 add

add an atom object to a list of atom objects

$residue->add('an atom object');


=cut

sub add{
  #populate both atoms object here
  my $self = shift;
  my $new_atom = shift;
  #poputlate atoms list
  push(@{$self->{atoms}}, $new_atom) if($new_atom);;
}

########
# each #
########

=head1 each_atom

Adds an atom object to a list of atom objects.

$residue->add('atom object');


=cut

sub each_atom {
  #returns a list of atom objects
  my $self = shift;
  return @{$self->{atoms}};
}

########
# type #
########

=head1 type

Get/set the residue type.  

Three character types must be one of 20 amino acids, plus ambigious residues
ASX = ASP or ASN, GLX = GLN or GLU and UNK (unknown).

Currently not restricted one to two character types that should corresspond to 
elements in the periodic table.

get

$residue->type();

set  

$residue->type('residue type');

=cut


sub type {
  my $self = shift;
  my $type = shift;
  if (defined $type){
#    if ($type =~ /\S{3}/){
#      my $match_aa  = 0 ;
#      if($type#
#	if ($type =~ /$_/i){
#	  $self->{type} = $type;
#	  $match_aa = 1;
#	}
#      }
#      if ($match_aa == 0){
#	$self->{type} = $type;
#      }
#    }elsif ($type =~ /\w{1,2}/){
      # verify all atoms in the periodic table ??????
      $self->{type} = $type;
#    }
  }else{
    return $self->{type};
  }
}

#########
# write #
#########

=head1 write 

Write out residue details of the residue object. Then write out
the atoms by passing the list of atom objects to the Atom script.

$residue->write(\*FILEHANDLE);

=cut

sub write {
	my $self = shift;
	my $FILEHANDLE = shift;
	
	print $FILEHANDLE "    RESIDUE::".$self->residue_no()."  "
							.$self->type()."\n";
	#print $FILEHANDLE "    RESIDUE_PROPERTIES::".$self->{secondary}.",".
	#			$self->{modified}."\n";

	
	foreach my $atoms ($self->each_atom)
		{
		$atoms->write($FILEHANDLE);
		}
	}


############
# distance #
############

=head1 distance

Calculates the distance between two residue objects from 
primary atom to primary atom. The distance is return in angstroms.

distance = $residue1->distance($residue2);

=cut
 
sub distance {
  my $self = shift;
  my $residue2 = shift;
	
  my @patoms1 = $self->each_atom();
  my @patoms2 = $residue2->each_atom();
  my ($patom1, $patom2);	
	
  foreach my $atom (@patoms1){
    my $query = $atom->primary();
    if ($query == '1'){
      $patom1 = $atom;
    }
  }
  foreach my $atom (@patoms2){
    my $query = $atom->primary();
    if ($query == '1'){
      $patom2 = $atom;
    }
  }
  my $distance ;
  if (defined $patom1 && defined $patom2){
    $distance = $patom1->distance($patom2);
  }else{
    $distance = 0;
  }
  return $distance;
}


###########
# primary #
###########

=head1 primary

Takes a residues object and returns the primary atom from the list of atoms 
objects contained in the residue object.

$residue->primary();

=cut

sub primary {
  my $self = shift;
  my $pa;
  foreach my $atom ($self->each_atom){
    if ($atom->{primary} == 1){
      $pa = $atom;
      last;
    }
  }
  if (!$pa){
    die "Cannot find primary atoms\n";
  }
  return $pa;
}

######
# ss #
######

=head1 

getset the secondary structure.


=cut

sub ss 
	{
	
	}

############
# modified #
############

=head1



=cut

sub modified
	{

	}


############
# Dihedral #
############

=head1 dihedral

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
	
	my $self = shift;
	my $residue1 = shift;
	my $residue3 =shift;
	my $pi = 3.1415927;

	my (@phi_atoms, @psi_atoms, @w);	
	#setup phi atoms c-n-ca-c, psi atoms n-ca-c-n, omega atoms, ca-c-n-ca
	
	@_ = $residue1->each_atom();
	foreach my $atom (@_)
		{
		my $type = $atom->type();
		$type =~ s/\s//g;
		if ($type eq "C")
			{
			$phi_atoms[0] = $atom;
			}
		}
	
	@_ = $self->each_atom();
	foreach my $atom (@_)
		{
		my $type = $atom->type();
		$type =~ s/\s//g;
		if ($type eq "N")
			{
			$phi_atoms[1] = $atom;
			$psi_atoms[0] = $atom;
			}
		elsif ($type eq "CA")
			{
			$phi_atoms[2] = $atom;
			$psi_atoms[1] = $atom;
			$w[0] = $atom;
			}
		elsif ($type eq "C")
			{
			$phi_atoms[3] = $atom;
			$psi_atoms[2] = $atom;
			$w[1] = $atom;
			}
		}


	@_ = $residue3->each_atom();
	
	foreach my $atom (@_)
		{
		my $type = $atom->type();
		$type =~ s/\s//g;
		if ($type eq "N")
			{
			$psi_atoms[3] = $atom;
			$w[2] =  $atom;
			}
		elsif ($type eq 'CA')
			{
			$w[3]= $atom;
			}
		}


	my (@dihedrals, @ppw, @xyz1, @xyz2, @xyz3, @xyz4);

	@dihedrals = (\@phi_atoms, \@psi_atoms,  \@w); 	
	foreach my $dh (@dihedrals)
		{
		my @atom_list = @{$dh};
		
		@xyz1 = $atom_list[0]->xyz();
		@xyz2 = $atom_list[1]->xyz();
		@xyz3 = $atom_list[2]->xyz();
		@xyz4 = $atom_list[3]->xyz();
		
	
		my $dx12=$xyz1[0]-$xyz2[0];
		my $dy12=$xyz1[1]-$xyz2[1];
		my $dz12=$xyz1[2]-$xyz2[2];
		
		my $dx23=$xyz2[0]-$xyz3[0];
		my $dy23=$xyz2[1]-$xyz3[1];
		my $dz23=$xyz2[2]-$xyz3[2];

		my $dx43=$xyz4[0]-$xyz3[0];
		my $dy43=$xyz4[1]-$xyz3[1];
		my $dz43=$xyz4[2]-$xyz3[2];

	
  		my $px1=$dy12*$dz23-$dy23*$dz12;
  		my $py1=$dz12*$dx23-$dz23*$dx12;
 		my $pz1=$dx12*$dy23-$dx23*$dy12;
 		
  		my $np1=sqrt($px1*$px1+$py1*$py1+$pz1*$pz1);

 		$px1/=$np1;
  		$py1/=$np1;
  		$pz1/=$np1;

		my $px2=$dy43*$dz23-$dy23*$dz43;
		my $py2=$dz43*$dx23-$dz23*$dx43;
		my $pz2=$dx43*$dy23-$dx23*$dy43;
		
		my $np2=sqrt($px2*$px2+$py2*$py2+$pz2*$pz2);
 
  		$px2/=$np2;
  		$py2/=$np2;
  		$pz2/=$np2;
  
		my $dp12=$px1*$px2+$py1*$py2+$pz1*$pz2;

		my $angle=$pi/2.0-atan2($dp12,sqrt(1.0-$dp12*$dp12));

		my $px3=$py1*$pz2-$py2*$pz1;
		my $py3=$pz1*$px2-$pz2*$px1;
		my $pz3=$px1*$py2-$px2*$py1;
		
		my $dp233=$px3*$dx23+$py3*$dy23+$pz3*$dz23;
  
		if ($dp233>0.0) 
			{
			$angle=-$angle;
  			}

 		my $angle_d = $angle/$pi*180.0;
		push (@ppw, $angle_d);
		}
	return @ppw;
}

sub write_pdb_residue {
  my ($self, $FILEHANDLE, $chainId) = @_;
  foreach my $atom ($self->each_atom){
    $atom->write_pdb_atom(\*$FILEHANDLE, $chainId, $self->type, $self->residue_no);
  }
}

my %AA = ('ALA' => 1,
	  'ARG' => 1,
	  'ASN' => 1,
	  'ASP' => 1,
	  'CYS' => 1,
	  'GLN' => 1,
	  'GLU' => 1,
	  'GLY' => 1,
	  'HIS' => 1,
	  'ILE' => 1,
	  'LEU' => 1,
	  'LYS' => 1,
	  'MET' => 1,
	  'PHE' => 1,
	  'PRO' => 1,
	  'SER' => 1,
	  'THR' => 1,
	  'TRP' => 1,
	  'TYR' => 1,
	  'VAL' => 1,
	  'ASX' => 1,
	  'GLX' => 1,
	  'UNK' => 1,
	  'HOH' => 1);

1;
