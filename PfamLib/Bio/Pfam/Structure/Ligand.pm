=head1 NAME

Ligand

=head1 DESCRIPTION

The Ligand object contains information on a particular
Ligand from a pdb file.  It also contains a list of atom 
objects.

=head1 AUTHOR

B<Robert Finn> Email rdf@sanger.ac.uk

=cut

#
#Perl module for PDB file manipulation
#
#Hacked up by Robert Finn <rdf@sanger.ac.uk>
#

package Bio::Pfam::Structure::Ligand;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use strict;
use Bio::Pfam::Structure::Atom;

@EXPORT_OK = qw();

@ISA = ( 'Exporter');

#######
# new #
#######

=head1 new_ligand

create a new residue object.
$residue = Ligand->new_ligand();

=cut

sub new_ligand {
	my $class = shift;
	my $self = {};
	$self = {
		 ligand_no => undef,
		 type => undef,
		 chain => undef,
		 atoms => [],
		 ligand_info => undef,
		 ligand_form => undef,
		 numHetAtm => undef,
		 fullname => undef,
		 synonym => undef,
		 ligand_form => undef,
		};
	bless ($self, $class);
	return $self;
	}

##############
# ligand_no #
##############

=head1 ligand_no

getset the residue number

to set

$residue->residue_no('the residue number');

to get 

$residue->residue_no();

=cut

sub ligand_no {
  # yet another get set
  my $self = shift;
  my $ligand_no = shift;
  if (defined $ligand_no){
    $self->{ligand_no} = $ligand_no;
  }else{
    return $self->{ligand_no};
  }
}

sub info {
  # yet another get set
  my $self = shift;
  my $ligand_info = shift;
  if (defined $ligand_info){
    $self->{ligand_info} = $ligand_info;
  }else{
    return $self->{ligand_info};
  }
}

sub formula {
  # yet another get set
  my $self = shift;
  my $ligand_formula = shift;
  if (defined $ligand_formula){
    if ($ligand_formula =~ /\d+\((.*)\)/){
      
      $self->{ligand_form} = $1;
    }else{
	
      $self->{ligand_form} = $ligand_formula;
    }
  }else{
    return $self->{ligand_form};
  }
}

sub numHetAtm {
  my $self = shift;
  my $numHetAtm = shift;
  if(defined $numHetAtm){
    $self->{numHetAtm} = $numHetAtm;
  }else{
    return $self->{numHetAtm};
  }
}

sub name {
  my $self = shift;
  my $name = shift;
  if(defined $name){
    
    $self->{fullname} = $name;
  }else{
    return $self->{fullname};
  }
}

sub synonym {
  my $self = shift;
  my $syn = shift;
  if(defined $syn){
    
    $self->{synonym} = $syn;
  }else{
    return $self->{synonym};
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
  #print scalar(@{$self->{atoms}})."\n";
  #print $self->numHetAtm()."\n";
  
  if (scalar(@{$self->{atoms}}) < $self->numHetAtm()){
    push (@{$self->{atoms}}, $new_atom);
  }elsif(!$self->numHetAtm()){
      push (@{$self->{atoms}}, $new_atom);
  }else{
    warn "no. of atoms, ".scalar(@{$self->{atoms}}).", exceeds hetatoms,".$self->numHetAtm()."\n";
  }
  return $self;	
}

########
# each #
########

=head1 each

Adds an atom object to a list of atom objects.

$residue->add('atom object');


=cut

sub each {
  #returns a list of atom objects
  my $self = shift;
  @_ = @{$self->{atoms}};
  return @_;
}

########
# type #
########

=head1 type

Get/set the ligand type.

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
  if($type){
    $self->{type} = $type;
  }else{			
    $_ = $self->{type};
    return $_;
  }
}


sub chain_id {
  my $self = shift;
  my $id = shift;
  if($id){
    $self->{chain} = $id;
  }else{			
    $_ = $self->{chain};
    return $_;
  }
}

sub fullname {
  my $self = shift;
  my $fullname = shift;
  if($fullname){
    #print STDERR "Fullname: $fullname\n";
    $self->{fullname} = $fullname;
  }else{			
    $_ = $self->{fullname};
    return $_;
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
  
	@_ = $self->each();
  
	foreach my $atoms (@_)
	  {
	    $atoms->write($FILEHANDLE);
	  }
	}


sub write_pdb_ligand {
  my $self = shift;
  my $FILEHANDLE = shift;	
  foreach my $atoms ($self->each()){
    $atoms->write_pdb_hetatm($FILEHANDLE, $self->chain_id, $self->type(), $self->ligand_no());
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
	
	my @patoms1 = $self->each();
	my @patoms2 = $residue2->each();
	my ($patom1, $patom2);	
	
	foreach my $atom (@patoms1)
		{
		my $query = $atom->primary();
		if ($query == '1')
			{
			$patom1 = $atom;
			}
		}
	
	foreach my $atom (@patoms2)
		{
		my $query = $atom->primary();
		if ($query == '1')
			{
			$patom2 = $atom;
			}
		}
	my $distance ;
	if (defined $patom1 && defined $patom2)
		{  
		$distance = $patom1->distance($patom2);
		}
	else	
		{
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
	my @atoms = $self->each();
	foreach my $atom (@atoms)
		{
		$_ = $atom->primary();
		if ($_ == 1)
			{
			$pa = $atom;
			}
		
		}
	
	if (!$pa)
		{
		die "Cannot find primary atoms\n";
		}
	return $pa;
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
	
	@_ = $residue1->each();
	foreach my $atom (@_)
		{
		my $type = $atom->type();
		$type =~ s/\s//g;
		if ($type eq "C")
			{
			$phi_atoms[0] = $atom;
			}
		}
	
	@_ = $self->each();
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


	@_ = $residue3->each();
	
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

sub transform_ligand {
  my $self = shift;
  my $trans = shift;
  foreach my $atom ($self->each){
    $atom->transform_atom($trans);
  }
}
	
