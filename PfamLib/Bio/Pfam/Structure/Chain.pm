=head1 NAME

Chain

=head1 DESCRIPTION

The Chain object conatins the information on a single peptide structure.
This can be derived from the spliting of a chainset, a single peptide
structure or by the spliting of a chain into regions.  

=head1 AUTHOR

B<Robert Finn> Email rdf@sanger.ac.uk

=cut

#
#Perl module fir PDB
#
#Hacked up by Robert Finn <rdf@sanger.ac.uk>
#

package Bio::Pfam::Structure::Chain;


use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use strict;
#use Atom;
#use Residue;
#use Chainset;
use Text::Wrap;

@EXPORT_OK = qw();

@ISA = ( 'Exporter');

#######
# new #
#######

=head1 new 

Create a new chain object;

$chain = Chain->new();

=cut


 
sub new {
	my ($class) = @_ ;
	my $self ={};
	$self = {
	chain_id => undef,
	type => undef,
	region => undef,
	residues => [],
	};
	
	# by default the region is expected to be the full chain length
	bless($self, $class);
	$self->region_info(0);
	return $self;
}

#######
# add #
#######

=head1 add

Add a new residue object to the list of reisude from that chain. 

$chain->add('a residue');

=cut

sub add {
	my $self = shift;
	my $new_residue = shift;
	my @residues = @{$self->{residues}};
	push (@residues,$new_residue);
	$self->{residues} = \@residues;
	return $self;
}

############
# chain_id #
############

=head1 chain_id

Get/set the chain identification.

If the chain does not have an identification the idnetification will be
set to null.


set

$chain->chain_id('a letter normally');

get 

$chain->chain_id();

returns the chain id.


=cut

sub chain_id {
	my $self = shift;
	my $chain_id = shift;
	if (defined $chain_id)
		{
		$self->{chain_id} = $chain_id;
		return $self;
		}
	else	
		{	
		$chain_id = $self->{chain_id};
		return $chain_id;
		}
}

########
# type #
########

=head1 type

Get/set the chain type.

Restricted vocab to:protein, hetatm, DNA, RNA, model
 

set

$chain->type('protein');

get
 
$chain->type()

returns the chain type

protein, hetatm, DNA, RNA, model

=cut

sub type {
	my $self = shift;
	my $type = shift;
	
	my @types_allowed = ( 'protein', 'hetatom', 'nucleic acid', 'DNA', 'RNA',  'model', 'unknown' );
	my $okay = 0;




	if ($type)
		{
		#verifiy the type.....
		foreach (@types_allowed)
			{
			if ($type =~ /$_/i)
				{
				$okay++;
				}
			}

		if ($okay > 0)
			{
			$self->{type} = $type;
			return $self;
			}
		else 
			{
			print "The chain type specified, $type, is an unknow chain type !!! Currently the chain type is unspecificed\n"; 
			}
		}
	else
		{
		$_ = $self->{type};
		return $_;
		}
}

###############
# region_info # 
###############

=head1 region_info

The region subroutine is a get/set to specify whether the chain is a sub region 
of a chain.  This is mostly for administrational purposes.

set

$chain->region_info('set to 0 or 1' );

0 = whole chain
1 = region of  chain


get

$chain->region_info();

=cut

sub region_info {
	my $self = shift;
	my $region = shift;
	if ($region) 
		{ 
		$self->{region} = $region;
		return $self;
		}
	else
		{
		$_ = $self->{region};
		return $_;
		}
}

########
# each #
########

=head1 each

Compiles a list of residues within the chain.  

Take the present list of residues and add the next residue to the
list.

$chain->add(residue);

=cut


sub each {
	my $self = shift;
	return @{$self->{residues}};
}


##########
# region #
##########

=head1 region

Retruns a new chain object that is a part chain of the current chain.   
There should be passed to this subroutine a start and end residue 
(inclusive) of the new chain.

$fragment = $chain->region('start', 'end');

=cut

sub region {
	my $self = shift;
	my ($start, $end, $new_chain) = @_;
	if (!defined $new_chain) 
		{
		$new_chain = Chain->new();
		}
	#print "Am selecting the following residues from the chain "
	#	.$self->chain_id().": $start, $end\n";
	foreach my $residue ($self->each())
		{
		my $rn = $residue->residue_no();
		if (($rn >= $start) && ($rn <= $end))
			{
			$new_chain->add($residue);
			}
		}

	my $chain_id = $self->chain_id(); 
	$new_chain->chain_id($chain_id);
	my $type = $self->type();
	$new_chain->type($type);
	$new_chain->region_info(1);
	return $new_chain;
}	

#########
# write #
#########

=head1 write

Write to a file the chain information.  This routine then
passes the residues list of the chain to the residue
module that will inturn write out residue information.

$chain->write(\*Filehandle);

=cut

sub write {
	my $self = shift;
	my $FILEHANDLE = shift;
	if ($self->region_info())
		{
		print $FILEHANDLE "  PARTIAL CHAIN: ";
		}
	else
		{
		print $FILEHANDLE "  CHAIN: ";
		}
	
	print $FILEHANDLE "Chain id:".$self->chain_id().", Chain type = ".$self->type()."\n";
	
	#@_ = $self->each();

	foreach my $residue ($self->each)
		{
		$residue->write(\*$FILEHANDLE);
		}
}	


sub write_pdb_chain {
  my $self = shift;
  my $FILEHANDLE = shift;
  #@_ = $self->each();
  
  foreach my $residue ($self->each){
    $residue->write_pdb_residue(\*$FILEHANDLE, $self->chain_id);
  }
  print $FILEHANDLE "TER\n";
}	

###########
# contact #
###########

=head1 contact


Estimates which residues of Chain B are in contact with Chain A.  Calculates 
a CA::CA distance between amino acids.  This method returns a list of residues
from Chain B that are within 10A of Chain A. 

@list = $chainA->contact(chainB);

To alter the 10A contact, give it as an extra variable.

@list = $chainA->contact(chainB, 'contact distance e.g. 7');

=cut


sub contact {
	
	my $self = shift;
	my $chain2 = shift;
	my $contact_distance = shift;
	my $n = 0; 
	my @contact_residues;
	if (!defined $contact_distance)
		{
		$contact_distance = 10.0;
		}
	
	my @chain1_residues = $self->each();
	my @chain2_residues = $chain2->each();

	foreach my $residue1 (@chain1_residues)
		{
		foreach my $residue2 (@chain2_residues)
			{
			my $distance = $residue1->distance($residue2);
			#print "C=$distance\n";
				if (($distance <= $contact_distance) && ($distance > 0))
					{
					#print  "CONTACT\n";
					#print "\n".$residue1->residue_no()." of "
					#.$self->chain_id()." is ".$distance." A away".
					#" from ".$residue2->residue_no()." of "
					#.$chain2->chain_id()."\n";
					my @data = ($residue1, $residue2);
					push (@contact_residues, \@data);
					$n++;
					}
			}
		}
	
	if ($n > 0)
		{
		return @contact_residues;
		}
	}


###############
# chain2fasta #
###############

=head1 chain2fasta

Writes out the sequences of the chain in FASTA format, 60 chars to a line.
It does not care what the chain is, nucleic acid or protein.
$chain->chain2fasta(\*FILEHANDLE);

=cut
sub chain2fasta {
  my $self = shift;
  my $fasta_out = shift;
  my $sequence;
  my %oneletter = qw(	ALA A
			ARG R
			ASN N
			ASP D
			CYS C
			GLN Q
			GLU E
			GLY G
			HIS H
			ILE I
			LEU L
			LYS K
			MET M
			PHE F
			PRO P
			SER S
			THR T
			TRP W
			TYR Y
			VAL V
			ASX B
			GLX Z
			UNK X
		        A   A
		        T   T
		        U   U
		        G   G
                        C   C
		        +A  A
		        +T  T
		        +G  G
		        +C  C
		        +U  U );
        my @residues = $self->each();
	foreach (@residues){
	  my $res_type = $_->type();
	  if(!$oneletter{$res_type}){
	    warn "$res_type is not in the alphabet\n";
	    $sequence .= "X";
	  }else{
	    $sequence .= $oneletter{$res_type};
	  }
	}
  
 
  if($fasta_out){
      my $chain_id = $self->chain_id();
      $Text::Wrap::columns = 60;
      print $fasta_out ">Chain $chain_id\n";
      print $fasta_out wrap("", "", "$sequence\n");
  }else{
      return $sequence;
  }
}

#################
# 2ry_structure #
#################

=head1 2ry_structure

write out sequences with structural mask 


=cut

sub transform_chain{
  my $self = shift;
  my $trans = shift;
  foreach my $residue ($self->each){
    $residue->transform_residue($trans);
  }
}

sub _guess_alphabet {
  my $self = shift;
  my $str = $self->chain2fasta();
  my $type= "unknown";
  
  my $total = CORE::length($str);
  if( $total == 0 ) {
    $self->throw("Got a sequence with no letters in - ".
		 "cannot guess alphabet [$str]");
  }

  my $u = ($str =~ tr/Uu/G/);
  my $atgc = ($str =~ tr/ATGCNatgcn//);
   
  if( ($atgc / $total) > 0.85 ) {
    if(!$u){
      $type = 'DNA';
      #Okay, could still be RNA, look for O2*
    BASE:
      foreach my $base ($self->each){
	foreach my $atom ($base->each_atom){
	  if ($atom->type =~ /O2\*/){
	    print "ATOM".$atom->type."\n";
	    $type = "RNA";
	    last BASE;
	  }
	}
      }
    }else{
      $type = 'RNA';
    }
  }else{
    $type = "protein";
  }
  $self->type($type);

  if ($type eq 'protein'){
    foreach my $res ($self->each){
      foreach my $atom ($res->each_atom){
	if ($atom->type eq "CA"){
	  $atom->primary(1);
	  last;
	}
      }
    }
  }else{
    foreach my $mono ($self->each){
      foreach my $atom ($mono->each_atom){
	$atom->primary(1);
	last;
      }
    }
  }
}
