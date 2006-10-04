=head1 NAME

Chainset

=head1 DESCRIPTION

The Chainset object conatins a list of any PDB chain(s) and HETATMs/Ligand 
objects. It contains the experimental method used and the pdb assigned UID.  
There isalso the pdb header information contain within this object.

=head1 AUTHOR

B<Robert Finn> Email rdf@sanger.ac.uk

=cut

#
#Perl module for reading in a PDB file
#
#Hacked up by Robert Finn <rdf@sanger.ac.uk>
#

package Bio::Pfam::Structure::Chainset;

use vars qw($AUTOLOAD @ISA @EXPORT_OK);
use Exporter;
use strict;
use Bio::Pfam::Structure::Atom;
use Bio::Pfam::Structure::Residue;
use Bio::Pfam::Structure::Chain;
use Bio::Pfam::Structure::Ligand;

@EXPORT_OK = qw();

@ISA = ( 'Exporter' );

=head2 new
 Title    :new
 Function :
 Returns  :
 Args     :
Create a new chainset object.
$chainset = Chainset->new();

=cut

sub new {
  my ($class) = @_;
  my $self = {};
  $self = {
	   uid => undef,
	   chains => [],
	   ligand => [],
	   expt => "no method defined",
	   header => undef,
	  };
  bless ($self, $class);
  return $self;
}


=head2
 Title

=cut

sub add_chain {
  my $self = shift;
  my $new_chain = shift;
  if($new_chain){
    push (@{$self->{chains}}, $new_chain);
  }
}
	
sub add_ligand{
  my $self = shift;
  my $new_ligand = shift;
  if($new_ligand){
    push (@{$self->{ligand}}, $new_ligand);
  }
}

########
# each #
########

=head1 each_chain

Returns a chain object for each chain within the chainset

Use: @_ = $chainset->each();

=cut

sub each_chain {
  my $self = shift;
  return(@{$self->{chains}});
}
		
######
# id #
######

=head1 id 

Get/set id method.  The uid is normally the unique identifier supplied
by to the structure by the PDB.   

Get

$id = $chainset->();

Set

$chainset->($new_id);

=cut

sub id {
  my $self = shift;
  my $uid = shift;
  if (defined $uid){
    $self->{uid} = $uid;
  }
  else{
    $_ = $self->{uid};
    return $_;
  }
}

#########
# write #
#########

=head1 write

Write out a Chainset.  This writes out the chainset details (id, header etc.)
Then it passes each chain object to the chain write method.

$chainset->(\*FILHANDLE);
 

=cut



# write out object
sub write {
  my $self = shift;
  my $FILEHANDLE  = shift;
  my @chains = $self->each();
  @_ = $self->header();
  foreach (@_){
    print $FILEHANDLE "$_\n";
  }

  foreach my $chain (@chains){
    my $uid = $self->id(); 
    print $FILEHANDLE "CHAINSET:: $uid\n";
    $chain->write($FILEHANDLE);
  }
}

sub write_pdb_style {
  #receive filename
  my $self = shift;
  my $FILEHANDLE  = shift;
  my @chains = $self->each();
  @_ = $self->header();
  my $uid = $self->id();
  
   print $FILEHANDLE "HEADER   Produce from Robs code - Original $uid \n";
  foreach(@_){
    print $FILEHANDLE $_;
  }
  foreach my $chain (@chains){
    $chain->write_pdb_chain($FILEHANDLE);
  }
  foreach my $ligand ($self->each_ligand()){
    $ligand->write_pdb_ligand($FILEHANDLE);
  }
  print $FILEHANDLE "END\n";
}

##########
# header #
##########


=head1 header

get/add header information,

Get

@header_info = $chainset->header();


Add

$chainset->header($a_header_line);

Note: This could probably be inproved so that the header information was
added as types of information, rather than just one block of information.

=cut

sub header
	{
	my $self = shift;
	my $header_info = shift;
	if (defined $header_info)
		{
		push(@{$self->{header}},$header_info);
		return $self;
		}
	else
		{
		if ($self->{header}){
			return @{$self->{header}};
			}
		}
	}

###############
# expt_method #
###############

=head1 expt_method

get/set experimental methods.

definitely do not want to work on experimental models

=cut

sub expt_method
	{
	my $self = shift;
	my $method = shift;
	if (defined $method)
		{
		if ($method =~ /NMR/)
			{
			$self->{expt}= "NMR";
			}
		elsif ($method =~ /DIFFRACTION/)
			{
			if ($method =~ /X-RAY/)
				{
				$self->{expt} = "X-RAY DIFFRACTION";
				}
			elsif ($method =~ /ELECTRON/)
				{
				$self->{expt}="ELECTRON DIFFRACTION";
				}
			elsif ($method =~ /FIBER/)
				{
				$self->{expt}="ELECTRON DIFFRACTION";
				}
			elsif ($method =~ /NEUTRON/)
				{
				$self->{expt}="NEUTRON DIFFRACTION";
				}
			}
		elsif ($method =~ /THEORETICAL/)
			{
			$self->{expt}="THEORETICAL MODEL";
			}
		else 
			{
			$self->{expt}="OTHER";
			}
		}
	else
		{
		return $self->{expt};
		}
	}


	
################ Method subroutines #########################
#							    #	
# All are used to populate the various objects              #
#                                                           #
#############################################################

#protein objects

sub new_chain {
  my $type = shift;		
  my $chain_id = shift; 
  my $chain = Bio::Pfam::Structure::Chain->new();
  $chain->chain_id($chain_id);
  $chain->type($type);
  $chain->region_info(0);
  return $chain;
}

sub new_residue{
  my ($no, $type) = @_;
  my $residue = Bio::Pfam::Structure::Residue->new();
  $residue->residue_no($no);
  $residue->type($type);
  return $residue;
}

sub new_atom {
  my ($chain_type, $first, $atom_type, $atom_no, $atom_temperature, @atom_xyz) = @_;
  my $atom = Bio::Pfam::Structure::Atom->new();
  $atom->number($atom_no);
  $atom->type($atom_type);
  $atom->xyz(@atom_xyz);
  $atom->temperature($atom_temperature);
  $atom->primary(0);
  return $atom;
}

sub new_ligand {
  my ($lig_no, $lig_name, $chain_id, $lig_head_info) = @_;
  my $ligand = Bio::Pfam::Structure::Ligand->new_ligand();
  $ligand->ligand_no($lig_no);
  $ligand->type($lig_name);
  $ligand->chain_id($chain_id);
  
  if(defined $$lig_head_info{$lig_name}{'numHetAtm'}){
    $ligand->numHetAtm($$lig_head_info{$lig_name}{'numHetAtm'})
  }
  if($$lig_head_info{$lig_name}{'info'}){
    $ligand->info($$lig_head_info{$lig_name}{'info'});
  }
  if($$lig_head_info{$lig_name}{'formula'}){
    $ligand->formula($$lig_head_info{$lig_name}{'formula'});
  }
  if($$lig_head_info{$lig_name}{'name'}){
    $ligand->name($$lig_head_info{$lig_name}{'name'});
  }
  if($$lig_head_info{$lig_name}{'syn'}){
    $ligand->synonym($$lig_head_info{$lig_name}{'syn'});
  }
  return $ligand;
}

sub each_ligand {
  my $self = shift;
  return(@{$self->{ligand}});
}


sub transform_chainset{
  my $self = shift;
  my $trans = shift; 
  foreach my $chain ($self->each()){
    $chain->transform_chain($trans);
  }
  foreach my $ligand ($self->each_ligand()){
    $ligand->transform_ligand($trans);
  }
}

#################################################################
# Locate the domain definitions and make a chain for the region #
#################################################################

sub domainer{
  my ($self, @mapdata) = @_;
  my $domainset = Bio::Pfam::Structure::Chainset->new();
  $domainset->id($self->id);
  my %domains;
  foreach my $domain_coos (@mapdata){
      if (!$domains{$$domain_coos[11]}){
	$domains{$$domain_coos[11]}++
      }
    }
  foreach my $domain_index (keys %domains){
    my $domain;
  DOMAIN:
    foreach my $domain_coos (@mapdata){
      next DOMAIN if ($$domain_coos[11] ne $domain_index);
      my $chain = $$domain_coos[3]; 
      if (!$chain){
	$chain = 'null';
      }
      foreach ($self->each()){
	if ($chain eq $_->chain_id){
	  if (defined $domain){
	    $domain = $_->region($$domain_coos[4], $$domain_coos[5], $domain);
	  }else{
	    $domain = $_->region($$domain_coos[4], $$domain_coos[5]);
	  }
	}
      }
    }	
    $domainset->add($domain);
  }
  return $domainset;	
}




############
# read_pdb #
############

=head1 read_pdb

Read in a pdb file.  This is specific to the pdb file format.  The 
inconsistancies of the pdb file mean that this may fail.  Test have
shown that over 95% of pdb files are parsed correctly.

use: $chainset->read_pdb(\*FILEHANDLE); 

=cut

sub read_pdb {
  my $self = shift;
  my $PDB = shift;
  #Could pass in a file handle or an array ref from pfetch server
  if(ref($PDB) ne "ARRAY"){
    my @tmp = <$PDB>;
    $PDB = \@tmp;
  }
  my ($chain, $residue, $ligand, %lig_head_info);
  foreach(@$PDB){
    #print "$_\n";
    my @line = split("", $_);
    my $record = join("", @line[0..5]);
    $record =~ s/\s//g;
    if($record eq "ATOM"){
      #Here I need to try and distinguish between protein and DNA/RNA
      my $atom_no = join("", @line[6..10]);
      $atom_no =~ s/\s+//g;
      my $atom_type = join("",@line[12..15]);
      $atom_type =~ s/\s+//g;
      my $alt_char = $line[16]; 
      my $res_name = join("", @line[17..19]);
      $res_name =~ s/\s+//g;
      my $chain_id = $line[21];
      $chain_id = "null" if ($chain_id eq " ");
      my $res_no = join("", @line[22..25]);
      $res_no =~ s/\s+//g;
      my $res_insert = $line[26];
      my $x = join("", @line[30..37]);
      $x =~ s/\s+//g;
      my $y = join("", @line[38..45]);
      $y =~ s/\s+//g;
      my $z = join("", @line[46..53]);
      $z =~ s/\s+//g;
      my $atom_occu = join("", @line[54..59]);
      $atom_occu =~ s/\s+//g;
      my $atom_temp = join("", @line[60..65]);
      $atom_temp =~ s/\s+//g;
      
      #Protein/DNA/RNA
      if(!$chain or $chain_id ne $chain->chain_id()){
 	#new chain is needed
	$chain = new_chain("unknown", $chain_id);
	$self->add_chain($chain);
      }

      if (!$residue or $res_no != $residue->residue_no){
	$residue = new_residue($res_no, $res_name);
	$chain->add($residue);
      }
      
      my $atom = new_atom("unknown", 1,$atom_type, $atom_no, $atom_temp, ($x,$y,$z));
      $residue->add($atom);
      
    }elsif($record eq "HETATM"){
      my $atom_no = join("", @line[6..10]);
      $atom_no =~ s/\s+//g;
      my $atom_type = join("",@line[12..15]);
      $atom_type =~ s/\s+//g;
      my $alt_char = $line[16]; 
      my $res_name = join("", @line[17..19]);
      $res_name =~ s/\s+//g;
      my $chain_id = $line[21];
      $chain_id = "null" if ($chain_id eq " ");
      my $res_no = join("", @line[22..25]);
      $res_no =~ s/\s+//g;
      my $res_insert = $line[26];
      my $x = join("", @line[30..37]);
      $x =~ s/\s+//g;
      my $y = join("", @line[38..45]);
      $y =~ s/\s+//g;
      my $z = join("", @line[46..53]);
      $z =~ s/\s+//g;
      my $atom_occu = join("", @line[54..59]);
      $atom_occu =~ s/\s+//g;
      my $atom_temp = join("", @line[60..65]);
      $atom_temp =~ s/\s+//g;
      if($res_name ne "HOH"){
	if(!$ligand or $chain_id ne $ligand->chain_id()){
	  #new chain is needed
	  $ligand = new_ligand($res_no, $res_name, $chain_id, \%lig_head_info);
	  $self->add_ligand($ligand);
	}elsif ($res_no != $ligand->ligand_no){
	  $ligand = new_ligand($res_no, $res_name, $chain_id, \%lig_head_info);
	  $self->add_ligand($ligand);
	}
      
	my $atom = new_atom("hetatm", 1,$atom_type, $atom_no, $atom_temp, ($x,$y,$z));
	$ligand->add($atom);
      }else{
	#warn "Ignoring water";
      }
    }elsif($record eq "HET"){
      my $cmpd = join("", @line[7..9]);
      $cmpd =~ s/\s+//g;#Remove all white space
      #print "Compound=$cmpd\n";
      $lig_head_info{$cmpd}{'chain'}= $line[12];
      $lig_head_info{$cmpd}{'insert'} = $line[17];
      $lig_head_info{$cmpd}{'seqNum'}=join("",@line[13..16]);
      $lig_head_info{$cmpd}{'numHetAtm'}=join("", @line[20..24]);
      $lig_head_info{$cmpd}{'info'} = join("", @line[30..70]);
    }elsif($record eq "FORMUL"){
      #This is the formula of the ligand
      my $cmpd = join("", @line[12..14]);
      $cmpd =~ s/\s+//g;#Remove all white space
      #There could be a continuation so add on.
      
      if (defined @line[19..69]){
	if($lig_head_info{$cmpd}{'formula'}){
	  $lig_head_info{$cmpd}{'formula'}.=join("",@line[19..69]);
	}else{
	  $lig_head_info{$cmpd}{'formula'} = join("",@line[19..69]) ;
	}
      }
    }elsif($record eq "HETNAM"){
      #The name of HET
      my $cmpd = join("", @line[11..13]);
      $cmpd =~ s/\s+//g;#Remove all white space
      $lig_head_info{$cmpd}{'name'}.=join("",@line[15..69]);
    }elsif($record eq "HETSYN"){
      my $cmpd = join("", @line[11..13]);
      $cmpd =~ s/\s+//g;#Remove all white space
      $lig_head_info{$cmpd}{'syn'}.=join("",@line[15..69]);
    }elsif($record eq "HEADER"){
      if ($_ =~ /^HEADER(.*)(\S{4})(\s+)(\d?)$/){
	$self->id($2);
      }
      $self->header($_);
    }elsif($record eq "CRYST1"){
      #If there is such a field, then this must have been X-Ray crystal
      $self->expt_method("X-RAY DIFFRACTION");
    }elsif($record eq "EXPDTA"){
      $self->expt_method($_);
    }
  }
  #Here would be the best place to determine the chain types......
  foreach my $chain ($self->each_chain){
    $chain->_guess_alphabet;
  }
}


1;
