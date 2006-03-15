package BioSeqFormat;

use strict;

###########################################################
# Constructor
sub new { my $self = bless {}, ref($_[0]) || $_[0] }

###########################################################
# Destructor
sub delete 
{
	delete $_[0]->{'id'};
	delete $_[0]->{'ac'};
	delete $_[0]->{'os'};
	delete $_[0]->{'oc'};
	delete $_[0]->{'de'};
	delete $_[0]->{'sq'};
}

###########################################################
# Set methods
sub set_id { return $_[0]->{'id'} = $_[1] }
sub set_ac { return $_[0]->{'ac'} = $_[1] }
sub set_gn { return $_[0]->{'gn'} = $_[1] }
sub set_os { return $_[0]->{'os'} = $_[1] }
sub set_oc { return $_[0]->{'oc'} = $_[1] }
sub set_de { return $_[0]->{'de'} = $_[1] }
sub set_sq { return $_[0]->{'sq'} = $_[1] }


###########################################################
# Get methods
sub get_id { return $_[0]->{'id'} }
sub get_ac { return $_[0]->{'ac'} }
sub get_gn { return $_[0]->{'gn'} }
sub get_os { return $_[0]->{'os'} }
sub get_oc { return $_[0]->{'oc'} }
sub get_de { return $_[0]->{'de'} }
sub get_sq { return $_[0]->{'sq'} }
sub get_ln { return length get_sq($_[0]) }
	

###########################################################
# SPTREMBL
package sptrembl;

@sptrembl::ISA = qw(BioSeqFormat);

sub toObject
{
	my ($self, $fh) = @_;
	LOOP: while (<$fh>)
	{ 
		CASE: {
		if (/^ID\s+([\w\d]+)[^\w\d]/) { $self->set_id($1); last CASE }
		if (/^AC\s+([\w\d]+)[^\w\d]/) { $self->set_ac($1); last CASE }
		if (/^DE\s+(\S.*)$/)          { $self->set_de($self->get_de . $1); last CASE }
		if (/^OS\s+(\S.*)$/)          { $self->set_os($self->get_os . $1); last CASE }
    if (/^OC\s+(\S.*)$/)          { $self->set_oc($self->get_oc . $1); last CASE }
    if (/^\s+([\w\s]+)$/)         { $self->set_sq($self->get_sq . $1); last CASE }
		if (/^GN\s+(.*)$/)            { ($self->get_gn)? $self->set_gn($self->get_gn ." $1"):
                                     $self->set_gn($self->get_gn . $1); last CASE }
		if (/^\/\//)                  { last LOOP }
		}
	}
  	$self->{'de'} =  lc $self->{'de'};
	$self->{'de'} =~ s/<//g;
	$self->{'de'} =~ s/>/-/g;
	$self->{'de'} =~ s/\.$//;
	$self->{'sq'} =~ s/\s+//g;
	$self->{'os'} =~ s/sp\./sp/g;
 	$self->{'os'} =~ s/[\(,\.].*$//;
  	$self->{'os'} =~ s/\'//g;
  	$self->{'os'} =~ s/\s+$//;
  	$self->{'os'} =~ s/\.+/\./g;
  	$self->{'os'} =~ s/\:/ /g;
  	$self->{'oc'} =~ s/\.$//;
 	$self->{'oc'} =~ s/\./;/g;
	$self->{'oc'} =~ s/;\s/;/g;
 	$self->{'oc'} =~ s/;;/;/g;
 	$self->{'oc'} =~ s/,//g;
	$self->{'gn'} =~ s/\.\s*$//;
	$self->{'gn'} =~ s/</'/g;
	$self->{'gn'} =~ s/>/'/g;
	
	($self->get_id eq '')? return -1: return 1;
}

# Translate to fasta format
sub toFasta
{
	my $self = shift;
	die "Try to create a fasta file without ID" if (!$self->get_id);
	my $fasta = fasta->new;
	$fasta->set_id($self->get_id);
	$fasta->set_ac($self->get_ac);
	$fasta->set_gn($self->get_gn);	
	$fasta->set_os($self->get_os);
	$fasta->set_sq($self->get_sq);
	return $fasta;
}

# Output
sub out {
	my $self = shift;
	die "Try to create a fasta file without ID" if (!$self->get_id);
	my $string = "ID   ".$self->get_id()."\n";
	$string   .= "AC   ".$self->get_ac().";\n" if ($self->get_ac());
	$string   .= "GN   ".$self->get_gn().";\n" if ($self->get_gn());
	$string   .= "DE   ".$self->get_de()."\n"  if ($self->get_de());
	$string   .= "OS   ".$self->get_os().".\n" if ($self->get_os());
	my @oc = split /;/, $self->get_oc();
	my $ocString = "OC   ";
	foreach (@oc) {
		if (length($ocString)+length($_) < 65) {
			$ocString .= $_.";";
		}
		else {
			$string .= $ocString."\n";
			$ocString = "OC   ".$_.";";
		}
	}
	$ocString =~ s/;$//;
	$string .= $ocString."\n" if ($ocString ne "OC   ");
	$string .= "SQ   SEQUENCE ".$self->get_ln()." AA;\n" if ($self->get_sq());
	my $sequence = $self->get_sq() if ($self->get_sq());
	while ($sequence) {
		$string .= "     ".substr($sequence, 0, 60)."\n";
		substr($sequence, 0, 60) = '';
	}
	$string .= "\/\/\n";
	return $string;
}

###########################################################
# FASTA
package fasta;

@fasta::ISA = qw(BioSeqFormat);

# Transform a fasta entry in an object
sub toObject
{
	my ($self, $fh) = @_;
	my $counter = 0;
	while (<$fh>)
	{
		if (/^>\s*([\d\w\.]+)[^\d\w\.]/)
		{
			last if ($counter++ > 0);
			$self->set_id($1);
			die "Set a fasta ID with null value" if (!$1);
		}
		else
		{
			chomp;
			$self->set_sq( $self->get_sq . $_);
		}
	}
	($self->get_id eq '')? return -1: return 1;
}

# Create a fasta output
sub out
{	
	my $self = shift;
	die "Try to create a fasta file without ID" if (!$self->get_id);
	my $string = ">".$self->get_id." ".$self->get_ac." ";
	$string .= "Gene:".$self->get_gn." " if ($self->get_gn);
	$string .= "(".$self->get_os.") ".$self->get_ln." AA\n";
	my $sequence = $self->get_sq;
	while ($sequence)
	{
		$string .= substr($sequence, 0, 60)."\n";
		substr($sequence, 0, 60) = '';
	}
	return $string;
}

# Translate to sptrembl format
sub toSptrembl
{
	my $self = shift;
	die "Try to create a fasta file without ID" if (!$self->get_id);
	my $sptrembl = sptrembl->new;
	$sptrembl->set_id($self->get_id);
	$sptrembl->set_ac($self->get_ac);
	$sptrembl->set_gn($self->get_gn);
	$sptrembl->set_sq($self->get_sq);
	return $sptrembl;
}
1;
