# stolen from Bio::EnsEMBL::Pipeline::SeqFetcher::xdget 
# sgj 9/11/04
# - moved into Bio::SeqFetcher space for now
# - inherit from Bio::Root instead of Bio::Ensembl::Root
# - fixed a "my" variable $seqstr masks earlier declaration 
#   error in db method
# - return undef if sequence is not found
# - other stuff from here down is unchanged

# Cared for by EnsEMBL  <ensembl-dev@ebi.ac.uk>
#
# Copyright GRL & EBI
#
# You may distribute this module under the same terms as perl itself
#
# POD documentation - main docs before the code

=pod 

=head1 NAME

Bio::EnsEMBL::Pipeline::SeqFetcher::xdget

=head1 SYNOPSIS

  my $obj = Bio::EnsEMBL::Pipeline::SeqFetcher::xdget->new(
      -executable => '/blah/xdget',
      -db         => '/data/db'
  );
  my $seq = $obj->get_Seq_by_acc($acc);

=head1 DESCRIPTION

Object to retrieve sequences using xdget (Wash U).
Database must be formatted with xdformat. Sequence type (protein
or nucleotide) is guessed, based on file extensions of the database
files. Returns sequence as Bio::Seq.
Additional options for xdget can be specifed though no checking
is performed for compatibility.

Note that, at the time of writing, xdget is case-insensitive:
retrieved sequence is in upper case, irrespective of what was
in the original unformatted fasta file.

=head1 CONTACT

B<ensembl-dev@ebi.ac.uk>

=head1 APPENDIX

The rest of the documentation details each of the object methods. 
Internal methods are usually preceded with a _

=cut

package Bio::SeqFetcher::xdget;

use strict;
use Bio::Root::Root;
use Bio::DB::RandomAccessI;
use Bio::Seq;

use vars qw(@ISA);

@ISA = qw(Bio::Root::Root Bio::DB::RandomAccessI);

sub new {
  my ($class, @args) = @_;
  my $self = bless {}, $class;

  my ($exe, $options, $db) = $self->_rearrange([qw(
    EXECUTABLE OPTIONS DB
  )], @args);

  $exe ||= 'xdget';
  $self->executable($exe);
  $self->options($options) if defined $options;
  $self->db($db) if defined $db;
  
  return $self;
}

=head2 executable

  Title   : executable
  Usage   : $self->executable('/path/to/executable');
  Function: Get/set for the executable to be used by the module.
  Returns : string
  Args    : string

=cut

sub executable {
  my ($self, $exe) = @_;
  if ($exe) {
      $self->{'_exe'} = $exe;
  }
  return $self->{'_exe'};  
}

=head2 db

  Title   : db
  Usage   : $self->db('/path/to/db');
  Function: Get/set for the db to be used by the module.
  Returns : string
  Args    : string

=cut

sub db {
  my ($self, $dbref) = @_;

  if ($dbref) {
      foreach my $db (@$dbref) {
          if (glob "$db.xp?") {
	      $self->_moltype($db, 'p');
          }
          elsif (glob "$db.xn?") {
	      $self->_moltype($db, 'n');
          }
          else {
	      $self->throw("XDF database appears to be missing files");
          }
	  push @{$self->{'_db'}}, $db;
      }
  }
  return $self->{'_db'};  
}

=head2 options

  Title   : options
  Usage   : $self->options('-r');
            Returns reverse complement of nucleotide sequence
  Function: Get/set for options to xdget
  Returns : string
  Args    : string

=cut

sub options {

  my ($self, $options) = @_;
  if ($options) {
      $self->{'_options'} = $options;
  }
  return $self->{'_options'};  

}

=head2 get_Seq_by_acc

  Title   : get_Seq_by_acc
  Usage   : $self->get_eq_by_acc($accession);
  Function: retrieves sequence via xdget
  Returns : Bio::Seq
  Args    : Sequence identifier string

=cut

sub get_Seq_by_acc {
  my ($self, $acc) = @_;
  
  $self->throw("No accession input") unless $acc;
  $self->throw("No database defined") unless $self->db;
  
  my $xdget   = $self->executable;
  my $db      = $self->db;
  local       *FH;
  my $seq;
  my $seqstr;
  my $command;
  my @out;

  # maybe should have some checking here to see if -n/-p have
  # already been specified in options

  DB: foreach my $db (@{$self->db}) {

    my $options = $self->options;

    if ($self->_moltype($db) eq 'n') {
      $options .= " -n";
    }
    else {
      $options .= " -p";
    }

    $command = "$xdget $options $db $acc";

    open FH, "$command 2>&1 |" or $self->throw("Error retrieving $acc from $db with $xdget");
    @out = <FH>;
    close FH;

    last DB if $out[0] !~ /Not found/;
  }

  if( $out[0] =~ /Not found/ ) {
      return undef;
  }

  shift @out;
  $seqstr = join(" ", @out);
  $seqstr =~ s/\s//g;

  $seq = Bio::Seq->new(
    -seq              => $seqstr,
    -display_id       => $acc,
    -accession_number => $acc,
    -desc             => ""
  );
     

  return $seq;
}

sub _moltype {
    my ($self, $db, $type) = @_;

    return undef unless $db;

    if ($type) {
	$self->{'_moltype'}{$db} = $type;
    }
    return $self->{'_moltype'}{$db};
}


1;
