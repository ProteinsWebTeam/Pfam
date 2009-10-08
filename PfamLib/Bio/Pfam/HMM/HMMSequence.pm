
package Bio::Pfam::HMM::HMMSequence;

use Moose;
use Moose::Util::TypeConstraints;

extends 'Bio::Pfam::HMM::HMMMatch';


has sumEvalue => (
  isa      => 'evalue',
  is       => 'rw',
);


has H2mode => (
  isa => 'Str',
  is  => 'rw'
);

has sumScore => (
  isa      => 'Num',
  is       => 'rw',
);

has desc => (
  isa      => 'Str',
  is       => 'rw',
  required => 1
);

has numberHits => (
  isa      => 'Int',
  is       => 'rw',
  required => 1
);



has 'exp' => (
  isa => 'Num',
  is  => 'rw'
);


has hmmUnits => (
  isa => "ArrayRef[ Bio::Pfam::HMM::HMMUnit ]",
  is  => 'rw',
  default => sub { [] }
);


#-------------------------------------------------------------------------------
=head1 Subroutines

=head2 addHMMUnit 

  Title    : addHMMUnit
  Usage    : $hmmseq->addHMMUnit($hmmUnit) 
  Function : Adds a hmmUnit to a sequence. It checks that the variable passed in is a Bio::Pfam::HMM::HMMUnit oject
  Args     : A Bio::Pfam::HMM::HMMUnit oject
  Returns  : Nothing
  
=cut

sub addHMMUnit {
  my ( $self, $hmmUnit ) = @_;
  if($hmmUnit->isa("Bio::Pfam::HMM::HMMUnit")){
    push(@{$self->hmmUnits}, $hmmUnit);
  }else{
    warn "$hmmUnit is not a Bio::Pfam::HMM::HMMUnit, not added\n"; 
  }
}


  __PACKAGE__->meta->make_immutable;
1;
