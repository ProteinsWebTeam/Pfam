
package Bio::Dfam::HMM::HMMSequence;

use strict;
use warnings;

use Moose;
#use Moose::Util::TypeConstraints;
use Bio::Dfam::Types ':all';

#extends 'Bio::Dfam::HMM::HMMMatch';
has 'sequenceEvalue',
  #isa       => EvalueStr, 
  isa       => 'Str',
  is        => 'rw';

has 'bits' => (
  isa => 'Str',
  is  => 'rw',
  required => 1
);

has 'name' => (
  isa => 'Str',
  is  => 'rw',
  required => 1
);

has bias => (
  isa => 'Num',
  is  => 'rw'
);

has sumEvalue => (
  isa      => EvalueStr,
  is       => 'rw',
);

has H2mode => (
  isa => 'Str'  ,
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
  isa => "ArrayRef[ Bio::Dfam::HMM::HMMUnit ]",
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
  if($hmmUnit->isa("Bio::Dfam::HMM::HMMUnit")){
    push(@{$self->hmmUnits}, $hmmUnit);
  }else{
    warn "$hmmUnit is not a Bio::Dfam::HMM::HMMUnit, not added\n"; 
  }
}


  __PACKAGE__->meta->make_immutable;

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1;
