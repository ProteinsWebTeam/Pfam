=head1 NAME

MODULENAME - a module that 

=cut

package Bio::Rfam::Family::Scores;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: Scores.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 28, 2013 5:36:59 PM

=cut

use strict;
use warnings;
use Moose;
use Moose::Util::TypeConstraints;

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'numRegions' => (
  is        => 'ro',
  isa       => 'Int',
  required  => 1
);

has 'regions' => (
  is    => 'ro',
  isa   => 'ArrayRef[ArrayRef]',
  required => 1
);

has 'nres' => (
  is    => 'rw',
  isa   => 'Int',
  required => 0
);

sub determineNres {
  my ($self) = @_;
  use DDP;
  my $res = 0;
  foreach my $r (@{$self->regions}){
    if($r->[1] <= $r->[2]){
      $res += (($r->[2] - $r->[1]) + 1);
    }else{
      #Reverse strand
      $res += (($r->[1] - $r->[2]) + 1);
    }
  }
  
  $self->nres($res);
  return($self->nres);
}


__PACKAGE__->meta->make_immutable;

1;
