=head1 NAME

MODULENAME - a module that 

=cut

package Bio::Rfam::Motif::CM;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: CM.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 25, 2013 8:32:16 AM

=cut

use strict;
use warnings;
use Moose;
use Bio::Rfam::Types ':all';
use Moose::Util::TypeConstraints;
#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'cmHeader' => (
  is        => 'ro',
  isa       => 'HashRef',
  required  => 1
);

has 'cmBody' => (
  is        => 'ro',
  isa       => 'ArrayRef',
  required  => 1
);

has 'hmmHeader' => (
  is        => 'ro',
  isa       => 'HashRef',
  required  => 1
);

has 'hmmBody' => (
  is        => 'ro',
  isa       => 'ArrayRef',
  required  => 1
);

has 'rawcm' => (
  is        => 'ro',
  isa       => 'ArrayRef',
  required  => 1
);


has 'match_pair_node' => (
  is  => 'ro',
  isa => 'Bool',
  required => 1
);

has 'is_calibrated' => (
  is  => 'ro',
  isa => 'Bool',
  required => 1,
  default => 0
);

#-------------------------------------------------------------------------------

=head2 setName

  Title    : setName
  Incept   : EPN, Wed May 28 13:33:33 2014
  Usage    : Bio::Rfam::Family::CM::setName($name)
  Function : Set the name field for a CM and for its filter HMM
  Args     : $self:  Bio::Rfam::Family::CM object
           : $name:  new name for the CM
  Returns  : void
  Dies     : if $name is not a single token (no spaces)

=cut

sub setName { 
  my ( $self, $name) = @_;

  if($name !~ m/^\S+$/) { die "ERROR, trying to set CM name to invalid string, string must have no whitespace"; }

  $self->{cmHeader}->{name}  = $name;
  $self->{hmmHeader}->{name} = $name;

  return;
}

#-------------------------------------------------------------------------------

=head2 setAccession

  Title    : setAccession
  Incept   : EPN, Wed May 28 13:37:25 2014
  Usage    : Bio::Rfam::Family::CM::setAccession($acc)
  Function : Set the accession field for a CM and for its filter HMM
  Args     : $self:  Bio::Rfam::Family::CM object
           : $acc:   new accession for the CM
  Returns  : void

=cut

sub setAccession { 
  my ( $self, $acc) = @_;

  if($acc !~ m/^\S+$/) { die "ERROR, trying to set CM accession to invalid string, string must have no whitespace"; }

  $self->{cmHeader}->{acc}  = $acc;
  $self->{hmmHeader}->{acc} = $acc;

  return;
}

#-------------------------------------------------------------------------------

=head2 setDescription

  Title    : setDescription
  Incept   : EPN, Wed May 28 13:37:28 2014
  Usage    : Bio::Rfam::Family::CM::setDescription($desc)
  Function : Set the description field for a CM and for its filter HMM
  Args     : $self:  Bio::Rfam::Family::CM object
           : $desc:  new description for the CM
  Returns  : void

=cut

sub setDescription { 
  my ( $self, $desc) = @_;

  $self->{cmHeader}->{desc}  = $desc;
  $self->{hmmHeader}->{desc} = $desc;

  return;
}

#-------------------------------------------------------------------------------

=head2 setGA

  Title    : setGA
  Incept   : EPN, Wed May 28 13:37:28 2014
  Usage    : Bio::Rfam::Family::CM::setGA($ga)
  Function : Set the GA field for a CM (not for its filter HMM)
  Args     : $self:  Bio::Rfam::Family::CM object
           : $ga:    new GA threshold for a CM
  Returns  : void

=cut

sub setGA { 
  my ( $self, $ga) = @_;

  $self->{cmHeader}->{hitGA} = $ga;
  # do not update the filter p7 HMM's GA threshold, it's not relevant

  return;
}

#-------------------------------------------------------------------------------

=head2 setTC

  Title    : setTC
  Incept   : EPN, Wed May 28 13:37:28 2014
  Usage    : Bio::Rfam::Family::CM::setTC($tc)
  Function : Set the TC field for a CM (not for its filter HMM)
  Args     : $self:  Bio::Rfam::Family::CM object
           : $tc:    new TC threshold for a CM
  Returns  : void

=cut

sub setTC { 
  my ( $self, $tc) = @_;

  $self->{cmHeader}->{hitTC} = $tc;
  # do not update the filter p7 HMM's TC threshold, it's not relevant

  return;
}

#-------------------------------------------------------------------------------

=head2 setNC

  Title    : setNC
  Incept   : EPN, Wed May 28 13:37:28 2014
  Usage    : Bio::Rfam::Family::CM::setNC($nc)
  Function : Set the NC field for a CM (not for its filter HMM)
  Args     : $self:  Bio::Rfam::Family::CM object
           : $nc:    new NC threshold for a CM
  Returns  : void

=cut

sub setNC { 
  my ( $self, $nc) = @_;

  $self->{cmHeader}->{hitNC} = $nc;
  # do not update the filter p7 HMM's NC threshold, it's not relevant

  return;
}

#-------------------------------------------------------------------------------

1;
