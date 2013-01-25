=head1 NAME

Bio::Rfam::Family::MSA - a module that reads in a Rfam SEED or ALIGN file.

=cut

package Bio::Rfam::Family::MSA;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: MSA.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 25, 2013 8:29:29 AM

=cut

use strict;
use warnings;
use Moose;
use MooseX::NonMoose;
use Moose::Util::TypeConstraints;

extends 'Bio::Easel::MSA';

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'aliType' => (
  is       => 'ro',
  isa      => enum([qw[ full seed ]]),
  required => 1,
);

#has 'type' => (
#  is       => 'ro',
#  isa      => 'Str',
#  required => 1,
#);

1;