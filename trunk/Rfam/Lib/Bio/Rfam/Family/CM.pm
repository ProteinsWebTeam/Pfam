=head1 NAME

MODULENAME - a module that 

=cut

package Bio::Rfam::Family::CM;

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

has 'hmmHeader' => (
  is        => 'ro',
  isa       => 'HashRef',
  required  => 1
);

has 'match_pair_node' => (
  is  => 'ro',
  isa => 'Bool',
  required => 1
);

1;