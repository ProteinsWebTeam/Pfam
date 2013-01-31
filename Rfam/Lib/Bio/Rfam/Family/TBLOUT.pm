=head1 NAME

MODULENAME - a module that 

=cut

package Bio::Rfam::Family::TBLOUT;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: TBLOUT.pm 

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

has 'fileLocation' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 1
);

__PACKAGE__->meta->make_immutable;

1;
