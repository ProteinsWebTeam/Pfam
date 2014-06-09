package Bio::Rfam::Motif;

=head1 NAME

Bio::Rfam::Motif; - a module that acts as a container for a Motif

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: Motif.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk or finnr 'at' janelia.hhmi.org)
Incept: finnr, Jan 24, 2013 9:37:47 PM

=cut



#-------------------------------------------------------------------------------

=head1 METHODS

=cut
use strict;
use warnings;
use Moose;
use Moose::Util::TypeConstraints;

use  Bio::Rfam::Motif::CM;
use  Bio::Rfam::Motif::DESC;
use  Bio::Rfam::Motif::MSA;

#-------------------------------------------------------------------------------

=head1 METHODS
=cut

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------

subtype 'Bio::Rfam::Motif::SEED' => as class_type('Bio::Rfam::Motif::MSA');

coerce 'Bio::Rfam::Motif::SEED',
  from 'HashRef',
    via { Bio::Rfam::Motif::MSA->new($_);};

coerce 'Bio::Rfam::Motif::DESC',
  from 'HashRef',
    via { Bio::Rfam::Motif::DESC->new($_);
    };


has 'SEED' => (
  is       => 'ro',
  isa      => 'Bio::Rfam::Motif::SEED',
  required => 1,
  coerce   => 1
);

has 'CM' => (
  is       => 'rw',
  isa      => 'Bio::Rfam::Motif::CM',
  required => 0,
);

has 'DESC' => (
  is        => 'ro',
  isa       => 'Bio::Rfam::Motif::DESC',
  required  => 1,
  coerce    => 1
);

has 'source' => (
  is    => 'ro',
  isa   => enum([ qw(database file svn) ]),
);

has 'rdb' => (
  is => 'rw',
  isa => 'HashRef'
);

__PACKAGE__->meta->make_immutable;
1;
