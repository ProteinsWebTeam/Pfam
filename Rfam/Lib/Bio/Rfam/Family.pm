package Bio::Rfam::Family;

=head1 NAME

Bio::Rfam::Family; - a module that acts as a container for an Rfam family

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: Family.pm 

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

use  Bio::Rfam::Family::MSA;
use  Bio::Rfam::Family::CM;
use  Bio::Rfam::Family::Scores;
use  Bio::Rfam::Family::TBLOUT;

#-------------------------------------------------------------------------------

=head1 METHODS
=cut

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------

subtype 'Bio::Rfam::Family::SEED' => as class_type('Bio::Rfam::Family::MSA');

coerce 'Bio::Rfam::Family::SEED',
  from 'HashRef',
    via { Bio::Rfam::Family::MSA->new($_);};

coerce 'Bio::Rfam::Family::TBLOUT',
  from 'HashRef',
    via { Bio::Rfam::Family::TBLOUT->new($_);
    };

has 'SEED' => (
  is       => 'ro',
  isa      => 'Bio::Rfam::Family::SEED',
  required => 1,
  coerce   => 1
);

has 'CM' => (
  is       => 'ro',
  isa      => 'Bio::Rfam::Family::CM',
  required => 0,
);

has 'SCORES' => (
  is       => 'rw',
  isa      => 'Bio::Rfam::Family::Scores',
  required => 0,
);

has 'TBLOUT' => (
  is       => 'ro',
  isa      => 'Bio::Rfam::Family::TBLOUT',
  required => 0,
  coerce   => 1
);

has 'DESC' => (
  is        => 'ro',
  isa       => 'Bio::Rfam::Family::DESC',
  required  => 1,
  #coerce    => 1
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
