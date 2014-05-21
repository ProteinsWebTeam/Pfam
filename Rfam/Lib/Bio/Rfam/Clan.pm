package Bio::Rfam::Clan;

=head1 NAME

Bio::Rfam::Clan; - a module that acts as a container for an Rfam Clan - which is only
one file, but keeps it in the same model as the other containers.

=head1 DESCRIPTION

Stores the entity objects, in this case a Clan DESC file object. It also includes information
regarding where the data cam from.

=head1 COPYRIGHT

File: Clan.pm 

Copyright (c) 2013: 


Author: Rob Finn (rdf 'at' ebi.ac.uk )
Incept: rdf, May 21, 2014 9:37:47 AM

=cut



#-------------------------------------------------------------------------------

=head1 METHODS

=cut
use strict;
use warnings;
use Moose;
use Moose::Util::TypeConstraints;

use  Bio::Rfam::Clan::DESC;

#-------------------------------------------------------------------------------

=head1 METHODS
=cut

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------

coerce 'Bio::Rfam::Clan::DESC',
  from 'HashRef',
    via { Bio::Rfam::Clan::DESC->new($_);
    };

has 'DESC' => (
  is        => 'ro',
  isa       => 'Bio::Rfam::Clan::DESC',
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
