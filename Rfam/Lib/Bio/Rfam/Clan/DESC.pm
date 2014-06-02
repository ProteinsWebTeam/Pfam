# DESC.pm
#
# $Author$
# $Id$
# $Revision$
# $Date$
#
=head1 NAME

Bio::Rfam::Clan::DESC - An object to represent a clandesc file

=cut

package Bio::Rfam::Clan::DESC;

=head1 DESCRIPTION

Stores the contents of a CLANDESC file as an object.  Most of the Moose
attributes are gets/sets for the CLANDESC fields. There are also the
default contents for the dummy CLANDESC file.

=head1 COPYRIGHT

File: DESC.pm

=cut

use strict;
use warnings;
use Moose;
use Bio::Rfam::Types ':all';
use Moose::Util::TypeConstraints;

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'AC' => (
  is        => 'rw',
  isa       => RfamClanAcc,
  required  => 0
);

has 'ID' => (
  is        => 'rw',
  isa       => RfamId,
  required  => 0 
);  

has 'DE' => (
  is        => 'ro',
  isa       => RfamDesc,
  required  => 0
);


has 'PI' => (
  is        => 'rw',
  isa       => 'Str',
  required  => 0
);

has 'AU' => (
  is        => 'ro',
  isa       => RfamAuthor,
  required  => 0
);

has 'REFS' => (
  is        => 'ro',
  isa       => RfamRef
);

has 'CC' => (
  is        => 'ro',
  isa       => 'Str',
);
  
has 'DBREFS' => (
  is     => 'ro',
  isa    => 'ArrayRef[ HashRef ]'
);

has 'MEMB' => (
  is        => 'ro',
  isa       => 'ArrayRef'
);

has 'private' => (
  is    => 'ro',
  isa   => 'Str'
);

has 'order' => (
  is    => 'ro',
  isa   => 'ArrayRef',
  default => sub {[ qw(AC ID PI DE AU DBREFS REFS CC MEMB private ) ] }
);

has 'requiredFields' => (
  is    => 'ro',
  isa   => 'ArrayRef',
  default => sub {[ qw(ID DE AU) ]}
);

has 'defaultButIllegalFields' => (
  is    => 'ro',
  isa   => 'HashRef',
  default => sub {{ 'ID' => 'ShortName', 'DE' => 'Clan description', 'AU' => 'Who RU' }}
);

__PACKAGE__->meta->make_immutable;
1;
