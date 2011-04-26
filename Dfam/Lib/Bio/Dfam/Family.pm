# DfamA.pm
#
# Author:        rdf
# Maintainer:    $Id: DfamA.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Nov 29, 2008
# Last Modified: $Date: 2009-10-08 12:27:28 $
=head1 NAME

Template - a short description of the class

=cut

package Bio::Dfam::Family;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: DfamA.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: DfamA.pm

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

 This is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License
 as published by the Free Software Foundation; either version 2
 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.
 
 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 or see the on-line version at http://www.gnu.org/copyleft/gpl.txt
 
=cut

use strict;
use warnings;

use Moose;
#use Moose::Util::TypeConstraints;
use Bio::Dfam::Types qw(AlignDfam HMMDfam TableDfam DESCDfam);
#-------------------------------------------------------------------------------

=head1 METHODS
=cut

#-------------------------------------------------------------------------------
#- attributes ------------------------------------------------------------------
#-------------------------------------------------------------------------------


has 'ALIGN' => (
  is       => 'ro',
  isa      => AlignDfam,
  required => 1,
  #coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'SEED' => (
  is       => 'ro',
  isa      => AlignDfam,
  required => 1,
  #coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'HMM' => (
  is       => 'ro',
  isa      => HMMDfam,
  required => 1,
  #coerce   => 1,
  #handles  => [ qw( length each_seq write_stockholm) ],
);

has 'TABLE' => (
  is       => 'ro',
  isa      => TableDfam,
  required => 1,
);

has 'DESC' => (
  is        => 'ro',
  isa       => DESCDfam,
  required  => 1,
  #coerce    => 1
);

has 'source' => (
  is    => 'ro',
  isa   => 'Str',#enum ([ qw(database file svn) ]),
);

has 'rdb' => (
  is => 'rw',
  isa => 'HashRef'
);

__PACKAGE__->meta->make_immutable;
1;
