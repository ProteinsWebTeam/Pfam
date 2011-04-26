# DESC.pm
#
# Author:        finnr
# Maintainer:    $Id: DESC.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Nov 30, 2008
# Last Modified: $Date: 2009-10-08 12:27:28 $
=head1 NAME

Bio::Dfam::Family::DESC - An object to represent a DESC file

=cut

package Bio::Dfam::Family::DESC;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

$Id: DESC.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $

=head1 COPYRIGHT

File: DESC.pm

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
use Bio::Dfam::Types qw(DfamAcc DfamId DfamDesc DfamAuthor DfamClanAcc
hmmCutOff DfamRef);
#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'AC' => (
  is        => 'rw',
  isa       => DfamAcc,
  required  => 0
);

has 'ID' => (
  is        => 'rw',
  isa       => DfamId,
  required  => 0 
);  

has 'DE' => (
  is        => 'ro',
  isa       => DfamDesc,
  required  => 0
);


has 'PI' => (
  is        => 'rw',
  isa       => 'Str',
  required  => 0
);

has 'AU' => (
  is        => 'ro',
  isa       => DfamAuthor,
  required  => 0
);

has 'SE' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 0
);

has 'CL' => (
  is       => 'rw',
  isa      => DfamClanAcc,
  required => 0
);

has 'CUTGA' => (
  is        => 'rw',
  isa       => hmmCutOff,
  required  => 0
);

has 'CUTTC' => (
  is        => 'rw',
  isa       => hmmCutOff,
  required  => 0
);

has 'CUTNC' => (
  is        => 'rw',
  isa       => hmmCutOff,
  required  => 0
);

has 'TP' => (
  is        => 'ro',
  isa       => 'Str', 
  #enum( [ qw( Family Domain Repeat Motif ) ]),
  required  => 0
);

has 'WIKI' => (
  is        => 'ro',
  isa       => 'HashRef'
);

has 'REFS' => (
  is        => 'ro',
  isa       => DfamRef
);

has 'CC' => (
  is        => 'ro',
  isa       => 'Str',
);

has 'EDITS' => (
  is       => 'rw',
  isa      => 'ArrayRef[ HashRef ]'
);

has 'NESTS' => (
  is       =>  'rw',
  isa      =>  'ArrayRef[ HashRef ]'
);
  
has 'BM' => (
  is    => 'rw',
  isa   => 'Str'
);

has 'SM' => (
  is    => 'rw',
  isa   => 'Str'
);

has 'SQ' => (
  is    => 'rw',
  isa   => 'Int'
);

has 'DBREFS' => (
  is     => 'ro',
  isa    => 'ArrayRef[ HashRef ]'
);

has 'private' => (
  is    => 'ro',
  isa   => 'Str'
);

has 'order' => (
  is    => 'ro',
  isa   => 'ArrayRef',
  default => sub {[ qw(ID AC PI DE AU SE CUTGA CUTTC CUTNC BM SM TP WIKI CL NESTS REFS DBREFS CC SQ EDITS private ) ] }
);


#__PACKAGE__->meta->make_immutable;
1;
