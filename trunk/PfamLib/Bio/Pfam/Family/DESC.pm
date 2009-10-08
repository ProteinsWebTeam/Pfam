# DESC.pm
#
# Author:        finnr
# Maintainer:    $Id: DESC.pm,v 1.1 2009-10-08 12:27:28 jt6 Exp $
# Version:       $Revision: 1.1 $
# Created:       Nov 30, 2008
# Last Modified: $Date: 2009-10-08 12:27:28 $
=head1 NAME

Bio::Pfam::Family::DESC - An object to represent a DESC file

=cut

package Bio::Pfam::Family::DESC;

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
use Moose::Util::TypeConstraints;

use Bio::Pfam::HMM::HMM;
#-------------------------------------------------------------------------------

=head1 METHODS

=cut

#subtype 'DescId'
#  => as Str
#  => where { $_ =~ m/^[\w_]{1,15}$/ }
#  => message { 'Not a valid Pfam-A accession' };

subtype 'PfamAuthor'
  => as Str
  => where { $_ =~ /^((\S+\s{1}\S{1,3}|SMART|LOAD)(\,\s{1})?)*$/ }
  => message { 'Not a vailid Pfam author' };

subtype 'PfamDesc'
  => as Str
  => where { $_ =~ /^(.{1,75})$/ }
  => message { 'Not a vailid Pfam author' };

subtype 'PfamRef'
  => as ArrayRef
  => where{ scalar(@{$_}) >= 1; }
  => message { 'Not a vaild Pfam Reference' }; 

subtype 'hmmCutOff'
  => as HashRef 
  => where { defined $_->{seq} and defined $_->{dom} }
  => message { 'expected hash containing  seq and dom as keys '.dumper($_) };  
  


has 'AC' => (
  is        => 'rw',
  isa       => 'PfamAcc',
  required  => 0
);

has 'ID' => (
  is        => 'rw',
  isa       => 'PfamId',
  required  => 0
  
);  

has 'DE' => (
  is        => 'ro',
  isa       => 'PfamDesc',
  required  => 1
);


has 'PI' => (
  is        => 'rw',
  isa       => 'Str',
  required  => 0
);

has 'AU' => (
  is        => 'ro',
  isa       => 'PfamAuthor',
  required  => 1
);

has 'SE' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 1
);

has 'CL' => (
  is       => 'rw',
  isa      => 'Str',
  required => 0
);

has 'CUTGA' => (
  is        => 'rw',
  isa       => 'hmmCutOff',
  required  => 1
);

has 'CUTTC' => (
  is        => 'rw',
  isa       => 'hmmCutOff',
  required  => 1
);

has 'CUTNC' => (
  is        => 'rw',
  isa       => 'hmmCutOff',
  required  => 1
);

has 'TP' => (
  is        => 'ro',
  isa       =>  enum( [ qw( Family Domain Repeat Motif ) ]),
  required  => 1
);

has 'REFS' => (
  is        => 'ro',
  isa       => 'PfamRef'
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
  default => sub {[ qw(ID AC PI DE AU SE CUTGA CUTTC CUTNC BM SM TP CL NESTS REFS DBREFS CC SQ EDITS private ) ] }
);


__PACKAGE__->meta->make_immutable;
1;
