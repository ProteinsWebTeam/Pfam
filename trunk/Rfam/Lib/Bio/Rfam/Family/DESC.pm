# DESC.pm
#
# $Author$
# $Id$
# $Revision$
# $Date$
#
=head1 NAME

Bio::Rfam::Family::DESC - An object to represent a DESC file

=cut

package Bio::Rfam::Family::DESC;

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: DESC.pm

Copyright (c) 2010, Howard Hughes Medical Institute, All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.
   2. Redistributions in binary form must reproduce the above copyright notice, 
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.
   3. Neither the name of the Howard Hughes Medical Institute nor the names of 
   its contributors may be used to endorse or promote products derived from 
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY 
IMPLIED WARRANTIES OF MERCHANTABILITY, NON-INFRINGEMENT, OR FITNESS FOR A 
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, 
OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; REASONABLE ROYALTIES; 
OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING 
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE. 

=cut

use strict;
use warnings;

use Moose;
use Bio::Rfam::Types ':all';
use Moose::Util::TypeConstraints;

#use Bio::Rfam::Types qw(RfamAcc RfamId RfamDesc RfamAuthor RfamClanAcc hmmCutOff RfamRef faslseDiscoveryRate);
#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'AC' => (
  is        => 'rw',
  isa       => RfamAcc,
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

has 'SE' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 0
);

has 'SS' => (
  is        => 'ro',
  isa       => 'Str',
  required  => 0
);

has 'CUTGA' => (
  is        => 'rw',
  isa       => CmCutOff,
  required  => 0
);

has 'CUTTC' => (
  is        => 'rw',
  isa       => CmCutOff,
  required  => 0
);

has 'CUTNC' => (
  is        => 'rw',
  isa       => CmCutOff,
  required  => 0
);

has 'CL' => (
  is       => 'rw',
  isa      => RfamClanAcc,
  required => 0
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


has 'MSP' => (
  is        => 'ro',
  isa       => 'HashRef'
);

has 'REFS' => (
  is        => 'ro',
  isa       => RfamRef
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
  isa   => 'ArrayRef'
);

has 'SM' => (
  is    => 'rw',
  isa   => 'Str'
);

has 'SQ' => (
  is    => 'rw',
  isa   => 'Int'
);

has 'SN' => (
  is    => 'rw',
  isa   => 'Str'
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
  default => sub {[ qw(ID AC PI DE AU SE SS CUTGA CUTTC CUTNC BM SM MSP TP SN WIKI CL REFS DBREFS CC SQ EDITS private ) ] }
);

__PACKAGE__->meta->make_immutable;
1;
