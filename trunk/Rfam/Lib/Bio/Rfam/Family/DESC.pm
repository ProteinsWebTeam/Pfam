# DESC.pm
#
# $Author$
# $Id$
# $Revision$
# $Date$
#
=head1 NAME

Bio::Rfam::Motif::DESC - An object to represent a Motif DESC file

=cut

package Bio::Rfam::Motif::DESC;

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

#-------------------------------------------------------------------------------

=head1 METHODS

=cut

has 'AC' => (
  is        => 'rw',
  required  => 0
);

has 'ID' => (
  is        => 'rw',
  required  => 0 
);  

has 'DE' => (
  is        => 'ro',
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

has 'TX' => (
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
  required  => 0
);

has 'WIKI' => (
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
  
has 'BM' => (
  is    => 'rw',
  isa   => 'Str'
);

has 'CB' => (
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
  default => sub {[ qw(AC ID PI DE AU SE SS CUTGA CUTTC CUTNC TP TX BM CB SM CL DBREFS REFS CC WIKI SQ private ) ] }
);

has 'requiredFields' => (
  is    => 'ro',
  isa   => 'ArrayRef',
  default => sub {[ qw(ID DE AU CUTGA CUTTC CUTNC BM CB SM SE SS REFS WIKI) ]}
);

has 'defaultButIllegalFields' => (
  is    => 'ro',
  isa   => 'HashRef',
  default => sub {{ 'ID' => 'ShortName', 'DE' => 'Family description', 'AU' => 'Who RU', 'SE' => 'Where did the seed come from' }}
);




__PACKAGE__->meta->make_immutable;
1;
