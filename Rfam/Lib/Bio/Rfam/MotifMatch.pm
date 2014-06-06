package Bio::Rfam::MotifMatch;

# MotifMatch.pm

#Bio::Rfam::MotifMatch - An object to represent a match between a CM and a sequence

=head1 DESCRIPTION

A more detailed description of what this class does and how it does it.

=head1 COPYRIGHT

File: MotifMatch.pm

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

has 'MOTIF_ACC' => (
  is        => 'rw',
  required  => 0 
);  

has 'RFAM_ACC' => (
  is        => 'rw',
  required  => 0
);

has 'RFAMSEQ_ID' => (
  is        => 'rw',
  isa       =>  'Str',
  required  => 0
);

has 'RFAMSEQ_ACC' => (
  is        => 'rw',
  isa       => 'Str',
  required  => 0
);

has 'RFAMSEQ_START' => (
  is        => 'rw',
  isa       => 'Str',
  required  => 0
);

has 'RFAMSEQ_STOP' => (
  is        => 'rw',
  isa       => 'Str',
  required  => 0
);

has 'QUERY_START' => (
  is        => 'rw',
  isa       => 'Int',
  required  => 0
);

has 'QUERY_STOP' => (
  is        => 'rw',
  isa       => 'Int',
  required  => 0
);

has 'MOTIF_START' => (
  is        => 'rw',
  isa       => 'Int',
  required  => 0
);

has 'MOTIF_STOP' => (
  is        => 'rw',
  isa       => 'Int',
  required  => 0
);

has 'E_VALUE' => (
  is        => 'rw',
  required  => 0
);

has 'BIT_SCORE' => (
  is        => 'rw',
  required  => 0
);

has 'order' => (
  is    => 'ro',
  isa   => 'ArrayRef',
  default => sub {[ qw(MATCH_ACC RM_ACC RFAM_ACC RFAMSEQ_ACC QUERY_START QUERY_STOP MOTIF_START MOTIF_STOP E_VALUE BIT_SCORE ) ] }
);

__PACKAGE__->meta->make_immutable;
1;
