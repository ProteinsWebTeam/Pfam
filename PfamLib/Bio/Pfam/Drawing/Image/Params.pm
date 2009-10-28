package Bio::Pfam::Drawing::Image::Params;

use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;


#lollipop head dimensions

has 'headSizeCircle' => (
  isa => 'Int',
  is  => 'ro',
  default => '3'
);

has 'headSizeSquare' => (
  isa => 'Int',
  is  => 'ro',
  default => '6'
);

has 'headSizeDiamond' => (
  isa => 'Int',
  is  => 'ro',
  default => '4'
);

has 'headSizeArrow' => (
  isa => 'Int',
  is  => 'ro',
  default => '3'
);

has 'headSizeLine' => (
  isa => 'Int',
  is  => 'ro',
  default => '3'
);

#parameters for adjusting the edges of domains
#default height for a lollipop     
has 'defaultMarkupHeight' => (
  isa => 'Int',
  is  => 'ro',
  default => '20'
);

#step up each lollipop X pixels from the previous one
has 'lollipopToLollipopIncrement' => (
  isa => 'Int',
  is  => 'ro',
  default => '7'
);

#step up each bridge X pixels from the last bridge
has 'bridgeToBridgeIncrement' => (
  isa => 'Int',
  is  => 'ro',
  default => '2'
);


#step up each lollipop X pixels from the previous one
has 'bridgeToLollipopIncrement' => (
  isa => 'Int',
  is  => 'ro',
  default => '5'
);

#number of steps on jagged edge (must be an even integer
has 'largeJaggedSteps' => (
  isa => 'Int',
  is  => 'ro',
  default => '6'
);

#general image parameters
## the height of a region
     
has 'regionHeight' => (
  isa => 'Int',
  is  => 'ro',
  default =>  20
);  

#the height of a motif
has 'motifHeight' => (
  isa => 'Int',
  is  => 'ro',
  default =>  14
);  

#the height of a motif
has 'motifOpacity' => (
  isa => 'Num',
  is  => 'ro',
  default =>  0.6
);  

#Padding for the text label on a region
has 'labelPadding' => (  
  isa => 'Int',
  is  => 'ro',
  default =>  3 );   

#xscale pixels per residue
has 'xscale' => (
  isa => 'Num',
  is  => 'ro',
  default => 0.5 );  

# not currently used
has 'yscale' =>(
  isa => 'Int',
  is  => 'ro',
  default => 1);
     
# opacity of the envelope regions
has 'envOpacity' => (
  isa => 'Num',
  is  => 'ro',
  default =>  0.6 ); 
  
=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program. If not, see <http://www.gnu.org/licenses/>.

=cut

1; 
  
