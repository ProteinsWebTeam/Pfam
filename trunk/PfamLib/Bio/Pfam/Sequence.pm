package Bio::Pfam::Sequence;

use strict;
use warnings;

use Moose;
use Moose::Util::TypeConstraints;

subtype 'HighlightType'
  => as HashRef
  => where {
    ( exists $_->{start}     and 
      exists $_->{end}       and 
      $_->{start} =~ /^\d+$/ and 
      $_->{end}   =~ /^\d+$/ and 
      $_->{end} >= $_->{start} );
  }
  => message { 'Not a valid highlight specification' };

has 'metadata' => (
  isa => 'Bio::Pfam::Sequence::MetaData',
  is  => 'rw',
);

has 'type' => (
  isa => enum([qw( protein DNA RNA )]),
  is  => 'rw'
);

has 'length' => (
  isa => 'Int',
  is  => 'rw',
  required => 1
);

has 'highlight' => (
  isa => 'HighlightType',
  is  => 'rw'
);

has 'regions' => (
  isa => 'ArrayRef[ Bio::Pfam::Sequence::Region ]',
  is  => 'rw'
);

has 'markups' => (
  isa => 'ArrayRef[ Bio::Pfam::Sequence::Markup ]',
  is  => 'rw'
);

has 'motifs' => (
  isa => 'ArrayRef[ Bio::Pfam::Sequence::Motif ]',
  is  => 'rw'
);


has 'options' => (
  isa => 'Bio::Pfam::Drawing::Image::Options',
  is  => 'rw'
);

has 'imageParams' => (
  isa => 'Bio::Pfam::Drawing::Image::Params',
  is  => 'rw'
);

sub eachRegionOrMotifOfType {
  my( $self, $type ) = @_;

  my @typeRegions;
  foreach my $r (@{ $self->regions }){
    if($r->type eq $type){
      push(@typeRegions, $r);
    }
  }
  
  foreach my $m (@{ $self->motifs }){
    if($m->type eq $type){
      push(@typeRegions, $m);
    }
  }
 
  return(\@typeRegions);
}

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
