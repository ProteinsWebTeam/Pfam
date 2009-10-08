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

1;
