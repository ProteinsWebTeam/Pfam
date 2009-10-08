package Bio::Pfam::Sequence::Region;

use Convert::Color;
use Moose;
use Moose::Util::TypeConstraints;

extends 'Bio::Pfam::Sequence::Feature';

enum 'Style' => qw( curved jagged straight arrow );


has 'startStyle' => (
  isa => 'Style',
  is  => 'rw',
); 

has 'endStyle' => (
  isa => 'Style',
  is  => 'rw'
);


has 'aliStart' => (
  isa => 'Int',
  is  => 'rw'
);

has 'aliEnd' => (
  isa => 'Int',
  is  => 'rw'
);

has 'modelStart' => (
  isa => 'Int',
  is  => 'rw'
);

has 'modelEnd' => (
  isa => 'Int',
  is  => 'rw'
);
has 'modelLength' => (
  isa => 'Int',
  is  => 'rw'
);

has 'text' => (
  isa => 'Str',
  is  => 'rw'
);

has 'type' => (
  isa => 'Str',
  is  => 'rw',
  required => 1,
  default => 'unknown'
);


sub clone {
  my ( $self ) = @_;  
  my %params;
  
  foreach my $attribute (qw(start end label display href colour 
                            metadata startStyle endStyle aliStart aliEnd
                            modelStart modelEnd modelLength text type )){
    if(defined($self->$attribute)){
      $params{$attribute} = $self->$attribute;
    }
    
  }
  
  my $copy = Bio::Pfam::Sequence::Region->new(%params);
  return $copy;
}



__PACKAGE__->meta->make_immutable;

1;
