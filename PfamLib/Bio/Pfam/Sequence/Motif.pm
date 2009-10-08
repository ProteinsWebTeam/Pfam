package Bio::Pfam::Sequence::Motif;

use Moose;
use Moose::Util::TypeConstraints;

extends 'Bio::Pfam::Sequence::Feature';

has 'type' => (
  isa => enum([ qw(pfamb sig_p transmembrane coiled_coil low_complexity )]),
  is  => 'rw'
);


sub clone {
  my ( $self ) = @_;  
  my %params;
  
  foreach my $attribute (qw(start end label display href colour 
                            metadata type )){
    if(defined($self->$attribute)){
      $params{$attribute} = $self->$attribute;
    }
  }
  
  my $copy = Bio::Pfam::Sequence::Motif->new(%params);
  return $copy;
}


__PACKAGE__->meta->make_immutable;
1;
