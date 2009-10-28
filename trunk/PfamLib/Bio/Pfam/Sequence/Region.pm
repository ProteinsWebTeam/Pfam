package Bio::Pfam::Sequence::Region;

use strict;
use warnings;

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
