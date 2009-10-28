package Bio::Pfam::Sequence::Motif;

use strict;
use warnings;

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
