
# $Author: jt6 $


package Bio::Pfam::Drawing::Layout::Config::PfamaConfig;

use strict;
use warnings;
use Convert::Color;

use Moose;
use Moose::Util::TypeConstraints;
use Data::Dump qw( dump );

subtype 'fixedColours'
  => as 'HashRef'
  => where { ref $_ eq 'HASH' }
  => message { 'not a valid colours hash' };

coerce 'fixedColours'
  => from 'Str'
    => via { 
      my $json = new JSON();
      my $colours = $json->decode( $_ );
      foreach my $key ( keys %$colours ) {
        $colours->{$key} = Convert::Color->new( 'rgb8:' . join( ',', @{$colours->{$key}} ) );
      }
      return $colours;
    }
  => from 'HashRef'
    => via {
      return $_;
    };

has 'assignedColours' => (
  isa    => 'fixedColours',
  is     => 'rw',
  coerce => 1
);

has 'colourIndex' => (
  isa     => 'Int',
  is      => 'rw',
  default => '0'
);

has 'preDeterminedColours' => (
  isa   => 'ArrayRef[Str]',
  is    => 'rw',
  
  default => sub { [ qw(2DCF00 FF5353 5B5BFF EBD61D BA21E0 FF9C42 FF7DFF B9264F BABA21 C48484 1F88A7 CAFEB8 4A9586 CEB86C) ] }
# default => sub { [ qw(2DCF00 FF5353 5B5BFF EBD61D 9A1CB9 FF9C42 FF7DFF B9264F BABA21 C48484 1F88A7 CAFEB8 4A9586 CEB86C) ] }
#green red blue yellow purple orange cyan fuchsia	maroon olive brown teal bluegreen brown
);

sub configureRegion {
  my ($self, $region) = @_;
  # set up the shape type


  
  #As we do not knw what sort of region this is we can nt construct a url
  $self->_setEdges($region);
  #Now contruct the label
  $self->_constructLabel($region);
  $self->_constructHref($region);
  #Now Colour the Region
#  print STDERR "***Setting the colour***\n";
  $self->_setColour($region);
}

sub _constructLabel{
  my ($self, $region) = @_;
  $region->text($region->metadata->identifier);
}

sub _constructHref{
  my ($self, $region) = @_;
  if($region->metadata->accession){
    $region->href('/family/'.$region->metadata->accession);  
  }
}
 
#This sets the generic region to a dark grey colour
sub _setColour{
  my ($self, $region) = @_;
#  print STDERR "****This Colour****\n";
  my $colour;
  if($self->assignedColours and $self->assignedColours->{$region->metadata->accession}){
    $colour = $self->assignedColours->{$region->metadata->accession};
  }elsif($self->preDeterminedColours->[$self->colourIndex]){
    $colour = Convert::Color->new( 'rgb8:'.$self->preDeterminedColours->[$self->colourIndex]);
    if ( defined $self->assignedColours ) {
      my $c = $self->assignedColours;
      $c->{$region->metadata->accession} = $colour;
    }
    else {
      $self->assignedColours( { $region->metadata->accession => $colour } );
    }
#    $self->assignedColours ? $self->assignedColours->{$region->metadata->accession} = $colour : $self->assignedColours( {$region->metadata->accession => $colour}) ;
    $self->colourIndex($self->colourIndex + 1 );
  }else{
    #randomly generate
    
    my @hex;
    for (my $x = 0; $x < 3; $x++) {
      my $rand = rand(255);
      $hex[$x] = sprintf ("%x", $rand);
      if ($rand < 9) {
        $hex[$x] = "0" . $hex[$x];
      }
      if ($rand > 9 && $rand < 16) {
        $hex[$x] = "0" . $hex[$x];
      }
    }
    $colour =  Convert::Color->new( 'rgb8:' . $hex[0] . $hex[1] . $hex[2]);
    if ( defined $self->assignedColours ) {
      my $c = $self->assignedColours;
      $c->{$region->metadata->accession} = $colour;
    }
    else {
      $self->assignedColours( { $region->metadata->accession => $colour } );
    }
#    $self->assignedColours ? $self->assignedColours->{$region->metadata->accession} = $colour : $self->assignedColours( {$region->metadata->accession => $colour});
  }
  $region->colour( $colour );
}

sub _setEdges{
  my ($self, $region) = @_;
  
  #A small image does not have ends, so we do not need to set them
  if($region->metadata->type eq 'Family' or $region->metadata->type eq 'Domain'){
    if($region->end - $region->start > 50){
      if($region->modelStart == 1){
        $region->startStyle( 'curved' );
      }else{
        $region->startStyle( 'jagged' );  
      }
    
      if($region->modelEnd == $region->modelLength){
        $region->endStyle( 'curved' );
      }else{
        $region->endStyle( 'jagged' );  
      }  
    }else{
      $region->startStyle( 'straight' );
      $region->endStyle( 'straight' );
    }   
  }elsif($region->metadata->type eq 'Repeat' or $region->metadata->type eq 'Motif'){
    $region->startStyle( 'straight' );
    $region->endStyle( 'straight' );
  }else{
    die "Unkown pfama metadata type\n";  
  } 
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

