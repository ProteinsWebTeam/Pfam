
# $Author: jt6 $

package Bio::Pfam::Drawing::Colour::rgbColour;

use strict;
use warnings;

sub new {
  my $caller = shift;
  my %params = @_;
  my $self = bless {}, ref($caller) || $caller;

  my $red = ($params{'-r'}||$params{'-R'});
  my $green = ($params{'-g'}||$params{'-g'});
  my $blue = ($params{'-b'}||$params{'-b'});
  
  $self->red($red);
  $self->green($green);
  $self->blue($blue);
  
  return $self;
}

sub red {
  my ($self, $red) = @_;
  if($red){
    if($red =~ /\d+/){
      $self->{'r'} = $red
    }else{
      warn "This is not integer, $red, setting red channel to 0\n";
      $self->{'r'}= "0";
    }
  }
  return $self->{'r'};
}

sub green {
  my ($self, $green) = @_;
  if($green){
    if($green =~ /\d+/){
      $self->{'g'} = $green
    }else{
      warn "This is not integer, $green, setting green channel to 0\n";
      $self->{'g'}= "0";
    }
  }
  return $self->{'g'};
}

sub blue {
  my ($self, $blue) = @_;
  if($blue){
    if($blue =~ /\d+/){
      $self->{'b'} = $blue
    }else{
      warn "This is not integer, $blue, setting blue channel to 0\n";
      $self->{'b'}= "0";
    }
  }
  return $self->{'b'};
}


sub colour2XMLDOM{
  my ($self, $dom) = @_;
  my $colour_element = $dom->createElement("colour");
  my $colour = $dom->createElement("RGB");
  $colour_element->appendChild($colour);
  $colour->setAttribute("R", $self->red);
  $colour->setAttribute("G", $self->green);
  $colour->setAttribute("B", $self->blue);
  return $colour_element;
}

1;
