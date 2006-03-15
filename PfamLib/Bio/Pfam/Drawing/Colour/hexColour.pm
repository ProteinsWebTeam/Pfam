package Bio::Pfam::Drawing::Colour::hexColour;

use strict;

sub new {
  my $caller = shift;
  my %params = @_;
  my $self = bless {}, ref($caller) || $caller;
  my $colour = ($params{'-colour'}||$params{'-COLOUR'});
  $self->colour($colour);
  return $self;
}

sub colour{
  my ($self, $colour) = @_;
  if($colour){
    if($colour =~ /#?(\S+)/){
      $self->{'colour'} = $1
    }else{
      warn "This is not a HEX colour string:$colour, setting to black\n";
      $self->{'colour'}= "000000";
    }
  }
  return $self->{'colour'};
}

sub colour2XMLDOM{
  my ($self, $dom) = @_;
  my $colour_element = $dom->createElement("colour");
  my $colour = $dom->createElement("hex");
  $colour_element->appendChild($colour);
  $colour->setAttribute("hexcode", $self->colour);
  return $colour_element;
}
1;
