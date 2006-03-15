package Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig;

use strict;
use Bio::Pfam::Drawing::Layout::Markup;
use Bio::Pfam::Drawing::Colour::hexColour;
use Bio::Pfam::Drawing::Colour::rgbColour;

sub new {
  my $caller = shift;
  my $self = bless {}, ref($caller) || $caller;
  return $self;
}

sub configure_Markup {
  my ($self, $markup) = @_; 
  
  #All generic feature are shown on the top
  $markup->v_align("top");
  #All line colours should be black
  $self->_lineColour($markup);
  $self->_lineStyle($markup);

  if(!$markup->end){
    #lolly-pop
    $self->_headStyle($markup);
    $self->_headColour($markup);
  }


  $self->_constructLabel($markup);
}

sub _lineStyle {
  my ($self, $markup) = @_;
  $markup->line("bold");
}

sub _lineColour {
  my ($self, $markup) = @_;
  #All line colours should be black
  my $colour = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "333333");
  $markup->line_colour($colour);
}

sub _headStyle{
  my ($self, $markup) = @_;
  $markup->head("diamond");
}

sub _headColour{
  my ($self, $markup) = @_;
  my $colour = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "000000");
  $markup->head_colour($colour);
}

sub _constructLabel{
  my ($self, $markup) = @_;
  $markup->label($markup->BioSeqFeature->display_name) if($markup->BioSeqFeature->display_name);
}

1;
