
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::SwisspredictedactivesiteConfig;

use vars qw($AUTOLOAD @ISA $VERSION);
use strict;
use warnings;

use Bio::Pfam::Drawing::Layout::Markup;
use Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig;

@ISA= qw(Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig);

sub configure_Markup {
  my ($self, $markup) = @_; 
  #All generic feature are shown on the top
  $markup->v_align("bottom");
  #All line colours should be black
  $self->_lineColour($markup);
  $self->_lineStyle($markup);
  #lolly-pop
  $self->_headStyle($markup);
  $self->_headColour($markup);
  $self->_constructLabel($markup);
}

sub _lineStyle {
  my ($self, $markup) = @_;
  $markup->line("bold");
}

sub _headStyle{
  my ($self, $markup) = @_;
  $markup->head("diamond");
}

sub _headColour{
  my ($self, $markup) = @_;
  my $colour = Bio::Pfam::Drawing::Colour::hexColour->new('-colour' => "FF66FF");
  $markup->head_colour($colour);
}

sub _constructLabel{
  my ($self, $markup) = @_;
  $markup->label($markup->BioSeqFeature->display_name) if($markup->BioSeqFeature->display_name);
}

1;
