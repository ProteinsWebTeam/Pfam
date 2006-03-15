package Bio::Pfam::Drawing::Layout::Config::NestedConfig;

use vars qw($AUTOLOAD @ISA $VERSION);
use strict;

use Bio::Pfam::Drawing::Layout::Markup;
use Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig;

@ISA= qw(Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig);


sub configure_Markup {
  my ($self, $markup) = @_; 
  #All generic feature are shown on the top
  $markup->v_align("top");
  #All line colours should be black
  $self->_lineColour($markup);
  $self->_lineStyle($markup);
  $self->_constructLabel($markup);
}

sub _lineStyle {
  my ($self, $markup) = @_;
  $markup->line("mixed");
}
1;
