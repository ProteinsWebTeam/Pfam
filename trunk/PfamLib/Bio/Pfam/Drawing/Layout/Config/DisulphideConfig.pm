
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::DisulphideConfig;

use vars qw($AUTOLOAD @ISA $VERSION);
use strict;
use warnings;


use Bio::Pfam::Drawing::Layout::Markup;
use Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig;

@ISA= qw(Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig);

sub configure_Markup {
  my ($self, $markup) = @_; 
  $markup->v_align("top");
  $self->_lineColour($markup);
  $self->_lineStyle($markup);
  $self->_constructLabel($markup);
}

sub _lineStyle {
  my ($self, $markup) = @_;
  $markup->line("dash");
}



1;
