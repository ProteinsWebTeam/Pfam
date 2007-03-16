
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::GenericMarkupConfig;

use strict;
use warnings;

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

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;

