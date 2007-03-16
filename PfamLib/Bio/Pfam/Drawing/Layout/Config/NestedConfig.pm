
# $Author: jt6 $

package Bio::Pfam::Drawing::Layout::Config::NestedConfig;

use vars qw($AUTOLOAD @ISA $VERSION);
use strict;
use warnings;


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

