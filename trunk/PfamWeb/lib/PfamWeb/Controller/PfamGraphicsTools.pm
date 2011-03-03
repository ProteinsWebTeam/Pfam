
# PfamGraphicsTools.pm
# jt 20070402 WTSI
#
# $Id: PfamGraphicsTools.pm,v 1.9 2009-03-20 15:56:08 jt6 Exp $

=head1 NAME

PfamWeb::Controller::PfamGraphicsTools - tools for drawing Pfam graphics

=cut

package PfamWeb::Controller::PfamGraphicsTools;

=head1 DESCRIPTION

A couple of utility methods for generating Pfam graphics from a user-supplied
JSON string and for displaying the graphic for a given Uniprot sequence.

$Id: PfamGraphicsTools.pm,v 1.9 2009-03-20 15:56:08 jt6 Exp $

=cut

use strict;
use warnings;

use base 'Catalyst::Controller';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 generate_graphic : Global

Displays the tools for generating Pfam graphics using the JS library.

=cut

sub generate_graphic : Global {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamGraphicsTools::generateGraphic: generating form' )
    if $c->debug;
  $c->stash->{template} = 'pages/generate_graphic.tt';

}

#-------------------------------------------------------------------------------

=head2 generate_uniprot_graphic : Global

Displays the graphic for a particular UniProt entry.

=cut

sub generate_uniprot_graphic : Global {
  my ( $this, $c ) = @_;

  $c->log->debug( 'PfamGraphicsTools::generate_uniprot_graphic: generating form' )
    if $c->debug;
  $c->stash->{template} = 'pages/generate_uniprot_graphic.tt';
}

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

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
