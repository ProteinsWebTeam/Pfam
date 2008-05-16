
# Jalview.pm
# jt6 20070726 WTSI
#
# $Id: Jalview.pm,v 1.2 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Family::Alignment::Jalview - view sequence alignments in jalview

=cut

package PfamWeb::Controller::Family::Alignment::Jalview;

=head1 DESCRIPTION

A class to control the viewing of sequence alignments in jalview. The whole
controller is just one action, which hands off to a template that loads up the
applet with the appropriate parameters.

$Id: Jalview.pm,v 1.2 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use base 'PfamWeb::Controller::Family::Alignment';

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 auto : Private

Overrides "auto" from the parent class to avoid checking parameters twice.

=cut

sub auto : Private {
  return 1;
}

#-------------------------------------------------------------------------------

=head2 showJalview : Path

This is the way into the JalView alignment viewer applet.

Hands straight off to a template that generates a "tool" page containing the 
JalView applet.

=cut

sub showJalview : Path {
  my( $this, $c ) = @_;

  $c->stash->{template} = 'components/tools/jalview.tt';
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
