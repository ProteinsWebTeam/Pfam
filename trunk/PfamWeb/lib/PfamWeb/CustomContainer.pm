
# CustomContainer.pm
# jt6 20061109 WTSI
#
# $Id: CustomContainer.pm,v 1.3 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::CustomContainer - a custom container for HTML::Widgets

=cut

package PfamWeb::CustomContainer;

=head1 DESCRIPTION

A custom container for HTML::Widgets that generates markup which is
more easily laid out using pure CSS. Specifically, we wrap a span.row
around each label/input pair.

$Id: CustomContainer.pm,v 1.3 2008-05-16 15:29:28 jt6 Exp $

=cut

use strict;
use warnings;

use base "HTML::Widget::Container";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 _build_element

Overrides the default method from L<HTML::Widget::Container> and
alters the markup to provide something more easily laid out with CSS.

=cut

sub _build_element {
  my( $this, $e ) = @_;

  return () unless $e;
  return map { $this->_build_element($_) } @{$e} if ref $e eq 'ARRAY';

  my $class = $e->attr('class') || '';

  my $span    = new HTML::Element( "span", class => "row" );
  $span->push_content( $this->label,
					   $e );

  return ( $span );
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
