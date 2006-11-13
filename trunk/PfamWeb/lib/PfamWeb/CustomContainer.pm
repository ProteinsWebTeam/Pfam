
# CustomContainer.pm
# jt6 20061109 WTSI
#
# $Id: CustomContainer.pm,v 1.1 2006-11-13 14:36:03 jt6 Exp $

=head1 NAME

PfamWeb::CustomContainer - a custom container for HTML::Widgets

=cut

package PfamWeb::CustomContainer;

=head1 DESCRIPTION

A custom container for HTML::Widgets that generates markup which is
more easily laid out using pure CSS. Specifically, we wrap a span.row
around each label/input pair.

$Id: CustomContainer.pm,v 1.1 2006-11-13 14:36:03 jt6 Exp $

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

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
