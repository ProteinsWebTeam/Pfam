
# JSON.pm
# jt 20090302 WTSI
#
# $Id: JSON.pm,v 1.1 2009-03-19 14:07:55 jt6 Exp $

=head1 NAME

PfamWeb::View::JSON - PfamWeb JSON view

=cut

package PfamWeb::View::JSON;

=head1 DESCRIPTION

Wrapper for the JSON view. Patterned on the example here:

  http://blog.afoolishmanifesto.com/archives/1273

which shows, amongst other things, how to make the view use L<JSON::XS> and
configure it to do C<convert_blessed>.

=cut

# use strict;
# use warnings;
# 
# use base 'Catalyst::View::JSON';

use Moose;
extends 'Catalyst::View::JSON';

use JSON ();

#-------------------------------------------------------------------------------

has encoder => (
  is => 'ro',
  lazy_build => 1,
);

sub _build_encoder {
  my $this = shift;
  return JSON->new->utf8->convert_blessed;
}

sub encode_json {
  my ( $this, $c, $data ) = @_;
  $this->encoder->encode( $data );
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
