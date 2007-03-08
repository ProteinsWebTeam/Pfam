
# DasProxy.pm
# jt6 20070307 WTSI
#
# $Id: DasProxy.pm,v 1.1 2007-03-08 14:14:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::DasProxy - a simple proxy server for DAS calls

=cut

package PfamWeb::Controller::DasProxy;

=head1 DESCRIPTION

A simple implementation of a proxy server for DAS calls. The aim of this is to
allow javascript methods from our pages to access DAS servers outside of our
domain.

$Id: DasProxy.pm,v 1.1 2007-03-08 14:14:58 jt6 Exp $

=cut

use strict;
use warnings;

use base "Catalyst::Controller";

#-------------------------------------------------------------------------------

=head1 METHODS

=head2 default : Private

Default method stub.

=cut

sub default : Private {
  my ( $this, $c ) = @_;

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
