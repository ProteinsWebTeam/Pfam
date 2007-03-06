
# TT.pm
# jt 20060316 WTSI
#
# $Id: TT.pm,v 1.5 2007-03-06 09:26:07 jt6 Exp $

=head1 NAME

PfamWeb::View::TT - PfamWeb Template Toolkit View

=cut

package PfamWeb::View::TT;

=head1 DESCRIPTION

An empty view class. Everything clever is handled by the controllers or the
templates themselves.

=cut

use strict;
use warnings;

use base 'Catalyst::View::TT';

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
