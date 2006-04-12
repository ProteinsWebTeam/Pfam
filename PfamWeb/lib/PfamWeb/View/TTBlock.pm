
# TTBlock.pm
# jt 20060410 WTSI
#
# Extend the basic TT.pm to allow us to override the WRAPPER
# configuration and thereby avoid outputting all the HTML boilerplate
# around page fragments.
#
# $Id: TTBlock.pm,v 1.1 2006-04-12 16:27:48 jt6 Exp $

package PfamWeb::View::TTBlock;

use strict;
use base 'PfamWeb::View::TT';

__PACKAGE__->config( {
					WRAPPER => ""
				   } );

=head1 NAME

PfamWeb::View::TT - Catalyst TT View

=head1 SYNOPSIS

See L<PfamWeb>

=head1 DESCRIPTION

Catalyst TT View.

=head1 AUTHOR

John Tate,,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
