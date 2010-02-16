package PfamViewer::View::TT;

use strict;
use base 'Catalyst::View::TT';

__PACKAGE__->config(
      TEMPLATE_EXTENSION => '.tt',
      WRAPPER => 'components/wrapper.tt'  
	);

=head1 NAME

PfamViewer::View::TT - TT View for PfamViewer

=head1 DESCRIPTION

TT View for PfamViewer. 

=head1 AUTHOR

=head1 SEE ALSO

L<PfamViewer>

Prasad Gunasekaran,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
