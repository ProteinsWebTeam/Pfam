package PfamWeb::View::TT;

use strict;
use base 'Catalyst::View::TT';

__PACKAGE__->config( {
					  CONSTANTS => {
									server => "http://www.sanger.ac.uk",
									root   => "/Software/Pfam"
								   },
					  EVAL_PERL => 1,
					 }
				   );

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
