package RfamWeb::View::Email;

use strict;
use base 'Catalyst::View::Email';

__PACKAGE__->config(
    stash_key => 'email'
);

=head1 NAME

RfamWeb::View::Email - Email View for RfamWeb

=head1 DESCRIPTION

View for sending email from RfamWeb. 

=head1 AUTHOR

John Tate

=head1 SEE ALSO

L<RfamWeb>

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;