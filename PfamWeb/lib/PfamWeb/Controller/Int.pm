# Int.pm
# jt6 20070308 WTSI
#
# $Id: Int.pm,v 1.4 2007-03-08 14:14:58 jt6 Exp $

=head1 NAME

PfamWeb::Controller::Int - controller for the iPfam section

=cut

package PfamWeb::Controller::Int;

=head1 DESCRIPTION

Controller to build the main iPfam page.

$Id: Int.pm,v 1.4 2007-03-08 14:14:58 jt6 Exp $

=cut

use strict;
use warnings;

use base "PfamWeb::Controller::Section";

# define the name of the section...
__PACKAGE__->config( SECTION => "ipfam" );

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
