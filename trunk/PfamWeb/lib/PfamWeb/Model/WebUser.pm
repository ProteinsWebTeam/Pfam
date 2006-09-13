
# WebUser.pm
# jt6 20060912 WTSI
#
# $Id: WebUser.pm,v 1.1 2006-09-13 08:36:48 jt6 Exp $

=head1 NAME

PfamWeb::Model::WebUser - a wrapper around the web_user database schema

=cut

package PfamWeb::Model::WebUser;

=head1 DESCRIPTION

This is a Model wrapper around the web_user schema. Connection
parameters are specified in the yaml.

$Id: WebUser.pm,v 1.1 2006-09-13 08:36:48 jt6 Exp $

=cut

use strict;
use warnings;

use base qw/Catalyst::Model::DBIC::Schema/;

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
