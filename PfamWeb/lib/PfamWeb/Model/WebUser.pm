
# WebUser.pm
# jt6 20060912 WTSI
#
# $Id: WebUser.pm,v 1.3 2008-05-16 15:29:28 jt6 Exp $

=head1 NAME

PfamWeb::Model::WebUser - a wrapper around the web_user database schema

=cut

package PfamWeb::Model::WebUser;

=head1 DESCRIPTION

This is a Model wrapper around the web_user schema. Connection
parameters are specified in the yaml.

$Id: WebUser.pm,v 1.3 2008-05-16 15:29:28 jt6 Exp $

=cut

use utf8;
use strict;
use warnings;

use base qw/Catalyst::Model::DBIC::Schema/;

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
