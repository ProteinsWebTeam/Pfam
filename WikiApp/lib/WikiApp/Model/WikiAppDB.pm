
# WikiAppDB.pm
# jt6 20100420 WTSI
#
# $Id$

=head1 NAME

WikiApp::Model::WikiApprove - a wrapper around the wiki_approve database
schema

=cut

package WikiApp::Model::WikiAppDB;

=head1 DESCRIPTION

The model for the database is defined in a L<DBIx::Class::Schema>,
C<WikiApprove>. This is a wrapper around that schema, which imports
auto-generates the L<Catalyst::Model> classes at runtime, using the
schema classes to define the components.

$Id$

=cut

use strict;
use warnings;

use base qw/Catalyst::Model::DBIC::Schema/;

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Rob Finn (rdf@sanger.ac.uk), 
         Paul Gardner (pg5@sanger.ac.uk)

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
