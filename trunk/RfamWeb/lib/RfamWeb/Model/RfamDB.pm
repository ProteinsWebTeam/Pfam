
# RfamDB.pm
# jt6 20080306 WTSI
#
# $Id: RfamDB.pm,v 1.2 2008-06-17 09:17:15 jt6 Exp $

=head1 NAME

RfamWeb::Model::RfamDB - a wrapper around the rfam database schema

=cut

package RfamWeb::Model::RfamDB;

=head1 DESCRIPTION

The model for the database is defined in a L<DBIx::Class::Schema>,
C<PfamDB>. This is a wrapper around that schema, which imports
auto-generates the L<Catalyst::Model> classes at runtime, using the
schema classes to define the components.

$Id: RfamDB.pm,v 1.2 2008-06-17 09:17:15 jt6 Exp $

=cut

use strict;
use warnings;

use base 'Catalyst::Model::DBIC::Schema';

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Paul Gardner, C<pg5@sanger.ac.uk>

Jennifer Daub, C<jd7@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: John Tate (jt6@sanger.ac.uk), Paul Gardner (pg5@sanger.ac.uk),
         Jennifer Daub (jd7@sanger.ac.uk)

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