
# iPfamDB.pm
# jt6 20060808 WTSI
#
# $Id: iPfamDB.pm,v 1.2 2008-03-19 15:11:40 jt6 Exp $

=head1 NAME

iPfamWeb::Model::PfamDB - a wrapper around the pfam database schema

=cut

package iPfamWeb::Model::iPfamDB;

=head1 DESCRIPTION

The model for the database is defined in a L<DBIx::Class::Schema>,
C<PfamDB>. This is a wrapper around that schema, which imports
auto-generates the L<Catalyst::Model> classes at runtime, using the
schema classes to define the components.

$Id: iPfamDB.pm,v 1.2 2008-03-19 15:11:40 jt6 Exp $

=cut

use strict;
use warnings;

use base 'Catalyst::Model::DBIC::Schema';

#-------------------------------------------------------------------------------

=head1 AUTHOR

John Tate, C<jt6@sanger.ac.uk>

Rob Finn, C<rdf@sanger.ac.uk>

=head1 COPYRIGHT

Copyright (c) 2007: Genome Research Ltd.

Authors: Rob Finn (rdf@sanger.ac.uk), John Tate (jt6@sanger.ac.uk)

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
or see the on-line version at http://www.gnu.org/copyleft/gpl.txt

=cut

1;
