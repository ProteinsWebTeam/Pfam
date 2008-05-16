
# Das_sources.pm
# jt6 20060428 WTSI
#
# Model for the das_sources table.
#
# $Id: Das_sources.pm,v 1.5 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package WebUser::Das_sources;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

# set up the table
__PACKAGE__->table( "das_sources" );

# get the columns that we want to keep
__PACKAGE__->add_columns( qw/ server_id name url system helper_url default_server /);

# set up the primary keys/contraints
__PACKAGE__->set_primary_key( "server_id" );

# no relationships

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
