
# $Id: PfamB_database_links.pm,v 1.2 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamLive::PfamB_database_links;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/); #Do we want to add DB

__PACKAGE__->table("pfamB_database_links"); # This is how we define the table

__PACKAGE__->add_columns( qw/auto_pfamB db_id comment db_link other_params/); # The columns that we want to have access to

__PACKAGE__->set_primary_key( "auto_pfamB");

#Set up relationships

#1 to 1 relationship that must be there.....
__PACKAGE__->has_one( pfamB => "PfamLive::PfamB",
					  { "foreign.auto_pfamB" => "self.auto_pfamB" },
					  { proxy => [ qw/pfamB_id pfamB_acc/] } );

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

