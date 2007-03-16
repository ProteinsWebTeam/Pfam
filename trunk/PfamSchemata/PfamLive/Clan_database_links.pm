
# $Id: Clan_database_links.pm,v 1.4 2007-03-16 11:25:20 jt6 Exp $
#
# $Author: jt6 $
package PfamLive::Clan_database_links;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("clan_database_links"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_clan db_id comment db_link other_params/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_clan");

#Set up relationships

#1 to 1 relationship that must be there.....
__PACKAGE__->has_one( "clans" => "PfamLive::Clans",
		       {"foreign.auto_clan"  => "self.auto_clan"},
		       {cascade_delete => 0});

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

