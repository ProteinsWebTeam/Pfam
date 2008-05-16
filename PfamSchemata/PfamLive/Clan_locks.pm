
# $Id: Clan_locks.pm,v 1.4 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $
package PfamLive::Clan_locks;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("clan_locks"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_clan locked user type updated/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_clan" );

#Set up relationships

#1 to many relationship
__PACKAGE__->has_one( "clans" => "PfamLive::Clans",
		       {"foreign.auto_clan"  => "self.auto_clan"},
		       {proxy => [qw/clan_acc clan_id clan_description/],
		        cascade_delete => 0 });

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

