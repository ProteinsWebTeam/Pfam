
# $Id: Clans.pm,v 1.5 2008-05-16 15:23:16 jt6 Exp $
#
# $Author: jt6 $

package PfamLive::Clans;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("clans"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_clan clan_acc clan_id clan_description clan_author clan_comment/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_clan" );

#Set up relationships

#1 to many relationship
__PACKAGE__->has_many( "clan_membership" => "PfamLive::Clan_membership",
		       {"foreign.auto_clan"  => "self.auto_clan"});

__PACKAGE__->has_many( "clan_lit_refs" => "PfamLive::Clan_lit_refs",
		       {"foreign.auto_clan"  => "self.auto_clan"});

__PACKAGE__->has_many( "clan_database_links" => "PfamLive::Clan_database_links",
		       {"foreign.auto_clan"  => "self.auto_clan"});

__PACKAGE__->has_many( "clan_versions" => "PfamLive::Clan_versions",
                      {"foreign.auto_clan" => "self.auto_clan"});

__PACKAGE__->might_have( "clan_locks" => "Pfamlive::Clan_locks",
                      {"foreign.auto_clan"  => "self.auto_clan"},
                      {proxy => [qw/locked user type updated/],
                      cascade_delete => 0});

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

