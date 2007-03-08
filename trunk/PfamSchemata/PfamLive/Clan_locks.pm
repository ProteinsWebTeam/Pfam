
# $Id: Clan_locks.pm,v 1.2 2007-03-08 14:16:23 jt6 Exp $
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

1;
