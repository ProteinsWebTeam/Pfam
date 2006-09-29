
package PfamSchemata::PfamDB::Clans;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("clans"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_clan clan_acc clan_id clan_description clan_author clan_comment/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_clan" );

#Set up relationships

#1 to many relationship
__PACKAGE__->has_many( "clan_membership" => "PfamSchemata::PfamDB::Clan_membership",
		       {"foreign.auto_clan"  => "self.auto_clan"});

__PACKAGE__->has_many( "clan_lit_refs" => "PfamSchemata::PfamDB::Clan_lit_refs",
		       {"foreign.auto_clan"  => "self.auto_clan"});

__PACKAGE__->has_many( "clan_database_links" => "PfamSchemata::PfamDB::Clan_database_links",
		       {"foreign.auto_clan"  => "self.auto_clan"});



#		       {proxy => [qw/pfamA_id pfamA_acc/]});

#? to 1 or many - ask jt6
#__PACKAGE__->might_have();

1;
