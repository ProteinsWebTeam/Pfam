package PfamWeb::Schema::PfamDB::Clan_database_links;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("clan_database_links"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_clan db_id comment db_link other_params/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_clan");

#Set up relationships

#1 to 1 relationship that must be there.....
__PACKAGE__->has_one( "clans" => "PfamWeb::Schema::PfamDB::Clans",
		       {"foreign.auto_clan"  => "self.auto_clan"});




1;
