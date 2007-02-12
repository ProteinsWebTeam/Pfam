package PfamLive::Clan_lit_refs;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("clan_lit_refs"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_clan auto_lit order_added comment/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_clan", "auto_lit" );

#Set up relationships

#1 to many relationship
__PACKAGE__->has_one( "clans" => "PfamLive::Clans",
		       {"foreign.auto_clan"  => "self.auto_clan"});

__PACKAGE__->has_one( "literature" => "PfamLive::Literature_references",
		       {"foreign.auto_lit"  => "self.auto_lit"},
		       {proxy => [qw/medline title author journal/]});


1;
