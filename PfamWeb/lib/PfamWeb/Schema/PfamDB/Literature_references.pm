package PfamWeb::Schema::PfamDB::Literature_references;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("literature_references"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_lit medline title author journal/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_lit" );

#Set up relationships

#1 to many relationship


__PACKAGE__->has_one( "clan_lit_refs" => "PfamWeb::Schema::PfamDB::Clan_lit_refs",
		       {"foreign.auto_lit"  => "self.auto_lit"});


1;
