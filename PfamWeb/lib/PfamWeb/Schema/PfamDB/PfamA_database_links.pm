package PfamWeb::Schema::PfamDB::PfamA_database_links;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("pfamA_database_links"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_pfamA db_id comment db_link other_params/); # The columns that we want to have access to
__PACKAGE__->set_primary_key( "auto_pfamA");

#Set up relationships

#1 to 1 relationship that must be there.....
__PACKAGE__->has_one( "pfamA" => "PfamWeb::Schema::PfamDB::Pfam",
		       {"foreign.auto_pfamA"  => "self.auto_pfamA"},
		      {proxy => [ qw/pfamA_id pfamA_acc/]});




1;
