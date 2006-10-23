package PfamDB::PfamB_database_links;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/); #Do we want to add DB

__PACKAGE__->table("pfamB_database_links"); # This is how we define the table

__PACKAGE__->add_columns( qw/auto_pfamB db_id comment db_link other_params/); # The columns that we want to have access to

__PACKAGE__->set_primary_key( "auto_pfamB");

#Set up relationships

#1 to 1 relationship that must be there.....
__PACKAGE__->has_one( pfamB => "PfamDB::PfamB",
					  { "foreign.auto_pfamB" => "self.auto_pfamB" },
					  { proxy => [ qw/pfamB_id pfamB_acc/] } );

1;
