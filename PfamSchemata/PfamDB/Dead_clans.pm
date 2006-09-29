package PfamSchemata::PfamDB::Dead_clans;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "dead_clans" );

#Add the columns
__PACKAGE__->add_columns(qw/clan_acc clan_description clan_membership comment forward_to user killed/);

#Set up the primary key
__PACKAGE__->set_primary_key("clan_acc");

#In theory the forward_to could join onto self or clans...... -todo

1;
