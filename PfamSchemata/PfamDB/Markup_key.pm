package PfamSchemata::PfamDB::Markup_key;

use strict;
use warnings;
use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "markup_key" );

#Get the columns that we want to keep
__PACKAGE__->add_columns(qw/auto_markup label/);

__PACKAGE__->set_primary_key("auto_markup");

__PACKAGE__->has_many("auto_markup" => "PfamSchemata::PfamDB::Pfamseq_markup",
		     {"foreign.auto_markup" => "self.auto_markup"});

1;
