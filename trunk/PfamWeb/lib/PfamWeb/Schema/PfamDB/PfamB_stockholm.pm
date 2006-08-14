package PfamWeb::Schema::PfamDB::PfamB_stockholm;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamB_stockholm" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamB stockholm_data/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamB");

#Now setup the relationship 

#pfamB_stockholm joins are to pfamB

__PACKAGE__->has_one( "pfamB" => "PfamWeb::Schema::PfamDB::PfamB",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" },
		      {proxy => [ qw/pfamB_id pfamB_acc/]});
1;
