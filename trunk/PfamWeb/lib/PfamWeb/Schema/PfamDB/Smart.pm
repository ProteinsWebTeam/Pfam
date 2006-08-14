package PfamWeb::Schema::PfamDB::Smart;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "smart" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_smart smart_acc smart_id/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_smart", "smart_acc");

#Now setup the relationship 

#PfamB joins are to pfamB_reg, pdbmap & pfamB_stockholm 

__PACKAGE__->has_many( "smart_reg" =>  "PfamWeb::Schema::PfamDB::Smart_reg",
		      { "foreign.auto_smart"  => "self.auto_smart" });


1;
