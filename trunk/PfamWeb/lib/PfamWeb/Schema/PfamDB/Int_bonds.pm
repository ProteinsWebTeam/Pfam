package PfamWeb::Schema::PfamDB::Int_bonds;

use strict;
use warnings;

use base "DBIx::Class";


__PACKAGE__->load_components( qw/Core/); #Do we want to add DB
__PACKAGE__->table("int_bonds"); # This is how we define the table
__PACKAGE__->add_columns( qw/auto_int_bonds bond_name/); # The columns that we want to have access to

#Set up the primary keys
__PACKAGE__->set_primary_key( "auto_int_bonds", "bond_name" );

#Set up relationships
#1 to many relationship

__PACKAGE__->has_many( "int_atoms" => "PfamWeb::Schema::PfamDB::Int_atoms",
		      {"foreign.auto_int_bonds"  => "self.auto_int_bonds"});

1;
