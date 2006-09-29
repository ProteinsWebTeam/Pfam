package PfamSchemata::PfamDB::PfamB;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );

#Set up the table
__PACKAGE__->table( "pfamB" );

#Get the columns that we want to keep
__PACKAGE__->add_columns( qw/auto_pfamB pfamB_acc pfamB_id/);

#Now set up the primary keys/contraints
__PACKAGE__->set_primary_key("auto_pfamB", "pfamB_acc");

#Now setup the relationship 

#PfamB joins are to pfamB_reg, pdbmap & pfamB_stockholm 

__PACKAGE__->has_many( pfamb_reg => "PfamSchemata::PfamDB::PfamB_reg",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" } );

__PACKAGE__->has_many( pdbMap => "PfamSchemata::PfamDB::PdbMap",
			  { "foreign.auto_pfam"  => "self.auto_pfamB" },
			  { proxy => [ qw/pdb_id/ ] } );

__PACKAGE__->has_one( pfamB_stock => "PfamSchemata::PfamDB::PfamB_stockholm",
		      { "foreign.auto_pfamB"  => "self.auto_pfamB" },
		      { proxy => qw[/stockholm_data/] } );
1;
