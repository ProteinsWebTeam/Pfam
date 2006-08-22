
package PfamWeb::Schema::PfamDB::Pdb;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb_new" );
__PACKAGE__->add_columns( qw/auto_pdb pdb_id header title date resolution
							 experiment_short experiment_long pubmed_id/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_many( "pdbAuthor" => "PfamWeb::Schema::PfamDB::PdbAuthor",
		       { "foreign.auto_pdb" => "self.auto_pdb" });

__PACKAGE__->has_many( "pdbResidueData" => "PfamWeb::Schema::PfamDB::PdbResidueData",
		       { "foreign.auto_pdb" => "self.auto_pdb" });

__PACKAGE__->has_many( "pdbMap" => "PfamWeb::Schema::PfamDB::PdbMap",
			  { "foreign.auto_pdb" => "self.auto_pdb" } );

__PACKAGE__->might_have( "image"  => "PfamWeb::Schema::PfamDB::PdbImage",
			 { "foreign.auto_pdb" => "self.auto_pdb" },
			 { proxy => [ qw/pdb_image/ ] } );

1;

