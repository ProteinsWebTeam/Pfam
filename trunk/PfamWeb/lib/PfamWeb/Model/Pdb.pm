
package PfamWeb::Model::Pdb;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb" );
__PACKAGE__->add_columns( qw/auto_pdb pdb_id header title/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_many(    "pdbMap" => "PfamWeb::Model::PdbMap",
						 { "foreign.auto_pdb" => "self.auto_pdb" } );

__PACKAGE__->might_have( "image"  => "PfamWeb::Model::PdbImage",
						 { "foreign.auto_pdb" => "self.auto_pdb" },
						 { proxy => [ qw/pdb_image/ ] } );

1;

