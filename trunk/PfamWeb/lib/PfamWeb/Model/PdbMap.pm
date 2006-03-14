
package PfamWeb::Model::PdbMap;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdbmap" );
__PACKAGE__->add_columns( qw/auto_pdb auto_pfam chain pdb_start_res pdb_end_res/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_many( "pfam" => "PfamWeb::Model::Pfam",
					   { "foreign.auto_pfamA" => "self.auto_pfam" } );

__PACKAGE__->has_one( "pdb"  => "PfamWeb::Model::Pdb",
					  { "foreign.auto_pdb"   => "self.auto_pdb" },
					  { proxy => [ qw/pdb_id header title image/ ] } );

1;

