
package PfamWeb::Model::PdbAuthor;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb_author" );
__PACKAGE__->add_columns( qw/auto_pdb author_order last_name name_initials/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_one(    "pdb" => "PfamWeb::Model::PdbNew",
			 { "foreign.auto_pdb" => "self.auto_pdb" } );




1;

