
package PfamDB::PdbAuthor;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb_author" );
__PACKAGE__->add_columns( qw/auto_pdb author_order last_name name_initials/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_one(    "pdb" => "PfamDB::PdbNew",
			 { "foreign.auto_pdb" => "self.auto_pdb" } );




1;

