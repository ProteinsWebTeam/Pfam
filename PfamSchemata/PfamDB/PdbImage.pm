
package PfamDB::PdbImage;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb_image" );
__PACKAGE__->add_columns( qw/auto_pdb pdb_image pdb_image_sml/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_one( "pdb" => "PfamDB::Pdb" );

1;

