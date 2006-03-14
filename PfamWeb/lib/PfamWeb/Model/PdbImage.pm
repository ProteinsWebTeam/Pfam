
package PfamWeb::Model::PdbImage;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "pdb_image" );
__PACKAGE__->add_columns( qw/auto_pdb pdb_image/ );
__PACKAGE__->set_primary_key( "auto_pdb" );

__PACKAGE__->has_one( "pdb" => "PfamWeb::Model::Pdb" );

1;

