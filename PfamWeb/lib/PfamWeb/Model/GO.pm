
package PfamWeb::Model::GO;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "gene_ontology" );
__PACKAGE__->add_columns( qw/auto_pfamA go_id term category/ );
__PACKAGE__->set_primary_key( "auto_pfamA" );

__PACKAGE__->has_one( "pfamA" => "PfamWeb::Model::Pfam" );
1;

