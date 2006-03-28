
package PfamWeb::Model::Interpro;

use strict;
use warnings;

use base "PfamWeb::Model::BaseModel";

__PACKAGE__->load_components( qw/Core/ );
__PACKAGE__->table( "interpro" );
__PACKAGE__->add_columns( qw/auto_pfamA interpro_id abstract/ );
__PACKAGE__->set_primary_key( "auto_pfamA" );

__PACKAGE__->has_one( "pfamA" => "PfamWeb::Model::Pfam",
		      {"foreign.auto_pfamA"  => "self.auto_pfamA" },
		      { proxy => [ qw/ pfamA_acc pfamA_id / ] } );
1;

