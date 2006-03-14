
package PfamWeb::Model::BaseModel;

use strict;
use warnings;

use base "DBIx::Class";

__PACKAGE__->load_components( qw/DB/ );
__PACKAGE__->connection( "dbi:mysql:pfam_19_0:pfam:3306",
						 "pfam",
                         "password" );

1;

