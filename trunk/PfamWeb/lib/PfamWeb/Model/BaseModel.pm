
package PfamWeb::Model::BaseModel;

use strict;
use warnings;

use base "DBIx::Class";
use Sys::Hostname;

my $dbHost;
if( hostname eq "gromit" ) {
  $dbHost = "127.0.0.1";
} else {
  $dbHost = "pfam";
}

__PACKAGE__->load_components( qw/DB/ );
__PACKAGE__->connection( "dbi:mysql:pfam_19_0:$dbHost:3306",
						 "pfam",
                         "password" );

1;

