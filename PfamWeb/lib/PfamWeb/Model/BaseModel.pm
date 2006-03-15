
package PfamWeb::Model::BaseModel;

use strict;
use warnings;

use base "DBIx::Class";
use Sys::Hostname;

my( $dbHost, $dbPort );
if( hostname eq "gromit" ) {
  $dbHost = "127.0.0.1";
  $dbPort = "3336";
} else {
  $dbHost = "pfam";
  $dbPort = "3306";
}

__PACKAGE__->load_components( qw/DB/ );
__PACKAGE__->connection( "dbi:mysql:pfam_19_0:$dbHost:$dbPort",
						 "pfam",
                         "password" );

1;

