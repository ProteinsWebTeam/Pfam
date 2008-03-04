
use strict;
use warnings;

use Test::More tests => 4;

use_ok( 'RfamDB', 'got RfamDB definition' );

my $schema;
ok( $schema = RfamDB->connect( 'dbi:mysql:database=rfam_8_1_new_website;host=pfamdb2a:port=3301',
                               'pfam',
                               'mafp1' ), 'got a schema' );

# need to use this method if we don't have a primary key defined in the model
#my @rs;
#ok( @rs = $schema->resultset('Version')->search, 'got VERSION data' );
#my $version_data = shift @rs;

my $version_data;
ok( $version_data = $schema->resultset('Version')->find({}), 'got VERSION data' );

is( $version_data->rfam_release, '8.1', 'correct release' );

