
use strict;
use warnings;

use Test::More tests => 5;

use_ok( 'RfamDB', 'got RfamDB definition' );

my $schema;
ok( $schema = RfamDB->connect( 'dbi:mysql:database=rfam_8_1_new_website;host=pfamdb2a:port=3301',
                               'pfam',
                               'mafp1' ), 'got a schema' );
$schema->storage()->debug( 1 );

my $rs;
ok( $rs = $schema->resultset('Rfam')
                 ->find( { rfam_acc => 'RF00066' } ), 'found row for RF00066' );

isa_ok( $rs, 'RfamDB::Rfam', '$rs' );

is( $rs->rfam_id, 'U7', 'id is "U7"' );

