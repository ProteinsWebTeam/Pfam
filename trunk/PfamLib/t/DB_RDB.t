use lib 't';
use strict;
use warnings;

use Test;
use TestUtils;

use Bio::Pfam::DB_RDB;

$| = 1;

plan tests => 3;

# 1 - compiles

ok(1);

# 2 - get live DB_RDB object

my $db = Bio::Pfam::DB_RDB -> new( '-db_name'   => 'pfamlive',
				   '-db_driver' => 'mysql', 
				   '-db_host'   => '172.18.19.1',
				   '-db_user'   => 'pfamro' );
ok( $db->isa('Bio::Pfam::DB_RDB') );

# 3 - find an id from an accession

ok( $db->acc2id('PF00001'), "7tm_1" );
