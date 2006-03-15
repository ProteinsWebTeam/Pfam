use lib 't';
use strict;
use warnings;

use Test;
use TestUtils;

use Bio::Pfam;

$| = 1;

plan tests => 3;

# 1 - compiles

ok(1);

# 2 - get default_db

my $db = Bio::Pfam::default_db();
ok( $db->isa('Bio::Pfam::DB_RCS') );

# 3 - get live_rdb

my $db = Bio::Pfam::live_rdb();
ok( $db->isa('Bio::Pfam::DB_RDB') );

