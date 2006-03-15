use lib 't';
use strict;
use warnings;

use Test;
use TestUtils;


use Bio::Pfam::Interpro::DB_XML ;


$| = 1;

plan tests => 2;

# 1 - compiles

ok(1);

# 2 - create Interpro XML database object

my $db_xml = Bio::Pfam::Interpro::DB_XML->new() ;

ok( 1 );



