use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Bio::Easel::Msa' ) || print "Bail out!\n";
}

