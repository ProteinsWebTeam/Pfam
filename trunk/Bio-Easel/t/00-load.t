use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Bio::Easel' ) || print "Bail out!\n";
}

diag( "Testing Bio::Easel $Bio::Easel::VERSION, Perl $], $^X" );
