#!perl -T
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'Bio::Pfam::EbiRestClient' ) || print "Bail out!\n";
}

diag( "Testing Bio::Pfam::EbiRestClient $Bio::Pfam::EbiRestClient::VERSION, Perl $], $^X" );
