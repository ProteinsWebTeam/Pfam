
use strict;
use warnings;

use Test::More;
use Test::Exception;

BEGIN {
  use_ok( 'Bio::Rfam::Pair' );
}

my $pair = Bio::Rfam::Pair->new;
isa_ok( $pair, 'Bio::Rfam::Pair' );

lives_ok { $pair->left(1)     } 'setting left to a valid value works';
dies_ok  { $pair->left("a")   } 'setting left to an invalid value fails as expected';
lives_ok { $pair->right(1)    } 'setting right to a valid value works';
dies_ok  { $pair->right("a")  } 'setting right to an invalid value fails as expected';
lives_ok { $pair->knot('.')   } 'setting knot to a valid value works';
dies_ok  { $pair->knot( \[] ) } 'setting knot to an invalid value fails as expected';

lives_ok { $pair = Bio::Rfam::Pair->new( left => 1, right => 10, knot => '.' ) } 
  'setting left, right and knot in "new" works';

is( $pair->left,  1,   '"left" is set to correct value' );
is( $pair->right, 10,  '"right" is set to correct value' );
is( $pair->knot,  '.', '"knot" is set to correct value' );

$pair = Bio::Rfam::Pair->new( left => 10, right => 1, knot => '.' );
is( $pair->left,  1,   '"left" is set to correct value when left > right' );
is( $pair->right, 10,  '"right" is set to correct value when left > right' );

done_testing();

