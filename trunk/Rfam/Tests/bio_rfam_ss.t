
use strict;
use warnings;

use Test::More;
use Test::Exception;

BEGIN {
  use_ok( 'Bio::Rfam::SS' );
}

my $ss = Bio::Rfam::SS->new;
isa_ok( $ss, 'Bio::Rfam::SS' );

my $ss_cons = '.[({.<<<<<.<<<......<<<<.......................................................>>>>..<<<<<..<<<....<<...............................................................>>....>>>>>.>>>.......................>>>>>>>>.})].';
my $infernal_ss_string = '.<<<.<<<<<.<<<......<<<<.......................................................>>>>..<<<<<..<<<....<<...............................................................>>....>>>>>.>>>.......................>>>>>>>>.>>>.';

lives_ok { $ss = Bio::Rfam::SS->new( $ss_cons ) } 'can call "new" with scalar arg';
lives_ok { $ss = Bio::Rfam::SS->new( ss_cons => $ss_cons ) } 'can call "new" with hash arg';

is( scalar @{$ss->pairs}, 25, 'parsed correct number of pairs' );
is( $ss->pairs->[0]->left,  24, 'got correct "left" value for first pair' );
is( $ss->pairs->[0]->right, 80, 'got correct "right" value for first pair' );

$ss->parse_ss_cons( $ss_cons );
is( scalar @{$ss->pairs}, 25, 'correct number of pairs found after parsing a second SS cons string' );

# TODO test the knots code

my $ss_string = $ss->get_infernal_string;
is( $infernal_ss_string, $ss_string, 'Infernal SS string is correct' );

my $expected_colour_map = {
  2   => 3, 3   => 3, 4   => 3, 6   => 3, 7   => 3, 8   => 3, 9   => 3, 10  => 3, 12  => 3, 13  => 3,
  14  => 3, 21  => 1, 22  => 1, 23  => 1, 24  => 1, 80  => 1, 81  => 1, 82  => 1, 83  => 1, 86  => 2,
  87  => 2, 88  => 2, 89  => 2, 90  => 2, 93  => 2, 94  => 2, 95  => 2, 100 => 2, 101 => 2, 165 => 2,
  166 => 2, 171 => 2, 172 => 2, 173 => 2, 174 => 2, 175 => 2, 177 => 2, 178 => 2, 179 => 2, 203 => 3,
  204 => 3, 205 => 3, 206 => 3, 207 => 3, 208 => 3, 209 => 3, 210 => 3, 212 => 3, 213 => 3, 214 => 3,
};

is_deeply( $ss->colour_map, $expected_colour_map, 'colour map calculated correctly' );

is( $ss->get_pair_in_column(1), undef, 'got "undef" correctly for empty column' );
is( $ss->get_pair_in_column(2)->right, 214, 'got correct pair using column number' );

done_testing();

__DATA__
        10        20        30        40        50        60        70        80        90       100
123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
.<<<.<<<<<.<<<......<<<<.......................................................>>>>..<<<<<..<<<....<
<...............................................................>>....>>>>>.>>>.....................
..>>>>>>>>.>>>.
