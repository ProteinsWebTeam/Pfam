use strict;
use warnings FATAL => 'all';
use Test::More tests => 5;

BEGIN {
    use_ok( 'Bio::Easel::Random' ) || print "Bail out!\n";
}

# test new 
my $rng = Bio::Easel::Random->new({
   seed => 181,
});
isa_ok($rng, "Bio::Easel::Random");

# test roll(), and test it gives expected output (with '181' as seed)
my $roll;
$roll = $rng->roll(6);
is ($roll, 5);

$roll = $rng->roll(6);
is ($roll, 5);

$roll = $rng->roll(6);
is ($roll, 0);

