use strict;
use warnings FATAL => 'all';
use Test::More tests => 2;

BEGIN {
    use_ok( 'Bio::Easel::SSI' ) || print "Bail out!\n";
}

# test new 
my $seqfile = "./t/data/test.fa";
my $ssi = Bio::Easel::SSI->new({
   fileLocation => $seqfile, 
});
isa_ok($ssi, "Bio::Easel::SSI");

