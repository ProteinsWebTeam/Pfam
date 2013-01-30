use strict;
use warnings;
use Test::More tests => 4;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::Family' ) || print "Failed to load Bio::Rfam::Family!\n";
  use_ok( 'Bio::Rfam::Family::MSA' ) || print "Failed to load Bio::Rfam::Family::MSA\n";
}

my $dir = $FindBin::Bin;
my $test_file = $dir . '/data/RF00014';

my $msa = Bio::Rfam::Family::MSA->new({
      fileLocation => $test_file,
          aliType => 'seed'
});

my $family = Bio::Rfam::Family->new( { SEED => $msa } );
isa_ok($family, 'Bio::Rfam::Family');

$family = Bio::Rfam::Family->new( { SEED => { aliType => 'seed',
                                              fileLocation => $test_file} } );
isa_ok($family, 'Bio::Rfam::Family');
