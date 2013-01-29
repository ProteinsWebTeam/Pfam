use strict;
use warnings;
use Test::More tests => 4;
use FindBin;
use Data::Printer;
use File::Slurp;
use File::Temp qw(tempfile tempdir);

BEGIN {
  use_ok( 'Bio::Rfam::FamilyIO' ) || print "Failed to load Bio::Rfam::FamilyIO!\n";
  use_ok( 'Bio::Rfam::Family::Scores' ) || print "Failed to load Bio::Rfam::Family::DESC!\n";
}

my $dir = $FindBin::Bin;
my $test_data= $dir . '/data/RF00014/SCORES';

my $familyIO = Bio::Rfam::FamilyIO->new( );
isa_ok($familyIO, 'Bio::Rfam::FamilyIO');

my $desc = $familyIO->parseScores( $test_data );
isa_ok($desc, 'Bio::Rfam::Family::Scores');
