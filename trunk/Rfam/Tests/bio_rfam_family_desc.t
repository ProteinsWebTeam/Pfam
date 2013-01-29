use strict;
use warnings;
use Test::More tests => 5;
use FindBin;
use Data::Printer;
use File::Slurp;
use File::Temp qw(tempfile tempdir);

BEGIN {
  use_ok( 'Bio::Rfam::FamilyIO' ) || print "Failed to load Bio::Rfam::FamilyIO!\n";
  use_ok( 'Bio::Rfam::Family::DESC' ) || print "Failed to load Bio::Rfam::Family::DESC!\n";
}

my $dir = $FindBin::Bin;
my $test_data= $dir . '/data/RF00014/DESC';

my $familyIO = Bio::Rfam::FamilyIO->new( );
isa_ok($familyIO, 'Bio::Rfam::FamilyIO');

my $desc = $familyIO->parseDESC( $test_data );
isa_ok($desc, 'Bio::Rfam::Family::DESC');

is($desc->AC, 'RF00014', 'The accesion for the DESC file is RF00014');
my $tdir  = tempdir( CLEANUP => 0 );
print STDERR $tdir;
$familyIO->writeDESC($desc, $tdir);

my @fileOld = read_file($test_data);
my @fileNew = read_file($tdir.'/DESC');
is_deeply(\@fileOld, \@fileNew, 'Check DESC files are the same');
