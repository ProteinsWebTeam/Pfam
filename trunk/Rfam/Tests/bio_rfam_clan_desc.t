use strict;
use warnings;
use Test::More tests => 7;
use FindBin;
use Data::Printer;
use File::Slurp;
use File::Temp qw(tempfile tempdir);

BEGIN {
  use_ok( 'Bio::Rfam::ClanIO' ) || print "Failed to load Bio::Rfam::ClanIO!\n";
  use_ok( 'Bio::Rfam::Clan::DESC' ) || print "Failed to load Bio::Rfam::Clan::DESC!\n";
}

my $dir = $FindBin::Bin;
my $test_data= $dir . '/data/CL00001/CLANDESC';

my $clanIO = Bio::Rfam::ClanIO->new( );
isa_ok($clanIO, 'Bio::Rfam::ClanIO');

my $desc = $clanIO->parseDESC( $test_data );
isa_ok($desc, 'Bio::Rfam::Clan::DESC');

is($desc->AC, 'CL00001', 'The accesion for the DESC file is CL00001');
is($desc->DE, 'tRNA clan', 'Got correct description.');
my $tdir  = tempdir( CLEANUP => 1 );
$clanIO->writeDESC($desc, $tdir);
my @fileOld = read_file($test_data);
my @fileNew = read_file($tdir.'/CLANDESC');
is_deeply(\@fileOld, \@fileNew, 'Check CALNDESC files are the same');
