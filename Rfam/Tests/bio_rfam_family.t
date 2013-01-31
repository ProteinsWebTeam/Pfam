use strict;
use warnings;
use Test::More tests => 7;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::Family' )         || print "Failed to load Bio::Rfam::Family!\n";
  use_ok( 'Bio::Rfam::FamilyIO' )       || print "Failed to load Bio::Rfam::FamilyIO!\n";
  use_ok( 'Bio::Rfam::Family::MSA' )    || print "Failed to load Bio::Rfam::Family::MSA\n";
  use_ok( 'Bio::Rfam::Family::DESC' )   || print "Failed to load Bio::Rfam::Family::DESC\n";
  use_ok( 'Bio::Rfam::Family::TBLOUT' ) || print "Failed to load Bio::Rfam::Family::TBLOUT\n"; 
}

my $dir = $FindBin::Bin;
my $test_data = $dir . '/data/RF00014';

my $msa = Bio::Rfam::Family::MSA->new({
      fileLocation => $test_data.'/SEED',
          aliType => 'seed' });




my $familyIO = Bio::Rfam::FamilyIO->new( );
my $desc = $familyIO->parseDESC( $test_data.'/DESC' );
isa_ok($desc, 'Bio::Rfam::Family::DESC');

#my $family = Bio::Rfam::Family->new( { SEED => $msa } );
#isa_ok($family, 'Bio::Rfam::Family');

my $family = Bio::Rfam::Family->new( { SEED => { aliType => 'seed',
                                                 fileLocation => $test_data.'/SEED'},
                                       DESC  => $desc,
                                       TBLOUT => { fileLocation => $test_data.'/TBLOUT'} });
isa_ok($family, 'Bio::Rfam::Family');
