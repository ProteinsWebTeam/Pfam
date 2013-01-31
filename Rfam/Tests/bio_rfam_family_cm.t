use strict;
use warnings;
use Test::More tests => 16;
use FindBin;
use Data::Printer;
use File::Slurp;
use File::Temp qw(tempfile tempdir);

BEGIN {
  use_ok( 'Bio::Rfam::FamilyIO' ) || print "Failed to load Bio::Rfam::FamilyIO!\n";
  use_ok( 'Bio::Rfam::Family::CM' ) || print "Failed to load Bio::Rfam::Family::CM!\n";
}

my $dir = $FindBin::Bin;
my $test_data= $dir . '/data/';


my $familyIO = Bio::Rfam::FamilyIO->new( );
isa_ok($familyIO, 'Bio::Rfam::FamilyIO');

#test.cm.cal    test.cm.justbuild
my $cm = $familyIO->parseCM( $test_data.'/test.cm.justbuild' );
isa_ok($cm, 'Bio::Rfam::Family::CM');
is($cm->is_calibrated, 0, 'Correctly not set calibration');

#Checks on the CM body.
is(defined($cm->cmBody), 1, 'has a cmBody defined');
my $body = $cm->cmBody;
is($body->[0],  "CM\n", 'Body started  with CM');
is($body->[$#$body],  "//\n", 'Body finished with//');
is($cm->match_pair_node, 1, 'Has found MATP line');


#Check on the HMM body.
is(defined($cm->hmmBody), 1, 'has a hmmBody defined');
$body = $cm->hmmBody;
is($body->[0],  "HMM          A        C        G        U   \n", 'Body started  with RNA HMM line');
is($body->[$#$body],  "//\n", 'Body finished with//');


my $cm2 = $familyIO->parseCM( $test_data.'/test.cm.cal' );
isa_ok($cm2, 'Bio::Rfam::Family::CM');
is($cm2->is_calibrated, 1, 'Correctly set calibration');
is($cm2->cmHeader->{clen}, 85, 'Corrected clen');
is($cm2->hmmHeader->{maxl}, 177, 'Corrected maxl');

