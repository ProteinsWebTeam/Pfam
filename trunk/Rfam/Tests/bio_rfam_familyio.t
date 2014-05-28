use strict;
use warnings;
use Test::More tests => 6;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::FamilyIO' )   || print "Failed to load Bio::Rfam::FamilyIO!\n";
  use_ok( 'Bio::Rfam::Family::CM' ) || print "Failed to load Bio::Rfam::Family::CM!\n";
}

my $dir = $FindBin::Bin;
my $test_data= $dir . '/data';
my $test_family= 'RF00014';

my $familyIO = Bio::Rfam::FamilyIO->new( );
isa_ok($familyIO, 'Bio::Rfam::FamilyIO');

my $family = $familyIO->loadRfamFromLocalFile($test_family, $test_data);
isa_ok($family, 'Bio::Rfam::Family');

# test writeCM
my $outfile = "$test_data/test.cm";
my $cm1 = $family->CM;
$familyIO->writeCM($cm1, $outfile);
# read it in
my $cm2 = $familyIO->parseCM($outfile);

# $cm1->rawcm and $cm->rawcm should be identical (b/c files should be identical)
my $nlines1 = scalar(@{$cm1->{rawcm}});
my $nlines2 = scalar(@{$cm2->{rawcm}});
is($nlines1, $nlines2, "writeCM() output proper number of lines");

my $rawcm1 = join("", @{$cm1->{rawcm}});
my $rawcm2 = join("", @{$cm2->{rawcm}});
is($rawcm1, $rawcm2, "writeCM() output identical CM file, the way it was supposed to");

