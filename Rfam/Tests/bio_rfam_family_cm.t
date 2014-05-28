use strict;
use warnings;
use Test::More tests => 34;
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

#Check that we can change NAME, ACC, DESC, GA, TC, NC
my $newname = "TestFamily";
my $newacc  = "RF99989";
my $newdesc = "A test RNA";
my $newGA   = "45";
my $newTC   = "44.3";
my $newNC   = "38.9";
my $newGAprinted = sprintf("%.2f", $newGA);
my $newTCprinted = sprintf("%.2f", $newTC);
my $newNCprinted = sprintf("%.2f", $newNC);
$cm->setName       ($newname);
$cm->setAccession  ($newacc);
$cm->setDescription($newdesc);
$cm->setGA         ($newGA);
$cm->setTC         ($newTC);
$cm->setNC         ($newNC);

# check values are what we set them as
is($cm->{cmHeader}->{name},  $newname, "setName seems to work.");
is($cm->{hmmHeader}->{name}, $newname, "setName seems to work.");
is($cm->{cmHeader}->{acc},   $newacc,  "setAccession seems to work.");
is($cm->{hmmHeader}->{acc},  $newacc,  "setAccession seems to work.");
is($cm->{cmHeader}->{desc},  $newdesc, "setDescription seems to work.");
is($cm->{hmmHeader}->{desc}, $newdesc, "setDescription seems to work.");
is($cm->{cmHeader}->{hitGA}, $newGA,   "setGA seems to work.");
is($cm->{cmHeader}->{hitTC}, $newTC,   "setTC seems to work.");
is($cm->{cmHeader}->{hitNC}, $newNC,   "setNC seems to work.");

# write out the CM to a file, read it back in and make sure it has the
# values we set
my $outfile = "$test_data/test.cm";
$familyIO->writeCM($cm, $outfile);
# read it in
my $cm3 = $familyIO->parseCM($outfile);
unlink $outfile;

# check the values are what we set them as
is($cm3->{cmHeader}->{name},  $newname,      "setName seems to work after output/input cycle.");
is($cm3->{hmmHeader}->{name}, $newname,      "setName seems to work after output/input cycle.");
is($cm3->{cmHeader}->{acc},   $newacc,       "setAccession seems to work after output/input cycle.");
is($cm3->{hmmHeader}->{acc},  $newacc,       "setAccession seems to work after output/input cycle.");
is($cm3->{cmHeader}->{desc},  $newdesc,      "setDescription seems to work after output/input cycle.");
is($cm3->{hmmHeader}->{desc}, $newdesc,      "setDescription seems to work after output/input cycle.");
is($cm3->{cmHeader}->{hitGA}, $newGAprinted, "setGA seems to work after output/input cycle.");
is($cm3->{cmHeader}->{hitTC}, $newTCprinted, "setTC seems to work after output/input cycle.");
is($cm3->{cmHeader}->{hitNC}, $newNCprinted, "setNC seems to work after output/input cycle.");

