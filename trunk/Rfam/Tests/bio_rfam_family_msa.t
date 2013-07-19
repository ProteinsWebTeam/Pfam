use strict;
use warnings;
use Test::More tests => 8;
use FindBin;
use File::Temp qw(tempfile);
use File::Slurp;
BEGIN {
  use_ok( 'Bio::Rfam::Family::MSA' ) || print "Failed to load Bio::Rfam::Family::MSA!\n";
  use_ok( 'Bio::Rfam::Config' )      || print "Failed to load Bio::Rfam::Config!\n";

}

my $dir = $FindBin::Bin;
my $test_file = $dir . '/data/RF00014/SEED';

my $msa = Bio::Rfam::Family::MSA->new({
    fileLocation => $test_file,
    aliType => 'seed'
});

isa_ok($msa, 'Bio::Rfam::Family::MSA');

is($msa->aliType, 'seed', 'Expecting to see seed type');

my $c = Bio::Rfam::Config->new;
my $rfamdb = $c->rfamlive;
$msa->seqToSpeciesNames($rfamdb);

my ($fh, $filename) = tempfile();

$msa->write_msa($filename);
close($fh);

my @fileOld = read_file($filename);
my @fileNew = read_file($test_file.'.species.sto');
is_deeply(\@fileOld, \@fileNew, 'Check SEED species files are the same');


my $msaSp = Bio::Rfam::Family::MSA->new({
    fileLocation => $test_file.'.species.sto',
    aliType => 'seed'
});
is($msaSp->nseq, 5, 'Read in species SEED file');

$msa->revert_to_original;
$msa->seqToBitNSEAndSpecies($rfamdb, 'RF00014', 1);
$msa->write_msa('SEED.bit.nse.sp');


# test nse_create
$msa->revert_to_original;
my $nadded = $msa->nse_createHAA;
is($nadded, 5, "nse_createHAA method returns correct value");

my $testname = "M15749.1/140-200";
my ($oname, $ofract) = $msa->nse_overlap($testname);
is($oname, "M15749.1/155-239", "nse_overlap method returns correct value");

