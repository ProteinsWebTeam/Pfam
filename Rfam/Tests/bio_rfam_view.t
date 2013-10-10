use strict;
use warnings;
use Test::More tests => 1;
use FindBin;

BEGIN {
  use_ok( 'Bio::Rfam::View' )         || print "Failed to load Bio::Rfam::QC!\n";
}

my $dir = $FindBin::Bin;
