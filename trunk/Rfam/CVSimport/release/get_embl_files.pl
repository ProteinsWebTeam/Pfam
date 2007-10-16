#!/usr/local/bin/perl -w

use strict;

my @globlist = qw( fun
		   hum
		   inv
		   mam
		   mus
		   phg
		   pln
		   pro
		   rod
		   vrl
		   vrt
		   );

my $embl = shift;
die unless( $embl );

my $srsroot = "cbi2:/nfs/disk100/pubseq/srspartition/data/embl$embl";
foreach my $glob ( @globlist ) {
    system "rsync -v '".$srsroot."/".$glob."*.dat' ." and die;
}
