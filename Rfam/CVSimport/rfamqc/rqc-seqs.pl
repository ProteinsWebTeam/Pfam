#!/usr/local/bin/perl -w

use strict;
use RfamQC;

my $family = shift;

open( LOG, ">$family/sequences" ) or die;

if( &RfamQC::valid_sequences( $family ) ) {
    print "$family passes sequence checks\n";
    exit(0);
}    
else {
    print "$family contains errors.  You should rebuild this family.\n";
    print LOG "$family contains errors.  You should rebuild this family.\n";
    exit(1);
}
