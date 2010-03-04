#!/software/bin/perl -w

use strict;
use RfamQC;

my $family = shift;

open( LOG, ">$family/sequences" ) or die;

if( &RfamQC::valid_sequences( $family ) ) {
    print STDERR "$family passes sequence checks\n";
    exit(0);
}    
else {
    print STDERR "$family contains errors.  You should rebuild this family.\n";
    print LOG "$family contains errors.  You should rebuild this family.\n";
    exit(1);
}
