#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
    $bioperl_dir =
        (defined $ENV{'BIOPERL_DIR'})
            ?$ENV{'BIOPERL_DIR'}:"/pfam/db/bioperl";
}

use lib $rfam_mod_dir;
use lib $bioperl_dir;

use strict;
use RfamQC;

my $family = shift;

if( &RfamQC::valid_sequences( $family ) ) {
    print "$family passes sequence checks\n";
    exit(0);
}    
else {
    print "$family contains errors.  You should rebuild this family.\n";
    exit(1);
}
