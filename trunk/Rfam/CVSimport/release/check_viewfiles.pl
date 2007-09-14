#!/software/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/software/rfam/scripts/Modules";
}

use lib $rfam_mod_dir;

use strict;
use Rfam;
use RfamRCS;

my $db = Rfam::default_db();
foreach my $acc ( $db->get_allacc() ) {
    if( &RfamRCS::view_file_errors( $acc ) ) {
	print "$acc: found errors with viewfiles\n";
    }
}

    
