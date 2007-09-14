#!/software/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/software/rfam/scripts/Modules";
}

use lib $rfam_mod_dir;

use strict;
use Rfam;

my $dir = shift;
if( $dir ) {
	chdir $dir or die "cannot chdir to $dir";
}

my $db = Rfam::default_db();
foreach my $acc ( $db->get_allacc() ) {
    system "rfco $acc" and do {
	warn "failed to checkout $acc";
	next;
    };
}
