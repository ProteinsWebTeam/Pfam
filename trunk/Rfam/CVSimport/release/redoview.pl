#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;

use strict;
use Rfam;

my @accs;

if( @ARGV ) {
    @accs = @ARGV;
}
else {
    my $db = Rfam::default_db();
    @accs = $db->get_allacc();
}

foreach my $acc ( @accs ) {
    print "Doing $acc ....\n";
    system "$Rfam::view_maker -n $acc" and die;
}
