#!/usr/local/bin/perl -w

BEGIN {
    $rfam_mod_dir = 
        (defined $ENV{'RFAM_MODULES_DIR'})
            ?$ENV{'RFAM_MODULES_DIR'}:"/pfam/db/Rfam/scripts/Modules";
}

use lib $rfam_mod_dir;
use strict;
use Rfam;

my $db = Rfam::default_db();
foreach my $acc ( $db->get_allacc() ) {
    system "$Rfam::scripts_dir/make/aln2ps.pl $Rfam::current_dir/$acc/SEED > $acc.ps" and die;
    system "pstopnm --portrait --stdout $acc.ps | pnmtojpeg --quality 90 > $acc.jpg" and die;
    system "pstopnm --portrait --stdout --xsize 200 $acc.ps | pnmtojpeg --quality 90 > tn_$acc.jpg" and die;
}
